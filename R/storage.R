# Connecting to the challenge's S3 stores and building paths inside them.
#
# A "store" is one S3 endpoint plus the credentials for it:
#   main         forecasts, scores, targets, archive, metadata (config `endpoint`)
#   submissions  where participants upload forecasts (`submissions_endpoint`)
#   noaa         NOAA driver forecasts (`noaa_endpoint`; main credentials)
#   scratch      temporary files used while scoring (`scratch$endpoint`)
# Credentials come from environment variables whose names are set in the
# config's `credentials` block (see config_defaults()).

store_names <- c("main", "submissions", "noaa", "scratch")

#' Endpoint and credentials for a store
#'
#' @inheritParams config_get
#' @param store One of `"main"`, `"submissions"`, `"noaa"` or `"scratch"`.
#' @param anonymous If `TRUE`, return empty credentials for public read-only
#'   access instead of reading environment variables.
#' @return A list with `endpoint`, `key` and `secret`.
#' @export
store_credentials <- function(cfg, store = "main", anonymous = FALSE) {
  store <- rlang::arg_match(store, store_names)
  endpoint <- switch(store,
    main = config_get(cfg, "endpoint"),
    submissions = config_get(cfg, "submissions_endpoint"),
    noaa = config_get(cfg, "noaa_endpoint", default = cfg$endpoint),
    scratch = config_get(cfg, "scratch", "endpoint")
  )
  if (anonymous) return(list(endpoint = endpoint, key = "", secret = ""))

  # NOAA drivers live in the main store unless the config says otherwise.
  env <- cfg$credentials[[store]] %||% cfg$credentials$main
  key <- Sys.getenv(env$key_env)
  secret <- Sys.getenv(env$secret_env)
  if (!nzchar(key) || !nzchar(secret)) {
    cli::cli_abort(c(
      "No credentials for the {.val {store}} store.",
      "i" = paste("Set {.envvar {env$key_env}} and {.envvar {env$secret_env}},",
                  "or use {.code anonymous = TRUE} for public reads.")
    ))
  }
  list(endpoint = endpoint, key = key, secret = secret)
}

#' Connect to a store
#'
#' `mc_connect()` registers a minio client alias, which `mc_cp()`,
#' `mc_ls()` and similar functions then use as a path prefix.
#' `duckdb_connect()` registers a duckdb secret so that `duckdbfs` can read and
#' write `s3://` paths in the store. `s3_connect()` does both.
#'
#' Each duckdb secret is scoped to the store's bucket, so one connection can
#' hold secrets for several stores (for example main and scratch while
#' scoring). duckdbfs names a secret after its key, so two anonymous
#' connections on the same duckdb connection replace each other.
#'
#' @inheritParams store_credentials
#' @param alias Name for the minio client alias. Defaults to the store name.
#' @param conn duckdb connection. Defaults to duckdbfs's cached connection.
#' @return `mc_connect()` and `s3_connect()`: the alias, invisibly.
#'   `duckdb_connect()`: `conn`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' cfg <- read_config()
#' s3_connect(cfg, "main")
#' minioclient::mc_ls(bucket_uri(cfg, "forecasts_bucket", scheme = "mc"))
#' }
s3_connect <- function(cfg, store = "main", alias = store, anonymous = FALSE,
                       conn = duckdbfs::cached_connection()) {
  mc_connect(cfg, store, alias = alias, anonymous = anonymous)
  duckdb_connect(cfg, store, anonymous = anonymous, conn = conn)
  invisible(alias)
}

#' @rdname s3_connect
#' @export
mc_connect <- function(cfg, store = "main", alias = store, anonymous = FALSE) {
  creds <- store_credentials(cfg, store, anonymous = anonymous)
  minioclient::mc_alias_set(alias, creds$endpoint, creds$key, creds$secret)
  invisible(alias)
}

#' @rdname s3_connect
#' @export
duckdb_connect <- function(cfg, store = "main", anonymous = FALSE,
                           conn = duckdbfs::cached_connection()) {
  creds <- store_credentials(cfg, store, anonymous = anonymous)
  duckdbfs::duckdb_secrets(
    key = creds$key,
    secret = creds$secret,
    endpoint = creds$endpoint,
    bucket = store_bucket(cfg, store),
    url_compatibility_mode = FALSE,
    conn = conn
  )
  invisible(conn)
}

#' Build a path inside one of the challenge's buckets
#'
#' Bucket settings in the config (`forecasts_bucket`, `scores_bucket`, ...)
#' hold a bucket name followed by a prefix, e.g.
#' `bio230014-bucket01/challenges/forecasts`. `bucket_uri()` adds further path
#' parts and formats the result for the tool that will use it.
#'
#' @inheritParams config_get
#' @param key Name of the config setting holding the bucket path, such as
#'   `"forecasts_bucket"`. Use `"scratch"` for the scoring scratch bucket.
#' @param ... Path parts to add, e.g. `"bundled-parquet"`, `"project_id=x"`.
#' @param scheme `"s3"` for duckdb and arrow (`s3://bucket/...`), `"mc"` for
#'   the minio client (`alias/bucket/...`), or `"https"` for public URLs.
#' @param alias The minio client alias, for `scheme = "mc"`. Defaults to the
#'   bucket's store name, which is the alias [mc_connect()] sets by default.
#' @return A character scalar.
#' @export
#' @examples
#' cfg <- list(endpoint = "sdsc.osn.xsede.org",
#'             forecasts_bucket = "bucket01/challenges/forecasts")
#' bucket_uri(cfg, "forecasts_bucket", "bundled-parquet")
#' bucket_uri(cfg, "forecasts_bucket", "bundled-parquet", scheme = "mc")
#' bucket_uri(cfg, "forecasts_bucket", scheme = "https")
bucket_uri <- function(cfg, key, ..., scheme = c("s3", "mc", "https"),
                       alias = NULL) {
  scheme <- rlang::arg_match(scheme)
  path <- paste(c(bucket_path(cfg, key), ...), collapse = "/")
  path <- gsub("/+", "/", gsub("^/|/$", "", path))
  store <- bucket_key_store(key)
  switch(scheme,
    s3 = paste0("s3://", path),
    mc = paste0(alias %||% store, "/", path),
    https = paste0("https://", store_credentials(cfg, store, anonymous = TRUE)$endpoint,
                   "/", path)
  )
}

#' An arrow S3 file system rooted in one of the challenge's buckets
#'
#' Use this for reading with arrow (e.g. `arrow::open_dataset()`). Upload with
#' [upload_file()] instead, because arrow's S3 uploads fail on OSN with
#' `XAmzContentSHA256Mismatch`.
#'
#' @inheritParams bucket_uri
#' @inheritParams store_credentials
#' @return An arrow `SubTreeFileSystem`.
#' @export
arrow_bucket <- function(cfg, key, ..., anonymous = FALSE) {
  store <- bucket_key_store(key)
  creds <- store_credentials(cfg, store, anonymous = anonymous)
  path <- sub("^s3://", "", bucket_uri(cfg, key, ..., scheme = "s3"))
  if (anonymous) {
    arrow::s3_bucket(path, endpoint_override = creds$endpoint, anonymous = TRUE)
  } else {
    arrow::s3_bucket(path, endpoint_override = creds$endpoint,
                     access_key = creds$key, secret_key = creds$secret)
  }
}

#' Run an expression again if it fails
#'
#' For flaky network calls. Waits `wait` seconds after the first failure,
#' `2 * wait` after the second, and so on.
#'
#' @param expr Expression to evaluate.
#' @param times Maximum number of attempts.
#' @param wait Seconds to wait after the first failure.
#' @return The value of `expr`. If every attempt fails, the last error is
#'   re-raised.
#' @export
#' @examples
#' with_retry(sqrt(4), times = 2, wait = 0)
with_retry <- function(expr, times = 3, wait = 10) {
  expr <- substitute(expr)
  env <- parent.frame()
  for (attempt in seq_len(times)) {
    result <- tryCatch(eval(expr, env), error = identity)
    if (!inherits(result, "error")) return(result)
    if (attempt < times) {
      cli::cli_warn(c(
        "Attempt {attempt} of {times} failed; retrying in {wait * attempt}s.",
        "x" = conditionMessage(result)
      ))
      Sys.sleep(wait * attempt)
    }
  }
  cli::cli_abort("Failed after {times} attempt{?s}.", parent = result)
}

# The bucket path held by a config setting.
bucket_path <- function(cfg, key) {
  path <- if (identical(key, "scratch")) {
    config_get(cfg, "scratch", "bucket")
  } else {
    config_get(cfg, key)
  }
  if (!rlang::is_string(path)) {
    cli::cli_abort("Setting {.field {key}} must be a single string.")
  }
  path
}

# The store each bucket setting belongs to.
bucket_key_store <- function(key) {
  switch(key,
    submissions_bucket = "submissions",
    noaa_forecast_bucket = "noaa",
    scratch = "scratch",
    "main"
  )
}

# The bucket name (first path segment) a store's duckdb secret is scoped to.
store_bucket <- function(cfg, store) {
  key <- switch(store,
    main = "forecasts_bucket",
    submissions = "submissions_bucket",
    noaa = "noaa_forecast_bucket",
    scratch = "scratch"
  )
  path <- tryCatch(bucket_path(cfg, key), error = function(e) NULL)
  if (is.null(path)) return(NULL)
  strsplit(path, "/", fixed = TRUE)[[1]][1]
}
