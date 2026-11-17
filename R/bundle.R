# Bundling: fold newly processed forecasts (or summaries) into one parquet file
# per model, so readers open a few large files instead of thousands of small
# ones.
#
#   <bucket>/parquet/project_id=../duration=../variable=../model_id=../reference_date=../*.parquet
#     -> appended to <bucket>/bundled-parquet/<same model path>/data_0.parquet
#     -> then moved to <bucket>/archive-parquet/<same path>
# Summaries use summaries/, bundled-summaries/ and archive-summaries/.

#' Folder names used by each kind of bundle
#'
#' @param kind `"forecasts"` or `"summaries"`.
#' @inheritParams config_get
#' @return A list with `bucket` (config key), `new`, `bundled` and `archive`
#'   (folder names within the bucket).
#' @export
bundle_layout <- function(kind = c("forecasts", "summaries")) {
  kind <- rlang::arg_match(kind)
  switch(kind,
    forecasts = list(bucket = "forecasts_bucket", new = "parquet",
                     bundled = "bundled-parquet", archive = "archive-parquet"),
    summaries = list(bucket = "summaries_bucket", new = "summaries",
                     bundled = "bundled-summaries", archive = "archive-summaries")
  )
}

#' Group new files by the model folder they belong to
#'
#' @param files Paths of new parquet files.
#' @return A named list: one element per model folder (the path up to and
#'   including `model_id=...`), holding that model's files.
#' @export
#' @examples
#' files_by_model(c(
#'   "main/b/parquet/model_id=a/reference_date=2024-01-01/x.parquet",
#'   "main/b/parquet/model_id=a/reference_date=2024-01-02/x.parquet"
#' ))
files_by_model <- function(files) {
  split(files, sub("/reference_date=[^/]+/.*$", "", files))
}

#' Append new forecast rows to a model's bundle
#'
#' Reads the new files and, when there is one, the existing bundle, and writes
#' the combined rows to `out_file`. The old bundle's rows are kept as they are.
#' New rows are skipped when the bundle already holds their
#' `reference_datetime` and `pub_datetime`, i.e. that submission was bundled by
#' an earlier run that failed before archiving it. Rows are appended with
#' `union_all()`, not `union()`, because de-duplicating a whole bundle runs out
#' of memory on the largest ones.
#'
#' Works on any paths duckdb can read, local or `s3://`.
#'
#' @param new Paths of the new parquet files.
#' @param old Path of the existing bundle file, or `NULL` if there is none.
#' @param out_file Local parquet file to write.
#' @param drop_incomplete Drop new rows with no `model_id`, `parameter` or
#'   `prediction` (forecasts only; summaries have no `prediction`).
#' @param conn duckdb connection.
#' @return `out_file`, invisibly.
#' @export
merge_bundle <- function(new, old, out_file, drop_incomplete = TRUE,
                         conn = duckdbfs::cached_connection()) {
  # Copy the new rows locally first, so the anti-join and the union read them
  # from disk rather than from S3 twice.
  new_local <- tempfile(fileext = ".parquet")
  on.exit(unlink(new_local), add = TRUE)
  # recursive = FALSE: given a single remote path, duckdbfs otherwise treats it
  # as a folder and appends "/**".
  new_rows <- duckdbfs::open_dataset(new, conn = conn, recursive = FALSE)
  if (drop_incomplete) {
    new_rows <- new_rows |>
      dplyr::filter(!is.na(.data$model_id), !is.na(.data$parameter),
                    !is.na(.data$prediction))
  }
  duckdbfs::write_dataset(new_rows, new_local, conn = conn)
  combined <- duckdbfs::open_dataset(new_local, conn = conn, recursive = FALSE)

  if (!is.null(old)) {
    old_rows <- duckdbfs::open_dataset(old, conn = conn, recursive = FALSE)
    already_bundled <- old_rows |>
      dplyr::distinct(.data$reference_datetime, .data$pub_datetime)
    combined <- combined |>
      dplyr::anti_join(already_bundled, by = c("reference_datetime", "pub_datetime"),
                       na_matches = "na")
    combined <- dplyr::union_all(old_rows, combined)
  }

  duckdbfs::write_dataset(combined, out_file, conn = conn,
                          options = list("PER_THREAD_OUTPUT false"))
  invisible(out_file)
}

#' Bundle one model's new files
#'
#' Reads exactly the files in `files`, appends them to the model's bundle
#' ([merge_bundle()]), uploads the bundle, and then moves those same files to
#' the archive. Only the files that were read are archived, so a submission
#' processed while bundling runs waits for the next run instead of being
#' archived unbundled.
#'
#' @param files Minio client paths of the model's new files.
#' @inheritParams config_get
#' @inheritParams bundle_layout
#' @param alias Minio client alias of the main store.
#' @return The number of new files bundled, invisibly.
#' @export
bundle_model <- function(files, cfg, kind = "forecasts", alias = "main") {
  layout <- bundle_layout(kind)
  base <- bucket_uri(cfg, layout$bucket, scheme = "mc", alias = alias)
  new_prefix <- paste0(base, "/", layout$new, "/")
  swap_folder <- function(path, folder) {
    paste0(base, "/", folder, "/", substring(path, nchar(new_prefix) + 1))
  }

  model_dir <- names(files_by_model(files))
  if (length(model_dir) != 1) cli::cli_abort("{.arg files} must belong to one model.")
  bundle_dir <- swap_folder(model_dir, layout$bundled)

  existing <- remote_files(bundle_dir)$path
  if (length(existing) > 1) {
    cli::cli_abort("Expected one bundle file in {.path {bundle_dir}}, found {length(existing)}.")
  }

  conn <- local_duckdb()
  duckdb_connect(cfg, "main", anonymous = is_dry_run() && !has_credentials(cfg, "main"),
                 conn = conn)

  out_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(out_file), add = TRUE)
  merge_bundle(
    new = mc_to_s3(files, alias),
    old = if (length(existing) == 1) mc_to_s3(existing, alias) else NULL,
    out_file = out_file,
    drop_incomplete = kind == "forecasts",
    conn = conn
  )
  bundle_file <- if (length(existing) == 1) existing else paste0(bundle_dir, "/data_0.parquet")
  upload_file(out_file, bundle_file)

  for (file in files) move_remote(file, swap_folder(file, layout$archive))
  invisible(length(files))
}

#' Bundle all new forecasts or summaries
#'
#' The `bundle` stage. Each model is bundled on its own. A model that fails
#' keeps its new files where they are, so they are retried next run, and the
#' run fails at the end. With `VERBOSE=true`, the rows in all bundles are
#' counted before and after (a full scan of the bucket).
#'
#' @inheritParams config_get
#' @inheritParams bundle_layout
#' @return A tibble with one row per model: `model` and `error` (`NA` when it
#'   succeeded), invisibly.
#' @export
run_bundling <- function(cfg, kind = c("forecasts", "summaries")) {
  kind <- rlang::arg_match(kind)
  layout <- bundle_layout(kind)
  log_step("Bundling {kind}")
  mc_connect(cfg, "main", anonymous = is_dry_run() && !has_credentials(cfg, "main"))

  count_bundled <- function() {
    if (!is_verbose()) return(invisible())
    duckdb_connect(cfg, "main", anonymous = TRUE)
    n <- duckdbfs::open_dataset(bucket_uri(cfg, layout$bucket, layout$bundled, "**")) |>
      dplyr::count() |>
      dplyr::pull(.data$n)
    log_step("{format(n, big.mark = ',')} rows in {layout$bundled}")
  }
  count_bundled()

  new_files <- remote_files(bucket_uri(cfg, layout$bucket, layout$new,
                                       paste0("project_id=", cfg$project_id),
                                       scheme = "mc"))$path
  models <- files_by_model(new_files)
  log_step("{length(new_files)} new file{?s} from {length(models)} model{?s}")

  errors <- purrr::imap_chr(models, function(files, model) {
    log_step("{model}")
    tryCatch(
      {
        bundle_model(files, cfg, kind)
        NA_character_
      },
      error = function(e) {
        cli::cli_inform(c("x" = "Failed: {conditionMessage(e)}"))
        conditionMessage(e)
      }
    )
  })
  results <- tibble::tibble(model = names(models), error = unname(errors))
  count_bundled()

  failed <- results$model[!is.na(results$error)]
  if (length(failed) > 0) {
    cli::cli_abort(c(
      "{length(failed)} model{?s} failed to bundle and will be retried next run:",
      rlang::set_names(failed, "x")
    ))
  }
  invisible(results)
}
