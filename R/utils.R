#' Dry-run mode
#'
#' With the environment variable `DRY_RUN=true`, write helpers in this package
#' write under a local folder instead of to S3. Everything else (downloads,
#' reads, computation) runs as normal, so a dry run is a full test of a stage
#' that cannot change stored data.
#'
#' * `is_dry_run()` is `TRUE` when `DRY_RUN` is `true`, `1` or `yes`, in any
#'   case.
#' * `dry_run_dir()` is the local folder writes go to: `DRY_RUN_DIR` when set,
#'   otherwise `dry-run` in the session's temporary directory.
#' * `output_uri()` returns a destination unchanged normally. In a dry run it
#'   returns the matching path under `dry_run_dir()`, with any `s3://` scheme
#'   removed and the rest of the path kept.
#'
#' @param uri Destination: an `s3://` URI or a minio client path
#'   (`alias/bucket/...`).
#' @return `is_dry_run()`: logical scalar. `dry_run_dir()`, `output_uri()`:
#'   character scalar.
#' @export
#' @examples
#' withr::with_envvar(c(DRY_RUN = "true", DRY_RUN_DIR = "out"),
#'   output_uri("s3://bucket/forecasts/x.parquet")
#' )
is_dry_run <- function() {
  tolower(Sys.getenv("DRY_RUN")) %in% c("true", "1", "yes")
}

#' @rdname is_dry_run
#' @export
dry_run_dir <- function() {
  Sys.getenv("DRY_RUN_DIR", file.path(tempdir(), "dry-run"))
}

#' @rdname is_dry_run
#' @export
output_uri <- function(uri) {
  if (!is_dry_run()) return(uri)
  file.path(dry_run_dir(), sub("^[a-z0-9]+://", "", uri))
}

#' Upload a local file, or copy it locally in a dry run
#'
#' Uploads with the minio client. In a dry run (see [is_dry_run()]) the file is
#' copied to [output_uri()] instead.
#'
#' @param from Local file.
#' @param to Minio client destination (`alias/bucket/...`), e.g. from
#'   [bucket_uri()] with `scheme = "mc"`.
#' @return The destination actually written, invisibly.
#' @export
upload_file <- function(from, to) {
  if (!file.exists(from)) cli::cli_abort("{.path {from}} does not exist.")
  dest <- output_uri(to)
  if (is_dry_run()) {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(from, dest, overwrite = TRUE)
    cli::cli_inform("DRY RUN: wrote {.path {dest}} instead of {.path {to}}.")
  } else {
    # minioclient splits its command on spaces, so quote the local path.
    minioclient::mc_cp(shQuote(from), to)
  }
  invisible(dest)
}

#' Log a step with a timestamp
#'
#' @param ... Message parts, pasted together. Supports cli inline markup and
#'   glue interpolation in the caller's environment.
#' @param .envir Environment for interpolation.
#' @return `NULL`, invisibly.
#' @export
#' @examples
#' n <- 3
#' log_step("Scoring {n} groups")
log_step <- function(..., .envir = parent.frame()) {
  cli::cli_inform(
    c(">" = paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)),
    .envir = .envir
  )
  invisible(NULL)
}
