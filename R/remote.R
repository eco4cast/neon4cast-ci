# Reading listings from, and writing to, S3 through the minio client (mc).
#
# Every write to S3 goes through upload_file(), upload_dir(), move_remote() or
# remove_remote(), so that DRY_RUN=true (see is_dry_run()) can redirect or skip
# it in one place. Uploads use mc rather than duckdb or arrow: duckdb >= 2.0
# cannot complete S3 multipart uploads to OSN, and arrow's uploads fail there
# with XAmzContentSHA256Mismatch.

#' List the files under a minio client path
#'
#' Folder markers are dropped. A prefix that does not exist gives an empty
#' result. Access or network errors are raised, never read as "no files": a
#' caller that took them for "nothing there yet" could overwrite data.
#'
#' @param path Minio client path (`alias/bucket/prefix`).
#' @return A tibble with columns `path` (full minio client path), `key`
#'   (relative to `path`) and `bytes`. One row per file.
#' @export
remote_files <- function(path) {
  listing <- minioclient::mc_ls(path, recursive = TRUE, details = TRUE)
  if (!is.data.frame(listing) || nrow(listing) == 0) {
    return(tibble::tibble(path = character(), key = character(), bytes = numeric()))
  }
  listing |>
    dplyr::filter(!.data$is_folder) |>
    dplyr::select("path", "key", "bytes") |>
    tibble::as_tibble()
}

#' Upload a local file or folder, or copy it locally in a dry run
#'
#' `upload_file()` copies one file with `mc cp`. `upload_dir()` copies a whole
#' folder with `mc mirror`, replacing remote files of the same name and leaving
#' other remote files alone. In a dry run (see [is_dry_run()]) both copy to
#' [output_uri()] instead.
#'
#' @param from Local file or folder.
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

#' @rdname upload_file
#' @export
upload_dir <- function(from, to) {
  if (!dir.exists(from)) cli::cli_abort("{.path {from}} is not a folder.")
  dest <- output_uri(to)
  if (is_dry_run()) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(from, full.names = TRUE), dest, recursive = TRUE,
              overwrite = TRUE)
    cli::cli_inform("DRY RUN: wrote {.path {dest}} instead of {.path {to}}.")
  } else {
    minioclient::mc_mirror(shQuote(from), to, overwrite = TRUE)
  }
  invisible(dest)
}

#' Move or delete remote objects, or only log it in a dry run
#'
#' @param from,to Minio client paths of the object and its new location.
#' @param path Minio client path to delete.
#' @param recursive Delete everything under `path`.
#' @return `TRUE`, invisibly.
#' @export
move_remote <- function(from, to) {
  if (is_dry_run()) {
    cli::cli_inform("DRY RUN: would move {.path {from}} to {.path {to}}.")
  } else {
    minioclient::mc_mv(from, to)
  }
  invisible(TRUE)
}

#' @rdname move_remote
#' @export
remove_remote <- function(path, recursive = FALSE) {
  if (is_dry_run()) {
    cli::cli_inform("DRY RUN: would delete {.path {path}}.")
  } else {
    minioclient::mc_rm(path, recursive = recursive)
  }
  invisible(TRUE)
}

#' Convert a minio client path to an `s3://` URI
#'
#' @param path Minio client path(s), `alias/bucket/...`.
#' @param alias The alias at the start of `path`.
#' @return Character vector of `s3://bucket/...` URIs.
#' @export
#' @examples
#' mc_to_s3("main/bucket/forecasts/a.parquet")
mc_to_s3 <- function(path, alias = "main") {
  prefix <- paste0(alias, "/")
  bad <- !startsWith(path, prefix)
  if (any(bad)) {
    cli::cli_abort("{.path {path[bad][1]}} does not start with alias {.val {alias}}.")
  }
  paste0("s3://", substring(path, nchar(prefix) + 1))
}
