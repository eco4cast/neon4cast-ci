# Processing new submissions: download from the submissions bucket, check,
# standardise, and write to the forecasts bucket as parquet plus summaries.
#
# Since 2026-09-08 the NRP submissions key has had ListBucket permission only:
# GetObject and DeleteObject return "Insufficient permissions". The bucket's
# own policy grants GetObject-by-ACL and DeleteObject to everyone, so an
# anonymous client can read and delete but cannot list. We therefore list with
# the authenticated alias and read and delete through an anonymous one. Because
# deleting may fail, we also keep our own record (the "manifest") of every
# submission handled, on the main store, and skip those keys. Drop the
# anonymous alias once NRP restores the key's permissions.

# minio client aliases used for the submissions bucket.
submissions_alias <- "submissions"
submissions_public_alias <- "submissions_public"

#' Read the record of processed submissions
#'
#' Only a record that does not exist yet starts a new, empty one. Any other
#' error (access, network) stops the run, because carrying on with an empty
#' record would overwrite the real one and reprocess every submission.
#'
#' @inheritParams config_get
#' @return Character vector of processed submission keys.
#' @export
read_manifest <- function(cfg) {
  remote <- bucket_uri(cfg, "processed_submissions", scheme = "mc")
  if (length(minioclient::mc_ls(remote)) == 0) {
    cli::cli_inform("No processed-submissions record found; starting a new one.")
    return(character(0))
  }
  local <- tempfile(fileext = ".csv")
  on.exit(unlink(local))
  minioclient::mc_cp(remote, local)
  manifest <- readr::read_csv(local, show_col_types = FALSE, progress = FALSE)
  if (!"key" %in% names(manifest)) {
    cli::cli_abort("Processed-submissions record {.path {remote}} has no {.field key} column.")
  }
  as.character(manifest$key)
}

#' @rdname read_manifest
#' @param keys All processed keys, to write as the new record.
#' @export
write_manifest <- function(cfg, keys) {
  local <- tempfile(fileext = ".csv")
  on.exit(unlink(local))
  readr::write_csv(tibble::tibble(key = keys), local)
  upload_file(local, bucket_uri(cfg, "processed_submissions", scheme = "mc"))
}

#' Decide which submissions to download
#'
#' @param keys Every key in the submissions bucket.
#' @param processed Keys already in the processed-submissions record.
#' @param exclude_patterns Regular expressions. Keys matching any of them are
#'   never downloaded or recorded (`submissions$exclude_patterns` in the
#'   config).
#' @return A list with `download` (new keys), `stale` (processed keys still in
#'   the bucket, to delete) and `n_excluded`.
#' @export
#' @examples
#' plan_submissions(c("a.csv", "b.csv", "old-2023.csv"), processed = "a.csv",
#'                  exclude_patterns = "2023")
plan_submissions <- function(keys, processed, exclude_patterns = character()) {
  excluded <- rep(FALSE, length(keys))
  for (pattern in exclude_patterns) excluded <- excluded | grepl(pattern, keys)
  wanted <- keys[!excluded]
  list(
    download = setdiff(wanted, processed),
    stale = intersect(keys, processed),
    n_excluded = sum(excluded)
  )
}

#' Process one downloaded submission
#'
#' Checks the file name, then reads, validates and standardises the forecast.
#' For a valid forecast, writes parquet and summaries under `out_dir` (in
#' `parquet/` and `summaries/`) ready to upload.
#'
#' @param path Local path of the downloaded file.
#' @inheritParams config_get
#' @param out_dir Local folder for the output.
#' @param pub_datetime Publication time stamped on the forecast.
#' @return A list with `status` and `reason`. `status` is `"processed"`, or
#'   `"invalid"` (failed validation) or `"skipped"` (a file name, type or date
#'   that is not processed). Errors are raised, not returned, so a failure can
#'   be retried on the next run.
#' @export
process_submission <- function(path, cfg, out_dir, pub_datetime = Sys.time()) {
  info <- parse_submission_filename(path)
  min_date <- config_get(cfg, "submissions", "min_reference_date", default = NA)

  if (is.na(info$model_id)) {
    return(list(status = "skipped",
                reason = "name is not <theme>-<YYYY-MM-DD>-<model_id>.<ext>"))
  }
  if (!info$extension %in% c("csv", "csv.gz")) {
    return(list(status = "skipped",
                reason = paste0(".", info$extension, " files are not accepted")))
  }
  if (!is.na(min_date) && as.Date(info$reference_datetime) < as.Date(min_date)) {
    return(list(status = "skipped", reason = paste("reference date before", min_date)))
  }

  fc <- read_submission(path)
  problems <- validate_forecast(fc, cfg)
  for (i in seq_len(nrow(problems))) {
    cli::cli_inform(c("!" = "{problems$severity[i]}: {problems$message[i]}"))
  }
  if (any(problems$severity == "error")) {
    return(list(status = "invalid",
                reason = paste(problems$message[problems$severity == "error"],
                               collapse = "; ")))
  }

  fc <- standardize_forecast(fc, cfg, info, pub_datetime = pub_datetime)
  file_id <- paste0("sub_", unname(tools::md5sum(path)))
  write_forecast_parquet(fc, file.path(out_dir, "parquet"), file_id)
  write_forecast_parquet(summarize_submission(fc), file.path(out_dir, "summaries"), file_id)
  list(status = "processed", reason = paste(nrow(fc), "rows"))
}

#' Process all new submissions
#'
#' The `process_submissions` stage. For each new file in the submissions
#' bucket:
#'
#' 1. download it (a file that cannot be read is skipped and retried next run);
#' 2. [process_submission()] it, and upload any parquet and summaries to the
#'    forecasts and summaries buckets;
#' 3. archive the raw file in `<forecasts_bucket>/raw/`, valid or not;
#' 4. add it to the processed-submissions record, then delete it from the
#'    submissions bucket (best effort).
#'
#' A submission that errors is logged and left out of the record, so it is
#' retried on the next run. The run carries on with the others and fails at the
#' end if any errored.
#'
#' @inheritParams config_get
#' @return A tibble with one row per downloaded submission: `key`, `status`
#'   (`"processed"`, `"invalid"`, `"skipped"` or `"failed"`) and `reason`,
#'   invisibly.
#' @export
run_submissions <- function(cfg) {
  log_step("Processing submissions")
  mc_connect(cfg, "main")
  mc_connect(cfg, "submissions", alias = submissions_alias)
  mc_connect(cfg, "submissions", alias = submissions_public_alias, anonymous = TRUE)

  bucket <- config_get(cfg, "submissions_bucket")
  public_key <- function(key) paste0(submissions_public_alias, "/", bucket, "/", key)
  remove_submission <- function(key) {
    tryCatch(
      {
        remove_remote(public_key(key))
        TRUE
      },
      error = function(e) {
        cli::cli_warn(
          "Could not delete {.path {key}} from the submissions bucket: {conditionMessage(e)}"
        )
        FALSE
      }
    )
  }

  processed <- read_manifest(cfg)
  keys <- remote_files(paste0(submissions_alias, "/", bucket))$key
  plan <- plan_submissions(
    keys, processed,
    exclude_patterns = unlist(config_get(cfg, "submissions", "exclude_patterns",
                                         default = character()))
  )
  log_step(
    "{length(keys)} object{?s} in the bucket: {plan$n_excluded} excluded, ",
    "{length(plan$stale)} already processed, {length(plan$download)} new."
  )

  # Processed submissions still in the bucket because an earlier delete failed.
  n_removed <- sum(vapply(plan$stale, remove_submission, logical(1)))

  work_dir <- tempfile("submissions")
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  time_stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  results <- vector("list", length(plan$download))

  for (i in seq_along(plan$download)) {
    key <- plan$download[i]
    log_step("{key}")
    local <- file.path(work_dir, "raw", key)
    dir.create(dirname(local), recursive = TRUE, showWarnings = FALSE)

    downloaded <- tryCatch(
      {
        minioclient::mc_cp(public_key(key), shQuote(local))
        TRUE
      },
      error = function(e) {
        cli::cli_warn("Could not download {.path {key}}: {conditionMessage(e)}")
        FALSE
      }
    )
    if (!downloaded) {
      results[[i]] <- tibble::tibble(key = key, status = "unreadable",
                                     reason = "download failed")
      next
    }

    out_dir <- file.path(work_dir, "out", i)
    result <- tryCatch(
      {
        outcome <- process_submission(local, cfg, out_dir)
        if (outcome$status == "processed") {
          upload_dir(file.path(out_dir, "parquet"),
                     bucket_uri(cfg, "forecasts_bucket", "parquet", scheme = "mc"))
          upload_dir(file.path(out_dir, "summaries"),
                     bucket_uri(cfg, "summaries_bucket", "summaries", scheme = "mc"))
        }
        upload_file(local, bucket_uri(cfg, "forecasts_bucket", "raw",
                                      paste0("T", time_stamp, "_", basename(key)),
                                      scheme = "mc"))
        # Record first, delete second: if the delete succeeded but the record
        # was never written, the submission would be lost rather than retried.
        processed <- unique(c(processed, key))
        write_manifest(cfg, processed)
        n_removed <- n_removed + remove_submission(key)
        outcome
      },
      error = function(e) list(status = "failed", reason = conditionMessage(e))
    )
    if (result$status != "processed") {
      cli::cli_inform(c("!" = "{result$status}: {result$reason}"))
    }
    results[[i]] <- tibble::tibble(key = key, status = result$status,
                                   reason = result$reason)
    unlink(c(local, out_dir), recursive = TRUE)
  }

  results <- purrr::list_rbind(results)
  if (is.null(results) || nrow(results) == 0) {
    results <- tibble::tibble(key = character(), status = character(),
                              reason = character())
  }
  counts <- table(factor(results$status,
                         c("processed", "invalid", "skipped", "unreadable", "failed")))
  log_step(
    "Done: {counts[['processed']]} processed, {counts[['invalid']]} invalid, ",
    "{counts[['skipped']]} skipped, {counts[['unreadable']]} unreadable, ",
    "{counts[['failed']]} failed; {n_removed} deleted from the bucket."
  )

  failed <- results$key[results$status == "failed"]
  if (length(failed) > 0) {
    cli::cli_abort(c(
      "{length(failed)} submission{?s} failed and will be retried next run:",
      rlang::set_names(failed, "x")
    ))
  }
  invisible(results)
}
