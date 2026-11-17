# Scoring: compare forecasts with observations once the observations exist.
#
# 1. prepare_scoring() copies the recent forecasts, scores and targets to the
#    scratch store and works out which forecast rows have an observation but no
#    score yet ("score_me", partitioned by variable and model).
# 2. score_group() scores one model/variable at a time (score4cast works in
#    memory) and merges the result into that model's bundled scores file.

# Columns identifying an observation, and a scored forecast row.
obs_key_cols <- c("project_id", "site_id", "datetime", "duration", "variable")
score_key_cols <- c(obs_key_cols, "model_id", "family", "reference_datetime")
# Columns identifying one forecast row in the bundled scores (adds pub_datetime,
# so a resubmission's scores replace only its own rows).
score_row_cols <- c(score_key_cols, "pub_datetime")

#' Where scoring keeps its working files
#'
#' @inheritParams config_get
#' @param ... Folder within the scratch bucket.
#' @return An `s3://` URI, or a local folder in a dry run.
#' @noRd
scratch_uri <- function(cfg, ...) output_uri(bucket_uri(cfg, "scratch", ...))

#' Drop forecast rows beyond each variable's maximum horizon
#'
#' Works on a duckdb table. Variables not in the config are kept, as they have
#' no targets and so are never scored anyway.
#'
#' @param forecasts Lazy duckdb table of forecasts.
#' @inheritParams config_get
#' @return The filtered lazy table.
#' @noRd
filter_max_horizon <- function(forecasts, cfg) {
  max_horizons <- target_variables(cfg) |>
    dplyr::summarise(max_horizon = max(.data$max_horizon),
                     .by = c("variable", "duration"))
  forecasts |>
    dplyr::left_join(max_horizons, by = c("variable", "duration"), copy = TRUE) |>
    dplyr::mutate(horizon = date_diff("day", as.POSIXct(reference_datetime),
                                      as.POSIXct(datetime))) |>
    dplyr::filter(is.na(.data$max_horizon) | .data$horizon <= .data$max_horizon) |>
    dplyr::select(-"horizon", -"max_horizon")
}

#' Find the forecast rows that need scoring
#'
#' Copies recent forecasts, scores and targets to the scratch store, then writes
#' `score_me`: forecast rows with an observation but no score, partitioned by
#' variable and model.
#'
#' @inheritParams config_get
#' @param since Only forecasts and observations after this date are considered.
#' @param conn duckdb connection.
#' @return A tibble of the groups to score, one row per project, duration,
#'   variable, model and family.
#' @export
prepare_scoring <- function(cfg, since = Sys.Date() - lubridate::dmonths(6),
                            conn = duckdbfs::cached_connection()) {
  log_step("Clearing scratch space")
  for (folder in c("score_me", "forecasts", "scores", "targets")) {
    clear_scratch(cfg, folder)
  }

  # Forecasts and scores are public; scratch needs its own credentials.
  duckdb_connect(cfg, "main", anonymous = TRUE, conn = conn)
  if (!is_dry_run()) duckdb_connect(cfg, "scratch", conn = conn)
  project <- cfg$project_id

  targets <- duckdbfs::open_dataset(
    unique(stats::na.omit(target_variables(cfg)$targets_file)),
    format = "csv", recursive = FALSE, conn = conn,
    parser_options = list(nullstr = "NA")
  ) |>
    dplyr::filter(.data$project_id == project, .data$datetime > since,
                  !is.na(.data$observation))

  # Forecasts for dates not yet observed cannot be scored.
  last_observed <- targets |>
    dplyr::summarise(last = max(.data$datetime, na.rm = TRUE)) |>
    dplyr::pull(.data$last)

  forecasts <- duckdbfs::open_dataset(
    bucket_uri(cfg, "forecasts_bucket", "bundled-parquet"), conn = conn
  ) |>
    dplyr::filter(.data$project_id == project, .data$datetime > since,
                  .data$datetime <= last_observed, !is.na(.data$model_id),
                  !is.na(.data$parameter), !is.na(.data$prediction)) |>
    # Older bundles say "ensemble"; without this they would be rescored forever.
    dplyr::mutate(family = ifelse(.data$family == "ensemble", "sample", .data$family)) |>
    filter_max_horizon(cfg)

  scores <- duckdbfs::open_dataset(
    bucket_uri(cfg, "scores_bucket", "bundled-parquet"), conn = conn
  ) |>
    dplyr::filter(.data$project_id == project, .data$datetime > since,
                  !is.na(.data$observation))

  # Copying the subsets to scratch first, then joining the copies, is much
  # faster and lighter on memory than joining the remote datasets directly.
  log_step("Copying forecasts, scores and targets to scratch")
  copy <- function(data, folder) {
    data |>
      dplyr::group_by(.data$variable) |>
      duckdbfs::write_dataset(scratch_uri(cfg, folder), conn = conn)
    duckdbfs::open_dataset(scratch_uri(cfg, folder, "**"), conn = conn)
  }
  forecasts <- copy(forecasts, "forecasts")
  scores <- copy(dplyr::select(scores, dplyr::all_of(score_key_cols)), "scores")
  targets <- copy(targets, "targets")

  log_step("Finding unscored forecasts with observations")
  forecasts |>
    dplyr::anti_join(scores, by = score_key_cols) |>
    dplyr::inner_join(targets, by = obs_key_cols) |>
    dplyr::group_by(.data$variable, .data$model_id) |>
    duckdbfs::write_dataset(scratch_uri(cfg, "score_me"), conn = conn)

  duckdbfs::open_dataset(scratch_uri(cfg, "score_me", "**"), conn = conn) |>
    dplyr::distinct(.data$project_id, .data$duration, .data$variable,
                    .data$model_id, .data$family) |>
    dplyr::collect()
}

# Delete one scratch folder. Scratch is temporary, so a missing folder is fine.
clear_scratch <- function(cfg, folder) {
  if (is_dry_run()) {
    unlink(scratch_uri(cfg, folder), recursive = TRUE)
    return(invisible())
  }
  mc_connect(cfg, "scratch")
  path <- bucket_uri(cfg, "scratch", folder, scheme = "mc")
  if (nrow(remote_files(path)) > 0) minioclient::mc_rm(path, recursive = TRUE)
  invisible()
}

#' Score forecast rows that have their observation joined
#'
#' A thin wrapper around `score4cast::crps_logs_score()`, which takes forecasts
#' and observations separately.
#'
#' @param joined Forecast rows with an `observation` column.
#' @return One row per forecast row: identifying columns, `observation`, `crps`,
#'   `logs`, and the forecast's `mean`, `median`, `sd` and 80% and 95%
#'   quantiles.
#' @export
score_rows <- function(joined) {
  rlang::check_installed("score4cast")
  observations <- joined |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(obs_key_cols, "observation"))))
  joined |>
    dplyr::select(-"observation") |>
    score4cast::crps_logs_score(observations, extra_groups = c("project_id", "duration"))
}

#' Merge new scores into existing ones
#'
#' Rows in `existing` for the same forecast rows as `new` are replaced, not
#' kept beside them. Forecast rows are matched on their identifying columns
#' (missing values match each other), not on score values, so a row rescored
#' against a revised observation replaces its old score.
#'
#' @param new Tibble of new scores.
#' @param existing Path of the existing scores (any path duckdb can read), or
#'   `NULL`.
#' @param conn duckdb connection.
#' @return A lazy duckdb table of the merged scores, without duplicate rows.
#' @export
merge_scores <- function(new, existing, conn = duckdbfs::cached_connection()) {
  new <- duckdbfs::as_dataset(new, conn = conn)
  if (is.null(existing)) return(dplyr::distinct(new))
  kept <- duckdbfs::open_dataset(existing, conn = conn) |>
    dplyr::anti_join(new, by = score_row_cols, na_matches = "na") |>
    dplyr::compute()
  dplyr::distinct(dplyr::union_all(kept, new))
}

#' Score one group and update its bundled scores
#'
#' @param group One row of [prepare_scoring()]'s result.
#' @inheritParams config_get
#' @return The number of rows scored, invisibly.
#' @export
score_group <- function(group, cfg) {
  # A separate connection per group, closed afterwards, keeps memory from
  # building up across hundreds of groups.
  conn <- local_duckdb()
  if (!is_dry_run()) duckdb_connect(cfg, "scratch", conn = conn)

  # Filtering on the partition columns lets duckdb read only this partition.
  joined <- duckdbfs::open_dataset(scratch_uri(cfg, "score_me", "**"), conn = conn) |>
    dplyr::filter(.data$variable == !!group$variable,
                  .data$model_id == !!group$model_id,
                  .data$project_id == !!group$project_id,
                  .data$duration == !!group$duration,
                  .data$family == !!group$family) |>
    dplyr::collect()
  new_scores <- score_rows(joined)

  partition <- c("bundled-parquet",
                 paste0("project_id=", group$project_id),
                 paste0("duration=", group$duration),
                 paste0("variable=", group$variable),
                 paste0("model_id=", group$model_id))
  existing <- remote_files(bucket_uri(cfg, "scores_bucket", partition, scheme = "mc"))
  duckdb_connect(cfg, "main", anonymous = !has_credentials(cfg, "main"), conn = conn)
  merged <- merge_scores(
    new_scores,
    if (nrow(existing) > 0) bucket_uri(cfg, "scores_bucket", partition) else NULL,
    conn = conn
  )

  out_dir <- tempfile("scores")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  merged |>
    dplyr::group_by(.data$project_id, .data$duration, .data$variable, .data$model_id) |>
    duckdbfs::write_dataset(out_dir, conn = conn)
  upload_dir(out_dir, bucket_uri(cfg, "scores_bucket", "bundled-parquet", scheme = "mc"))
  invisible(nrow(new_scores))
}

#' Score all new forecasts
#'
#' The `score` stage: [prepare_scoring()], then [score_group()] for each group.
#' A group that fails is logged and the rest carry on. Its rows stay unscored,
#' so they are picked up next run, and the run fails at the end.
#'
#' @inheritParams prepare_scoring
#' @return A tibble of groups with `n_scored` and `error` columns, invisibly.
#' @export
run_scoring <- function(cfg, since = Sys.Date() - lubridate::dmonths(6)) {
  log_step("Scoring")
  mc_connect(cfg, "main", anonymous = !has_credentials(cfg, "main"))
  groups <- prepare_scoring(cfg, since = since)
  log_step("{nrow(groups)} group{?s} to score")

  outcomes <- purrr::map(seq_len(nrow(groups)), function(i) {
    group <- groups[i, ]
    log_step("[{i}/{nrow(groups)}] {group$variable} {group$model_id} ({group$family})")
    tryCatch(
      list(n = score_group(group, cfg), error = NA_character_),
      error = function(e) {
        cli::cli_inform(c("x" = "Failed: {conditionMessage(e)}"))
        list(n = NA_integer_, error = conditionMessage(e))
      }
    )
  })
  groups$n_scored <- purrr::map_int(outcomes, "n")
  groups$error <- purrr::map_chr(outcomes, "error")

  failed <- groups[!is.na(groups$error), ]
  if (nrow(failed) > 0) {
    cli::cli_abort(c(
      "{nrow(failed)} group{?s} failed to score and will be retried next run:",
      rlang::set_names(paste(failed$variable, failed$model_id, failed$family), "x")
    ))
  }
  log_step("Scored {sum(groups$n_scored)} forecast row{?s}")
  invisible(groups)
}
