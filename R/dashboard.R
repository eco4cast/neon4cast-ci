# Data for the dashboard (the Quarto site in dashboard/).
#
# stage_dashboard_data() runs before `quarto render`: it does every S3 query
# and writes small local files, so the pages only read local data and plot it.
# One query per duration partition and dataset; nothing reads the raw
# forecasts.
#
#   forecasts.parquet    latest forecast summaries at the plot sites
#   analysis.parquet     scores of an older forecast, now with observations
#   leaderboard.parquet  mean CRPS by model, reference date and horizon
#   stats.json           headline numbers for the home page
#   model_types.csv      registered models per modelling approach
#   variables.csv        the challenge's variables, names and descriptions
#   sites.csv            sites, locations and the groups they belong to

#' Dashboard settings
#'
#' Reads the `dashboard` block of the config and fills in defaults:
#' * `n_plot_sites` (2): sites per target group shown in forecast plots, taken
#'   from the start of each group's `focal_sites`.
#' * `leaderboard_max_horizon` (35): longest horizon, in days, in the
#'   leaderboards.
#' * `excluded_models`: model ids left out of the plots, per variable, plus
#'   `all` for every variable.
#'
#' @inheritParams config_get
#' @return A list.
#' @export
dashboard_settings <- function(cfg) {
  defaults <- list(n_plot_sites = 2, leaderboard_max_horizon = 35, excluded_models = list())
  utils::modifyList(defaults, cfg$dashboard %||% list())
}

#' How far back the dashboard looks for a duration
#'
#' Weekly and longer variables are observed slowly, so their leaderboards
#' and forecast analysis use the last year; shorter ones use the last 30 days.
#'
#' @param duration ISO 8601 duration, e.g. `"P1D"`.
#' @return Number of days.
#' @export
#' @examples
#' dashboard_lookback_days(c("P1D", "P1W"))
dashboard_lookback_days <- function(duration) {
  seconds <- vapply(duration, function(d) as.numeric(lubridate::duration(d)), numeric(1))
  unname(ifelse(seconds >= 7 * 86400, 365, 30))
}

#' Sites shown in each target group's forecast plots
#'
#' @inheritParams config_get
#' @return A tibble with `group`, `variable`, `duration` and `site_id`: the
#'   first `n_plot_sites` of the group's `focal_sites`.
#' @export
plot_sites <- function(cfg) {
  n <- dashboard_settings(cfg)$n_plot_sites
  sites <- purrr::imap(cfg$target_groups, function(settings, group) {
    focal <- as.character(unlist(settings$focal_sites))
    tibble::tibble(group = group, site_id = utils::head(focal, n))
  }) |>
    purrr::list_rbind()
  dplyr::inner_join(target_variables(cfg)[c("group", "variable", "duration")], sites,
                    by = "group", relationship = "many-to-many")
}

#' Stage the data the dashboard plots
#'
#' @inheritParams config_get
#' @param out_dir Folder to write to.
#' @param now Time the dashboard is built; the windows are relative to it.
#' @return `out_dir`, invisibly.
#' @export
stage_dashboard_data <- function(cfg, out_dir = file.path("dashboard", "data"),
                                 now = Sys.time()) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  duckdb_connect(cfg, "main", anonymous = TRUE)
  conn <- duckdbfs::cached_connection()
  settings <- dashboard_settings(cfg)
  sites <- plot_sites(cfg)
  durations <- unique(target_variables(cfg)$duration)
  project <- paste0("project_id=", cfg$project_id)

  per_duration <- purrr::map(durations, function(duration) {
    log_step("Dashboard: {duration}")
    lookback <- dashboard_lookback_days(duration)
    partition <- paste0("duration=", duration)
    summaries <- bucket_uri(cfg, "summaries_bucket", "bundled-summaries", project, partition) |>
      duckdbfs::open_dataset(conn = conn)
    scores <- bucket_uri(cfg, "scores_bucket", "bundled-parquet", project, partition) |>
      duckdbfs::open_dataset(conn = conn)
    duration_sites <- dplyr::filter(sites, .data$duration == !!duration)
    list(
      forecasts = latest_forecasts(summaries, duration_sites, now) |>
        dplyr::mutate(duration = duration),
      analysis = scored_forecasts(scores, duration_sites, now, lookback) |>
        dplyr::mutate(duration = duration),
      leaderboard = leaderboard_scores(scores, now, lookback,
                                       settings$leaderboard_max_horizon) |>
        dplyr::mutate(duration = duration)
    )
  })
  for (part in c("forecasts", "analysis", "leaderboard")) {
    rows <- purrr::list_rbind(purrr::map(per_duration, part))
    arrow::write_parquet(rows, file.path(out_dir, paste0(part, ".parquet")))
    log_step("Dashboard: {nrow(rows)} rows in {part}.parquet")
  }

  jsonlite::write_json(dashboard_stats(cfg, conn, now), file.path(out_dir, "stats.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  read_registered_models(cfg) |>
    dplyr::mutate(model_type = model_type_label(.data$model_type)) |>
    dplyr::count(.data$model_type, name = "n_models") |>
    readr::write_csv(file.path(out_dir, "model_types.csv"))
  readr::write_csv(catalog_variables(cfg), file.path(out_dir, "variables.csv"))
  readr::write_csv(dashboard_sites(cfg), file.path(out_dir, "sites.csv"))
  write_quarto_metadata(cfg, file.path(dirname(out_dir), "_challenge.yml"))
  invisible(out_dir)
}

summary_columns <- c("model_id", "site_id", "variable", "reference_datetime", "datetime",
                     "mean", "quantile02.5", "quantile97.5")

# The most recent forecast of each variable made at least two days ago (so
# most teams have submitted), at the plot sites.
latest_forecasts <- function(summaries, sites, now) {
  cutoff <- now - lubridate::days(2)
  refs <- summaries |>
    dplyr::filter(.data$reference_datetime < !!cutoff,
                  .data$variable %in% !!unique(sites$variable)) |>
    dplyr::summarise(reference_datetime = max(.data$reference_datetime, na.rm = TRUE),
                     .by = "variable")
  summaries |>
    dplyr::filter(.data$site_id %in% !!unique(sites$site_id),
                  .data$datetime > .data$reference_datetime) |>
    dplyr::inner_join(refs, by = c("variable", "reference_datetime")) |>
    dplyr::select(dplyr::all_of(summary_columns)) |>
    dplyr::collect() |>
    dplyr::semi_join(sites, by = c("variable", "site_id"))
}

# For each variable, the latest forecast made at least `lookback` days ago
# that has observations, with them, at the plot sites.
scored_forecasts <- function(scores, sites, now, lookback) {
  newest <- now - lubridate::days(lookback)
  oldest <- now - lubridate::days(2 * lookback)
  refs <- scores |>
    dplyr::filter(.data$reference_datetime <= !!newest, .data$reference_datetime > !!oldest,
                  !is.na(.data$observation),
                  .data$variable %in% !!unique(sites$variable),
                  .data$site_id %in% !!unique(sites$site_id)) |>
    dplyr::summarise(reference_datetime = max(.data$reference_datetime, na.rm = TRUE),
                     .by = "variable")
  scores |>
    dplyr::filter(.data$site_id %in% !!unique(sites$site_id)) |>
    dplyr::inner_join(refs, by = c("variable", "reference_datetime")) |>
    dplyr::select(dplyr::all_of(c(summary_columns, "observation"))) |>
    dplyr::collect() |>
    dplyr::semi_join(sites, by = c("variable", "site_id"))
}

# Mean CRPS over all sites, by model, reference date and horizon (days), for
# forecasts made in the last `lookback` days. `n` lets the pages average these
# means again without bias.
leaderboard_scores <- function(scores, now, lookback, max_horizon) {
  since <- now - lubridate::days(lookback)
  scores |>
    dplyr::filter(.data$reference_datetime >= !!since, !is.na(.data$crps)) |>
    dplyr::mutate(horizon = date_diff("day", as.POSIXct(reference_datetime),
                                      as.POSIXct(datetime))) |>
    dplyr::filter(.data$horizon > 0, .data$horizon <= !!max_horizon) |>
    dplyr::summarise(crps = mean(.data$crps, na.rm = TRUE), n = dplyr::n(),
                     .by = c("variable", "model_id", "reference_datetime", "horizon")) |>
    dplyr::collect() |>
    dplyr::mutate(horizon = as.numeric(.data$horizon), n = as.numeric(.data$n))
}

# Headline numbers: forecasts and models submitted, and the targets' span.
dashboard_stats <- function(cfg, conn, now) {
  uri <- bucket_uri(cfg, "summaries_bucket", "bundled-summaries",
                    paste0("project_id=", cfg$project_id))
  counts <- duckdbfs::open_dataset(uri, conn = conn) |>
    dplyr::distinct(.data$model_id, .data$variable, .data$duration, .data$reference_datetime) |>
    dplyr::summarise(n_forecasts = dplyr::n(), n_models = dplyr::n_distinct(.data$model_id)) |>
    dplyr::collect()
  files <- unique(stats::na.omit(target_variables(cfg)$targets_file))
  targets <- readr::read_csv(files, col_select = c("datetime", "variable"),
                             show_col_types = FALSE, progress = FALSE)
  list(
    n_forecasts = as.numeric(counts$n_forecasts),
    n_models = as.numeric(counts$n_models),
    n_variables = dplyr::n_distinct(targets$variable),
    targets_start = format(as.Date(min(targets$datetime, na.rm = TRUE))),
    targets_end = format(as.Date(max(targets$datetime, na.rm = TRUE))),
    updated = format(as.Date(now))
  )
}

# Groups free-text model types: "empirical (a statistical model)" and
# "Empirical (a statistical model)" are the same answer.
model_type_label <- function(x) {
  x <- trimws(sub("\\s*\\(.*\\)$", "", x))
  x <- ifelse(is.na(x) | x == "", "Not described", x)
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

# Every site, with a TRUE/FALSE column per target group saying whether the
# group forecasts there.
dashboard_sites <- function(cfg) {
  sites <- read_sites(cfg)
  membership <- purrr::imap(cfg$target_groups, function(settings, group) {
    column <- settings$sites_column %||% tolower(group)
    if (column %in% names(sites)) sites[[column]] %in% c(1, TRUE) else rep(NA, nrow(sites))
  })
  dplyr::bind_cols(sites[c("site_id", "site_name", "latitude", "longitude")],
                   tibble::as_tibble(membership))
}

#' Link to a page of the challenge's catalog in the STAC browser
#'
#' @inheritParams config_get
#' @param path Path of a JSON file in the catalog repository, e.g.
#'   `"summaries/collection.json"`.
#' @return A URL.
#' @export
stac_browser_url <- function(cfg, path = "catalog.json") {
  repo <- config_get(cfg, "catalog_config", "catalog_repo", default = cfg$github_repo)
  paste0("https://radiantearth.github.io/stac-browser/#/external/raw.githubusercontent.com/",
         repo, "/main/", path)
}

# Site-wide Quarto settings that come from the config. _quarto.yml lists this
# file under metadata-files, so Quarto merges it into the project settings.
write_quarto_metadata <- function(cfg, path) {
  repo <- paste0("https://github.com/", config_get(cfg, "github_repo"))
  title <- dashboard_settings(cfg)$title %||% cfg$challenge_long_name
  yaml::write_yaml(list(website = list(
    title = title,
    `site-url` = cfg$challenge_url,
    `repo-url` = repo,
    `page-footer` = list(left = "CC-BY", right = list(list(icon = "github", href = repo)))
  )), path)
}
