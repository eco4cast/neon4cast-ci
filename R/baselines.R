# Baseline (null) models: simple reference forecasts every submitted model is
# compared against. Each target group lists its baselines in the config:
#
#   target_groups:
#     Aquatics:
#       baselines:
#         - {model_id: climatology, method: climatology, schedule: daily}
#         - {model_id: persistenceRW, method: persistence_rw, schedule: daily,
#            args: {transform: {chla: log, oxygen: log}}}
#
# `method` picks one of the baseline_*() functions below (see
# baseline_methods). `args` are passed to it, apart from two that
# run_baselines() handles itself: `variables` (default: all of the group's
# variables) and `horizon` (in days; default: the variables' max_horizon).
#
# Every method takes targets, a reference date and a horizon, and returns rows
# of site_id, variable, datetime, family, parameter and prediction.
# run_baselines() adds the remaining standard columns and submits the forecast
# through the submissions bucket, exactly as a participant would.

#' Reference date for a forecast made today
#'
#' Daily forecasts are issued today. Weekly forecasts are issued on the Monday
#' that starts the current ISO week, the date weekly targets are stamped with.
#'
#' @param duration ISO 8601 duration of the forecast, `"P1D"` or `"P1W"`.
#' @param today The date the forecast is made.
#' @return A Date.
#' @export
#' @examples
#' reference_date_for("P1W", as.Date("2026-11-19"))  # a Thursday
reference_date_for <- function(duration, today = Sys.Date()) {
  switch(duration,
    P1D = as.Date(today),
    P1W = lubridate::floor_date(as.Date(today), "week", week_start = 1),
    cli::cli_abort("Baselines support durations P1D and P1W, not {.val {duration}}.")
  )
}

#' Dates a forecast covers
#'
#' @param reference_date Reference date (from [reference_date_for()]).
#' @param horizon Horizon in days.
#' @param duration `"P1D"` (every day) or `"P1W"` (every 7 days).
#' @return A vector of Dates after `reference_date`, up to `horizon` days
#'   ahead.
#' @noRd
forecast_dates <- function(reference_date, horizon, duration) {
  step <- if (duration == "P1W") 7 else 1
  reference_date + seq(step, horizon, by = step)
}

#' Day-of-year climatology (daily, normal distribution)
#'
#' For each site, variable and forecast day: the mean and standard deviation of
#' all past observations on that day of the year. Days with no past
#' observations are filled by linear interpolation (constant beyond the ends).
#' The standard deviation is the median over the forecast window, so it is
#' smooth. A series needs at least two days with data in the window.
#'
#' @param targets Targets: `site_id`, `variable`, `datetime`, `observation`.
#' @param reference_date Reference date. The forecast starts the day after.
#' @param horizon Days to forecast.
#' @param exclude_current_year Only use years before the reference date's
#'   year, so the climatology is not partly made of this year's observations.
#' @return Forecast rows with family `normal`, parameters `mu` and `sigma`.
#' @export
baseline_climatology <- function(targets, reference_date, horizon,
                                 exclude_current_year = FALSE) {
  history <- dplyr::filter(targets, !is.na(.data$observation))
  if (exclude_current_year) {
    history <- dplyr::filter(history, lubridate::year(.data$datetime) <
                               lubridate::year(reference_date))
  }
  climatology <- history |>
    dplyr::mutate(doy = lubridate::yday(.data$datetime)) |>
    dplyr::summarise(mean = mean(.data$observation), sd = stats::sd(.data$observation),
                     .by = c("site_id", "variable", "doy"))

  dates <- forecast_dates(reference_date, horizon, "P1D")
  forecast <- climatology |>
    dplyr::distinct(.data$site_id, .data$variable) |>
    dplyr::cross_join(tibble::tibble(datetime = dates, doy = lubridate::yday(dates))) |>
    dplyr::left_join(climatology, by = c("site_id", "variable", "doy")) |>
    dplyr::arrange(.data$site_id, .data$variable, .data$datetime) |>
    dplyr::filter(sum(!is.na(.data$mean)) >= 2, .by = c("site_id", "variable")) |>
    dplyr::mutate(mu = fill_linear(.data$mean),
                  sigma = stats::median(.data$sd, na.rm = TRUE),
                  .by = c("site_id", "variable")) |>
    dplyr::filter(!is.na(.data$sigma))

  dplyr::bind_rows(
    dplyr::mutate(forecast, parameter = "mu", prediction = .data$mu),
    dplyr::mutate(forecast, parameter = "sigma", prediction = .data$sigma)
  ) |>
    dplyr::mutate(family = "normal") |>
    dplyr::select("site_id", "variable", "datetime", "family", "parameter", "prediction")
}

#' ISO-week climatology (weekly, log-normal ensemble)
#'
#' For each site and variable: the mean and standard deviation of past
#' observations in each ISO week, with weeks that have no data filled by
#' linear interpolation. Missing or zero standard deviations get the site's
#' mean standard deviation. Each forecast week is then a `n_members`
#' ensemble drawn from a log-normal distribution with that mean (plus
#' `shift`) and standard deviation, which keeps the forecast above zero.
#'
#' `shift` is added to the mean so that a mean of zero still gives a valid
#' log-normal, and is not subtracted afterwards. Predictions are therefore
#' about `shift` higher than the climatological mean. This matches the
#' original ticks model.
#'
#' @inheritParams baseline_climatology
#' @param n_members Ensemble members.
#' @param shift Added to the mean before the log-normal is fitted.
#' @return Forecast rows with family `ensemble`, one parameter per member.
#' @export
baseline_weekly_climatology <- function(targets, reference_date, horizon,
                                        n_members = 500, shift = 1) {
  weekly <- targets |>
    dplyr::filter(!is.na(.data$observation)) |>
    dplyr::mutate(week = lubridate::isoweek(.data$datetime)) |>
    dplyr::summarise(mean = mean(.data$observation), sd = stats::sd(.data$observation),
                     .by = c("site_id", "variable", "week"))

  filled <- weekly |>
    dplyr::distinct(.data$site_id, .data$variable) |>
    dplyr::cross_join(tibble::tibble(week = 1:53)) |>
    dplyr::left_join(weekly, by = c("site_id", "variable", "week")) |>
    dplyr::arrange(.data$site_id, .data$variable, .data$week) |>
    dplyr::mutate(
      mean = fill_linear(.data$mean),
      sd = dplyr::coalesce(.data$sd, mean(.data$sd, na.rm = TRUE)),
      sd = dplyr::if_else(.data$sd == 0, mean(.data$sd), .data$sd),
      .by = c("site_id", "variable")
    )

  dates <- forecast_dates(reference_date, horizon, "P1W")
  forecast <- filled |>
    dplyr::inner_join(tibble::tibble(datetime = dates, week = lubridate::isoweek(dates)),
                      by = "week", relationship = "many-to-many") |>
    dplyr::filter(!is.na(.data$mean), !is.na(.data$sd))

  # Log-normal parameters from the (shifted) mean and standard deviation.
  mu <- forecast$mean + shift
  sdlog <- sqrt(log(1 + forecast$sd^2 / mu^2))
  meanlog <- log(mu) - sdlog^2 / 2
  forecast[rep(seq_len(nrow(forecast)), each = n_members), ] |>
    dplyr::mutate(
      family = "ensemble",
      parameter = as.character(rep(seq_len(n_members), times = nrow(forecast))),
      prediction = stats::rlnorm(dplyr::n(), rep(meanlog, each = n_members),
                                 rep(sdlog, each = n_members))
    ) |>
    dplyr::select("site_id", "variable", "datetime", "family", "parameter", "prediction")
}

#' Persistence (random walk) forecast
#'
#' Fits a random walk to each site and variable with `fable::RW()` and
#' simulates `n_members` bootstrapped paths. Each path starts at the series'
#' last observation, so a site that has not reported recently starts further
#' back, with wider uncertainty by the reference date. Only days after the
#' reference date are kept.
#'
#' @inheritParams baseline_climatology
#' @param transform Named list of transformations by variable: `"log"`,
#'   `"log1p"` or `"sqrt"`. Unlisted variables are modelled untransformed.
#' @param log_floor Before a log transformation, observations at or below zero
#'   are set to this value.
#' @param n_members Bootstrapped paths.
#' @return Forecast rows with family `ensemble`, one parameter per path.
#' @export
baseline_persistence_rw <- function(targets, reference_date, horizon,
                                    transform = list(), log_floor = 1e-5,
                                    n_members = 200) {
  rlang::check_installed(c("fable", "fabletools", "tsibble"))
  end_date <- reference_date + horizon
  data <- dplyr::filter(targets, !is.na(.data$observation))
  data$how <- vapply(data$variable, function(v) transform[[v]] %||% "none", character(1))
  data <- data |>
    dplyr::mutate(
      datetime = as.Date(.data$datetime),
      observation = dplyr::if_else(.data$how == "log" & .data$observation <= 0,
                                   log_floor, .data$observation)
    ) |>
    # A random walk needs at least two observations.
    dplyr::filter(dplyr::n() >= 2, .by = c("site_id", "variable"))

  # Series that need the same transformation share one model fit.
  purrr::map(split(data, data$how), function(series) {
    ts <- series |>
      dplyr::select("site_id", "variable", "datetime", "observation") |>
      tsibble::as_tsibble(key = c("site_id", "variable"), index = "datetime") |>
      tsibble::fill_gaps()
    fit <- switch(series$how[1],
      none = fabletools::model(ts, RW = fable::RW(observation)),
      log = fabletools::model(ts, RW = fable::RW(log(observation))),
      log1p = fabletools::model(ts, RW = fable::RW(log1p(observation))),
      sqrt = fabletools::model(ts, RW = fable::RW(sqrt(observation))),
      cli::cli_abort("Unknown transform {.val {series$how[1]}}.")
    )
    # Each series is simulated from its last observation to the end date.
    future <- ts |>
      tibble::as_tibble() |>
      dplyr::summarise(last = max(.data$datetime), .by = c("site_id", "variable")) |>
      dplyr::filter(.data$last < end_date) |>
      dplyr::reframe(datetime = seq(.data$last + 1, end_date, by = "day"),
                     .by = c("site_id", "variable")) |>
      tsibble::as_tsibble(key = c("site_id", "variable"), index = "datetime")
    fabletools::generate(fit, new_data = future, bootstrap = TRUE, times = n_members) |>
      tibble::as_tibble()
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$datetime > reference_date) |>
    dplyr::transmute(.data$site_id, .data$variable,
                     datetime = .data$datetime, family = "ensemble",
                     parameter = as.character(.data$.rep), prediction = .data$.sim)
}

#' Historical mean forecast (weekly, bootstrapped)
#'
#' Fits the mean of `log(observation + 1)` for each site and variable with
#' `fable::MEAN()` and simulates `n_members` bootstrapped values for each week
#' after the reference date.
#'
#' @inheritParams baseline_climatology
#' @param n_members Bootstrapped values per week.
#' @return Forecast rows with family `ensemble`, one parameter per member.
#' @export
baseline_mean <- function(targets, reference_date, horizon, n_members = 500) {
  rlang::check_installed(c("fable", "fabletools", "tsibble"))
  data <- targets |>
    dplyr::filter(!is.na(.data$observation)) |>
    dplyr::transmute(.data$site_id, .data$variable,
                     datetime = as.Date(.data$datetime), .data$observation)
  # An empty row at the reference date makes every series end there, so the
  # simulated weeks start the week after it.
  ends <- data |>
    dplyr::distinct(.data$site_id, .data$variable) |>
    dplyr::mutate(datetime = reference_date, observation = NA_real_)
  ts <- dplyr::bind_rows(dplyr::filter(data, .data$datetime < reference_date), ends) |>
    tsibble::as_tsibble(key = c("site_id", "variable"), index = "datetime")

  ts |>
    fabletools::model(mean = fable::MEAN(log(observation + 1))) |>
    # A time span, not a number of steps: fable converts it with each series'
    # own interval, as the original model's h = "1 year" did.
    fabletools::generate(h = paste(horizon, "days"), bootstrap = TRUE, times = n_members) |>
    tibble::as_tibble() |>
    dplyr::transmute(.data$site_id, .data$variable,
                     datetime = .data$datetime, family = "ensemble",
                     parameter = as.character(.data$.rep), prediction = .data$.sim)
}

#' The baseline methods a config can name
#'
#' Maps each `method` in a group's `baselines` list to its function.
#'
#' @return A named list of functions.
#' @export
baseline_methods <- function() {
  list(
    climatology = baseline_climatology,
    weekly_climatology = baseline_weekly_climatology,
    persistence_rw = baseline_persistence_rw,
    mean = baseline_mean
  )
}

#' Run one configured baseline
#'
#' @param baseline One entry of a group's `baselines` list.
#' @param targets The group's targets.
#' @param vars The group's rows of [target_variables()].
#' @inheritParams config_get
#' @param today The date the forecast is made.
#' @return The forecast in the standard submission format.
#' @export
run_baseline <- function(baseline, targets, vars, cfg, today = Sys.Date()) {
  method <- baseline_methods()[[baseline$method]]
  if (is.null(method)) {
    cli::cli_abort(c(
      "Unknown baseline method {.val {baseline$method}}.",
      "i" = "Methods: {.val {names(baseline_methods())}}."
    ))
  }
  args <- baseline$args %||% list()
  variables <- unlist(args$variables) %||% vars$variable
  vars <- dplyr::filter(vars, .data$variable %in% variables)
  duration <- unique(vars$duration)
  if (length(duration) != 1) {
    cli::cli_abort("The variables of baseline {.val {baseline$model_id}} must share one duration.")
  }
  horizon <- args$horizon %||% min(vars$max_horizon)
  reference_date <- reference_date_for(duration, today)

  extra <- args[setdiff(names(args), c("variables", "horizon"))]
  forecast <- do.call(method, c(list(
    targets = dplyr::filter(targets, .data$variable %in% variables),
    reference_date = reference_date,
    horizon = horizon
  ), extra))

  forecast |>
    dplyr::filter(!is.na(.data$prediction)) |>
    dplyr::transmute(
      project_id = cfg$project_id,
      model_id = baseline$model_id,
      datetime = as.POSIXct(format(.data$datetime), tz = "UTC"),
      reference_datetime = as.POSIXct(format(reference_date), tz = "UTC"),
      duration = !!duration,
      .data$site_id, .data$family, .data$parameter, .data$variable, .data$prediction
    )
}

#' Submit a forecast the way a participant would
#'
#' Checks the forecast with [validate_forecast()], writes it as
#' `<theme>-<reference date>-<model_id>.csv.gz` and uploads it to the
#' submissions bucket, where the submissions stage processes it like any other.
#' In a dry run the file goes to [dry_run_dir()] instead.
#'
#' @param forecast Forecast in the standard format.
#' @inheritParams config_get
#' @param theme File name prefix.
#' @return The uploaded file's name, invisibly.
#' @export
submit_forecast <- function(forecast, cfg, theme) {
  problems <- validate_forecast(forecast, cfg)
  errors <- problems$message[problems$severity == "error"]
  if (length(errors) > 0) {
    cli::cli_abort(c("The forecast is not valid:", rlang::set_names(errors, "x")))
  }
  file <- file.path(tempdir(), paste0(theme, "-", as.Date(forecast$reference_datetime[1]),
                                      "-", forecast$model_id[1], ".csv.gz"))
  on.exit(unlink(file))
  readr::write_csv(forecast, file)
  if (!is_dry_run()) mc_connect(cfg, "submissions", alias = submissions_alias)
  upload_file(file, paste0(submissions_alias, "/", config_get(cfg, "submissions_bucket"),
                           "/", basename(file)))
  invisible(basename(file))
}

#' Run a target group's baselines
#'
#' The `baselines` stage. Reads the group's targets once, then runs and submits
#' each baseline in the group's config that has the given schedule. A baseline
#' that fails is logged and the rest carry on, and the run fails at the end.
#'
#' @inheritParams group_config
#' @param schedule Only run baselines with this `schedule` (e.g. `"daily"`,
#'   `"weekly"`). `NULL` runs them all.
#' @param today The date the forecasts are made.
#' @return A tibble with one row per baseline run: `model_id`, `rows` and
#'   `error`, invisibly.
#' @export
run_baselines <- function(cfg, group, schedule = NULL, today = Sys.Date()) {
  settings <- group_config(cfg, group)
  baselines <- settings$baselines
  if (!is.null(schedule)) {
    baselines <- purrr::keep(baselines, \(b) identical(b$schedule, schedule))
  }
  if (length(baselines) == 0) {
    log_step("{group}: no {schedule} baselines configured")
    return(invisible(tibble::tibble(model_id = character(), rows = integer(),
                                    error = character())))
  }
  vars <- dplyr::filter(target_variables(cfg), .data$group == !!group)
  log_step("{group}: reading targets")
  targets <- readr::read_csv(unique(vars$targets_file), guess_max = 1e6,
                             show_col_types = FALSE, progress = FALSE)
  theme <- settings$theme %||% tolower(group)

  results <- purrr::map(baselines, function(baseline) {
    log_step("{group}: {baseline$model_id} ({baseline$method})")
    tryCatch(
      {
        forecast <- run_baseline(baseline, targets, vars, cfg, today = today)
        submit_forecast(forecast, cfg, theme)
        tibble::tibble(model_id = baseline$model_id, rows = nrow(forecast),
                       error = NA_character_)
      },
      error = function(e) {
        cli::cli_inform(c("x" = "Failed: {conditionMessage(e)}"))
        tibble::tibble(model_id = baseline$model_id, rows = NA_integer_,
                       error = conditionMessage(e))
      }
    )
  }) |>
    purrr::list_rbind()

  failed <- results$model_id[!is.na(results$error)]
  if (length(failed) > 0) {
    cli::cli_abort("{group}: baseline{?s} {.val {failed}} failed.")
  }
  invisible(results)
}

# Fill NAs by linear interpolation, constant beyond the first and last values.
fill_linear <- function(y) {
  known <- which(!is.na(y))
  if (length(known) == 0) return(y)
  if (length(known) == 1) return(rep(y[known], length(y)))
  stats::approx(known, y[known], xout = seq_along(y), rule = 2)$y
}
