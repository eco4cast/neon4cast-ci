# Daily targets for two sites and two variables: three years of a seasonal
# cycle, with the observation equal to the day of year (plus year noise).
daily_targets <- function(end = as.Date("2026-11-17")) {
  dates <- seq(end - 3 * 365, end - 1, by = "day")
  tidyr::expand_grid(site_id = c("AAAA", "BBBB"), variable = c("temperature", "oxygen"),
                     datetime = dates) |>
    dplyr::mutate(observation = lubridate::yday(datetime) +
                    (lubridate::year(datetime) - 2024),
                  datetime = as.POSIXct(datetime, tz = "UTC"))
}

weekly_targets <- function(ref = as.Date("2026-11-16")) {
  dates <- seq(ref - 7 * 150, ref - 7, by = 7)
  tidyr::expand_grid(site_id = c("AAAA", "BBBB"), variable = "richness", datetime = dates) |>
    dplyr::mutate(observation = rep(c(0, 2, 4, 6), length.out = dplyr::n()))
}

test_that("reference dates: today for daily, the ISO week's Monday for weekly", {
  thursday <- as.Date("2026-11-19")
  expect_equal(reference_date_for("P1D", thursday), thursday)
  expect_equal(reference_date_for("P1W", thursday), as.Date("2026-11-16"))
  expect_equal(reference_date_for("P1W", as.Date("2026-11-16")), as.Date("2026-11-16"))
  expect_error(reference_date_for("PT30M", thursday), "P1D and P1W")
})

test_that("climatology forecasts each day's historical mean", {
  ref <- as.Date("2026-11-17")
  fc <- baseline_climatology(daily_targets(), ref, horizon = 10)
  expect_named(fc, c("site_id", "variable", "datetime", "family", "parameter", "prediction"))
  expect_equal(sort(unique(fc$datetime)), ref + 1:10)
  mu <- dplyr::filter(fc, parameter == "mu", site_id == "AAAA", variable == "oxygen")
  # Years 2023-2025 on each day of year: mean of doy + (-1, 0, 1) = doy.
  expect_equal(mu$prediction, lubridate::yday(ref + 1:10))
  expect_true(all(fc$prediction[fc$parameter == "sigma"] == 1))
})

test_that("climatology fills missing days and can exclude the current year", {
  ref <- as.Date("2026-11-17")
  targets <- daily_targets() |>
    dplyr::filter(lubridate::yday(datetime) != lubridate::yday(ref + 3))
  mu <- baseline_climatology(targets, ref, horizon = 5) |>
    dplyr::filter(parameter == "mu", site_id == "AAAA", variable == "oxygen")
  expect_equal(nrow(mu), 5)   # the missing day is interpolated
  expect_equal(mu$prediction[3], lubridate::yday(ref + 3))

  this_year <- daily_targets() |> dplyr::filter(lubridate::year(datetime) == 2026)
  expect_equal(nrow(baseline_climatology(this_year, ref, 5, exclude_current_year = TRUE)), 0)
})

test_that("weekly climatology is a positive weekly ensemble", {
  ref <- as.Date("2026-11-16")
  fc <- baseline_weekly_climatology(weekly_targets(), ref, horizon = 365, n_members = 50)
  expect_equal(sort(unique(fc$datetime)), ref + 7 * 1:52)
  expect_equal(dplyr::n_distinct(fc$parameter), 50)
  expect_true(all(fc$prediction > 0))
  expect_equal(unique(fc$family), "ensemble")
})

test_that("persistence starts from the last observation and covers the horizon", {
  skip_if_not_installed("fable")
  ref <- as.Date("2026-11-17")
  targets <- daily_targets() |>
    dplyr::filter(variable == "oxygen") |>
    # BBBB stopped reporting five days ago; AAAA has a zero to log-transform.
    dplyr::filter(!(site_id == "BBBB" & datetime > as.POSIXct(ref - 5))) |>
    dplyr::mutate(observation = dplyr::if_else(dplyr::row_number() == 10, 0, observation))
  fc <- baseline_persistence_rw(targets, ref, horizon = 7,
                                transform = list(oxygen = "log"), n_members = 5)
  expect_equal(sort(unique(fc$datetime)), ref + 1:7)
  expect_equal(dplyr::n_distinct(fc$parameter), 5)
  expect_equal(nrow(fc), 2 * 7 * 5)
  expect_false(anyNA(fc$prediction))
})

test_that("the mean model gives a year of weekly forecasts", {
  skip_if_not_installed("fable")
  ref <- as.Date("2026-11-16")
  fc <- baseline_mean(weekly_targets(), ref, horizon = 365, n_members = 10)
  expect_equal(sort(unique(fc$datetime)), ref + 7 * 1:52)
  expect_equal(nrow(fc), 2 * 52 * 10)
})

test_that("run_baseline() returns a valid forecast in the standard format", {
  cfg <- test_config()
  vars <- dplyr::filter(target_variables(cfg), group == "Lakes")
  fc <- run_baseline(group_config(cfg, "Lakes")$baselines[[1]], daily_targets(), vars, cfg,
                     today = as.Date("2026-11-17"))
  expect_named(fc, c("project_id", "model_id", "datetime", "reference_datetime", "duration",
                     "site_id", "family", "parameter", "variable", "prediction"))
  expect_equal(unique(fc$reference_datetime), as.POSIXct("2026-11-17", tz = "UTC"))
  expect_equal(length(unique(fc$datetime)), 35)   # the variables' max_horizon
  expect_equal(nrow(validate_forecast(fc, cfg)), 0)
})

test_that("run_baseline() explains configuration mistakes", {
  cfg <- test_config()
  vars <- target_variables(cfg)
  expect_error(run_baseline(list(model_id = "x", method = "magic"), daily_targets(),
                            vars, cfg), "Unknown baseline method")
  # Lakes (P1D) and Birds (P1W) variables together have no single duration.
  expect_error(run_baseline(list(model_id = "x", method = "climatology"), daily_targets(),
                            vars, cfg), "one duration")
})

test_that("run_baselines() submits each baseline and carries on past failures", {
  skip_if_not_installed("fable")
  dir <- withr::local_tempdir()
  withr::local_envvar(DRY_RUN = "true", DRY_RUN_DIR = dir)
  cfg <- test_config()
  targets_file <- file.path(dir, "lakes-targets.csv")
  readr::write_csv(daily_targets(Sys.Date()), targets_file)
  cfg$target_groups$Lakes$targets_file <- targets_file
  broken <- list(model_id = "broken", method = "magic", schedule = "daily")
  cfg$target_groups$Lakes$baselines[[3]] <- broken

  expect_error(suppressMessages(run_baselines(cfg, "Lakes", schedule = "daily")), "broken")
  submitted <- list.files(file.path(dir, "submissions", "submissions"))
  expect_setequal(submitted, paste0("lakes-", Sys.Date(), c("-climatology", "-persistenceRW"),
                                    ".csv.gz"))
  # Nothing is scheduled weekly for Lakes.
  expect_equal(nrow(suppressMessages(run_baselines(cfg, "Lakes", schedule = "weekly"))), 0)
})

test_that("submit_forecast() refuses an invalid forecast", {
  cfg <- test_config()
  fc <- dplyr::select(example_forecast(), -"family")
  expect_error(submit_forecast(fc, cfg, "lakes"), "not valid")
})
