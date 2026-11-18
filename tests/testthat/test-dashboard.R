dashboard_test_config <- function() {
  cfg <- test_config()
  cfg$target_groups$Lakes$focal_sites <- c("BBBB", "AAAA", "CCCC")
  cfg$github_repo <- "org/challenge"
  cfg$challenge_url <- "https://example.org/challenge"
  cfg$dashboard <- list(title = "Test Challenge", n_plot_sites = 2,
                        excluded_models = list(all = "bad"))
  cfg
}

# Writes `df` as parquet and opens it with duckdb, as the staging code does.
lazy_table <- function(df) {
  path <- withr::local_tempfile(fileext = ".parquet", .local_envir = parent.frame())
  arrow::write_parquet(df, path)
  duckdbfs::open_dataset(path, recursive = FALSE)
}

utc <- function(x) as.POSIXct(x, tz = "UTC")

test_that("dashboard settings have defaults the config can override", {
  s <- dashboard_settings(dashboard_test_config())
  expect_equal(s$n_plot_sites, 2)
  expect_equal(s$leaderboard_max_horizon, 35)
  expect_equal(s$excluded_models$all, "bad")
})

test_that("weekly variables look back a year, others 30 days", {
  expect_equal(dashboard_lookback_days(c("P1D", "PT30M", "P1W", "P1M")), c(30, 30, 365, 365))
})

test_that("plot_sites() takes the first focal sites of each group", {
  sites <- plot_sites(dashboard_test_config())
  expect_equal(unique(sites$site_id), c("BBBB", "AAAA"))
  expect_setequal(sites$variable, c("temperature", "oxygen"))
})

test_that("latest_forecasts() keeps the newest settled forecast at plot sites", {
  skip_if_not_installed("arrow")
  now <- utc("2024-06-10")
  summaries <- tidyr::expand_grid(
    model_id = "m", site_id = c("AAAA", "ZZZZ"), variable = "temperature",
    reference_datetime = utc(c("2024-06-01", "2024-06-05", "2024-06-09")),
    lead = 0:2
  ) |>
    dplyr::mutate(datetime = reference_datetime + lead * 86400, mean = 1,
                  quantile02.5 = 0, quantile97.5 = 2) |>
    dplyr::select(-lead)
  sites <- tibble::tibble(variable = "temperature", site_id = "AAAA")
  out <- latest_forecasts(lazy_table(summaries), sites, now)
  # 2024-06-09 is too recent (less than two days old); horizon 0 is dropped.
  expect_equal(unique(as.Date(out$reference_datetime)), as.Date("2024-06-05"))
  expect_equal(unique(out$site_id), "AAAA")
  expect_equal(nrow(out), 2)
})

test_that("scored_forecasts() picks the latest forecast old enough to have observations", {
  skip_if_not_installed("arrow")
  now <- utc("2024-06-30")
  scores <- tibble::tibble(
    model_id = "m", site_id = "AAAA", variable = "temperature",
    reference_datetime = utc(c("2024-05-01", "2024-05-25", "2024-06-20")),
    datetime = reference_datetime + 86400, mean = 1, quantile02.5 = 0, quantile97.5 = 2,
    observation = c(1, 1, NA)
  )
  sites <- tibble::tibble(variable = "temperature", site_id = "AAAA")
  out <- scored_forecasts(lazy_table(scores), sites, now, lookback = 30)
  expect_equal(as.Date(out$reference_datetime), as.Date("2024-05-25"))
})

test_that("leaderboard_scores() averages recent scores within the horizon", {
  skip_if_not_installed("arrow")
  now <- utc("2024-06-30")
  scores <- tibble::tibble(
    model_id = "m", site_id = c("A", "B", "A", "A", "A"), variable = "temperature",
    reference_datetime = utc(c("2024-06-10", "2024-06-10", "2024-06-10", "2024-06-10",
                               "2024-01-01")),
    datetime = utc(c("2024-06-11", "2024-06-11", "2024-06-10", "2024-08-30", "2024-01-02")),
    crps = c(1, 3, 100, 100, 100)
  )
  out <- leaderboard_scores(lazy_table(scores), now, lookback = 30, max_horizon = 35)
  # Horizon 0, horizon 81 and the old reference date are left out.
  expect_equal(nrow(out), 1)
  expect_equal(out$crps, 2)
  expect_equal(out$n, 2)
  expect_equal(out$horizon, 1)
})

test_that("model types group differently written answers", {
  expect_equal(model_type_label(c("empirical (a statistical model)",
                                  "Empirical (a statistical model)", NA, "Process based")),
               c("Empirical", "Empirical", "Not described", "Process based"))
})

test_that("dashboard_sites() marks which groups each site belongs to", {
  sites <- dashboard_sites(dashboard_test_config())
  expect_equal(sites$Lakes, c(TRUE, TRUE, FALSE))
  expect_equal(sites$Birds, c(FALSE, TRUE, TRUE))
})

test_that("the Quarto site settings come from the config", {
  path <- withr::local_tempfile(fileext = ".yml")
  write_quarto_metadata(dashboard_test_config(), path)
  site <- yaml::read_yaml(path)$website
  expect_equal(site$title, "Test Challenge")
  expect_equal(site$`repo-url`, "https://github.com/org/challenge")
  expect_equal(site$`site-url`, "https://example.org/challenge")
})

test_that("stac_browser_url() points at the catalog repository", {
  cfg <- list(catalog_config = list(catalog_repo = "org/cat"))
  expect_equal(stac_browser_url(cfg, "sites/collection.json"),
               paste0("https://radiantearth.github.io/stac-browser/#/external/",
                      "raw.githubusercontent.com/org/cat/main/sites/collection.json"))
})
