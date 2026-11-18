targets_fixture <- function(n_sites = 2, days = 3) {
  tidyr::expand_grid(site_id = c("AAAA", "BBBB", "CCCC")[seq_len(n_sites)],
                     datetime = as.Date("2024-03-01") + 0:(days - 1),
                     variable = c("temperature", "oxygen")) |>
    dplyr::mutate(observation = 1)
}

test_that("targets_object() turns the targets URL into a store path", {
  cfg <- test_config()
  expect_equal(targets_object(cfg, "Lakes"), "main/test-bucket/lakes-targets.csv.gz")
  cfg$target_groups$Lakes$targets_file <- "https://elsewhere.org/x.csv"
  expect_error(targets_object(cfg, "Lakes"), "must be a URL starting")
})

test_that("format_targets() gives the standard columns and types", {
  raw <- targets_fixture() |> dplyr::mutate(observation = c(NaN, rep(2, 11)))
  out <- format_targets(raw, test_config(), "P1D")
  expect_named(out, c("project_id", "site_id", "datetime", "duration", "variable", "observation"))
  expect_s3_class(out$datetime, "POSIXct")
  expect_equal(unique(out$project_id), "testcast")
  expect_equal(sum(is.na(out$observation)), 1)
  expect_false(any(is.nan(out$observation)))
  expect_error(format_targets(dplyr::select(raw, -"site_id"), test_config(), "P1D"), "site_id")
})

test_that("check_targets() passes good targets", {
  cfg <- test_config()
  new <- format_targets(targets_fixture(days = 4), cfg, "P1D")
  old <- format_targets(targets_fixture(days = 3), cfg, "P1D")
  expect_message(out <- check_targets(new, cfg, "Lakes", published = old), "published")
  expect_identical(out, new)
  # Nothing published yet.
  expect_silent(check_targets(new, cfg, "Lakes", published = NULL))
})

test_that("check_targets() stops bad targets from replacing good ones", {
  cfg <- test_config()
  old <- format_targets(targets_fixture(n_sites = 2, days = 10), cfg, "P1D")
  fails <- function(new, pattern) {
    expect_error(suppressMessages(check_targets(new, cfg, "Lakes", published = old)), pattern)
  }
  fails(format_targets(targets_fixture(n_sites = 2, days = 5), cfg, "P1D"), "fewer than 95%")
  fails(format_targets(targets_fixture(n_sites = 1, days = 20), cfg, "P1D"), "sites missing: BBBB")
  fails(dplyr::filter(old, variable == "oxygen") |>
          dplyr::bind_rows(dplyr::mutate(dplyr::filter(old, variable == "oxygen"),
                                         datetime = datetime + 86400 * 30)),
        "variables missing: temperature")
  fails(dplyr::bind_rows(old, old[1, ]), "1 duplicated")
  fails(dplyr::mutate(old, variable = dplyr::if_else(variable == "oxygen", "salinity", variable)),
        "not in the config: salinity")
  fails(dplyr::select(old, -"duration"), "standard")
})

test_that("write_targets() uploads to the targets file's location", {
  dir <- withr::local_tempdir()
  withr::local_envvar(DRY_RUN = "true", DRY_RUN_DIR = dir)
  cfg <- test_config()
  targets <- format_targets(targets_fixture(), cfg, "P1D")
  expect_message(write_targets(targets, cfg, "Lakes"), "DRY RUN")
  written <- readr::read_csv(file.path(dir, "main/test-bucket/lakes-targets.csv.gz"),
                             show_col_types = FALSE)
  expect_equal(nrow(written), nrow(targets))
})
