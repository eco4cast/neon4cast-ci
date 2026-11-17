scored_row <- function(...) {
  defaults <- list(
    project_id = "p", site_id = "AAAA",
    datetime = as.POSIXct("2024-03-02", tz = "UTC"),
    duration = "P1D", variable = "temperature", model_id = "m", family = "normal",
    reference_datetime = as.POSIXct("2024-03-01", tz = "UTC"),
    pub_datetime = as.POSIXct("2024-03-01 12:00:00", tz = "UTC"),
    observation = 10, crps = 0.5
  )
  # modifyList() drops NULLs but keeps NA, which the tests need.
  tibble::as_tibble(utils::modifyList(defaults, list(...)))
}

test_that("new scores replace old scores for the same forecast row", {
  dir <- withr::local_tempdir()
  existing <- file.path(dir, "scores.parquet")
  old <- dplyr::bind_rows(
    scored_row(),                                                   # rescored below
    scored_row(site_id = "BBBB"),                                   # kept
    scored_row(site_id = NA, crps = 0.7)                            # NA key, rescored
  )
  duckdbfs::write_dataset(old, existing)
  new <- dplyr::bind_rows(
    scored_row(observation = 11, crps = 0.9),   # revised observation
    scored_row(site_id = NA, crps = 0.8)
  )
  merged <- merge_scores(new, existing) |>
    dplyr::collect() |>
    dplyr::arrange(.data$site_id)
  expect_equal(nrow(merged), 3)
  expect_equal(merged$crps, c(0.9, 0.5, 0.8))
})

test_that("with no existing scores, new scores are de-duplicated", {
  merged <- merge_scores(dplyr::bind_rows(scored_row(), scored_row()), NULL) |>
    dplyr::collect()
  expect_equal(nrow(merged), 1)
})

test_that("score_rows() matches score4cast", {
  skip_if_not_installed("score4cast")
  joined <- example_forecast() |>
    dplyr::mutate(observation = 11)
  scores <- score_rows(joined)
  expect_equal(nrow(scores), nrow(joined) / 2)
  # CRPS of N(10, 1) at 11.
  expected <- scoringRules::crps_norm(11, mean = 10, sd = 1)
  expect_equal(unique(scores$crps), expected)
  expect_true(all(c("logs", "mean", "quantile97.5", "observation") %in% names(scores)))
})
