test_that("plan_submissions() skips processed and excluded keys", {
  keys <- c("a.csv", "b.csv", "x-2023-01-01-m.csv", "usgsrc4cast/c.csv", "d.csv")
  plan <- plan_submissions(keys, processed = c("a.csv", "gone.csv"),
                           exclude_patterns = c("2023", "usgsrc4cast"))
  expect_equal(plan$download, c("b.csv", "d.csv"))
  expect_equal(plan$stale, "a.csv")
  expect_equal(plan$n_excluded, 2)
})

write_submission <- function(fc, name) {
  path <- file.path(withr::local_tempdir(.local_envir = parent.frame()), name)
  readr::write_csv(fc, path)
  path
}

test_that("a valid submission is written as parquet and summaries", {
  skip_if_not_installed("score4cast")
  path <- write_submission(example_forecast(), "lakes-2024-03-01-my-model.csv.gz")
  out <- withr::local_tempdir()
  res <- process_submission(path, test_config(), out)
  expect_equal(res$status, "processed")
  # One reference date: one partition, one file each.
  expect_length(list.files(file.path(out, "parquet"), recursive = TRUE), 1)
  expect_length(list.files(file.path(out, "summaries"), recursive = TRUE), 1)
})

test_that("an invalid submission is reported, not written", {
  path <- write_submission(dplyr::select(example_forecast(), -"family"),
                           "lakes-2024-03-01-my-model.csv")
  out <- withr::local_tempdir()
  res <- suppressMessages(process_submission(path, test_config(), out))
  expect_equal(res$status, "invalid")
  expect_match(res$reason, "family")
  expect_length(list.files(out, recursive = TRUE), 0)
})

test_that("files that cannot be processed are skipped with a reason", {
  cfg <- test_config()
  cfg$submissions$min_reference_date <- "2024-01-01"
  out <- withr::local_tempdir()
  skip_reason <- function(name) {
    res <- process_submission(write_submission(example_forecast(), name), cfg, out)
    expect_equal(res$status, "skipped")
    res$reason
  }
  expect_match(skip_reason("my forecast.csv"), "name")
  expect_match(skip_reason("lakes-2024-03-01-m.parquet"), "parquet")
  expect_match(skip_reason("lakes-2023-12-31-m.csv"), "before 2024-01-01")
})
