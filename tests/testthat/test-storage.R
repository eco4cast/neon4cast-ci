test_that("bucket_uri() formats paths for each tool", {
  cfg <- test_config()
  expect_equal(bucket_uri(cfg, "forecasts_bucket", "bundled-parquet"),
               "s3://test-bucket/challenges/forecasts/bundled-parquet")
  expect_equal(bucket_uri(cfg, "forecasts_bucket", "bundled-parquet", scheme = "mc"),
               "main/test-bucket/challenges/forecasts/bundled-parquet")
  expect_equal(bucket_uri(cfg, "scores_bucket", scheme = "https"),
               "https://s3.example.org/test-bucket/challenges/scores")
})

test_that("bucket_uri() picks the store, and alias, from the bucket", {
  cfg <- test_config()
  expect_equal(bucket_uri(cfg, "submissions_bucket", "raw", scheme = "mc"),
               "submissions/submissions/raw")
  expect_equal(bucket_uri(cfg, "scratch", "score_me", scheme = "https"),
               "https://scratch.example.org/scratch-bucket/tmp/score_me")
  expect_equal(bucket_uri(cfg, "forecasts_bucket", scheme = "mc", alias = "osn"),
               "osn/test-bucket/challenges/forecasts")
})

test_that("bucket_uri() tidies stray slashes", {
  cfg <- test_config()
  expect_equal(bucket_uri(cfg, "targets_bucket", "/project_id=x/", "a.csv"),
               "s3://test-bucket/challenges/targets/project_id=x/a.csv")
})

test_that("bucket_uri() names a missing bucket setting", {
  expect_error(bucket_uri(test_config(), "archive_bucket"), "archive_bucket")
})

test_that("store_credentials() reads the configured environment variables", {
  cfg <- test_config()
  withr::local_envvar(OSN_KEY = "k", OSN_SECRET = "s")
  expect_equal(store_credentials(cfg, "main"),
               list(endpoint = "s3.example.org", key = "k", secret = "s"))
  # NOAA falls back to the main endpoint and credentials.
  expect_equal(store_credentials(cfg, "noaa")$key, "k")
})

test_that("store_credentials() explains missing credentials", {
  cfg <- test_config()
  withr::local_envvar(EFI_NRP_KEY = NA, EFI_NRP_SECRET = NA)
  expect_error(store_credentials(cfg, "scratch"), "EFI_NRP_KEY")
  expect_equal(store_credentials(cfg, "scratch", anonymous = TRUE),
               list(endpoint = "scratch.example.org", key = "", secret = ""))
})

test_that("store_credentials() rejects unknown stores", {
  expect_error(store_credentials(test_config(), "osn"), "main")
})

test_that("duckdb secrets are scoped to each store's bucket", {
  cfg <- test_config()
  expect_equal(store_bucket(cfg, "main"), "test-bucket")
  expect_equal(store_bucket(cfg, "scratch"), "scratch-bucket")
  expect_null(store_bucket(list(), "scratch"))
})

test_that("with_retry() retries, then re-raises the last error", {
  attempts <- 0
  flaky <- function() {
    attempts <<- attempts + 1
    if (attempts < 2) stop("network blip")
    "ok"
  }
  expect_warning(result <- with_retry(flaky(), times = 3, wait = 0), "network blip")
  expect_equal(result, "ok")
  expect_equal(attempts, 2)

  expect_error(
    suppressWarnings(with_retry(stop("down"), times = 2, wait = 0)),
    "Failed after 2 attempts"
  )
})
