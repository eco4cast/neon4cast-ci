test_that("is_dry_run() reads DRY_RUN", {
  withr::local_envvar(DRY_RUN = NA)
  expect_false(is_dry_run())
  for (value in c("true", "TRUE", "1", "yes")) {
    withr::local_envvar(DRY_RUN = value)
    expect_true(is_dry_run())
  }
  withr::local_envvar(DRY_RUN = "false")
  expect_false(is_dry_run())
})

test_that("output_uri() redirects writes only in a dry run", {
  withr::local_envvar(DRY_RUN = "false")
  expect_equal(output_uri("s3://bucket/a/b.parquet"), "s3://bucket/a/b.parquet")

  dir <- withr::local_tempdir()
  withr::local_envvar(DRY_RUN = "true", DRY_RUN_DIR = dir)
  expect_equal(output_uri("s3://bucket/a/b.parquet"), file.path(dir, "bucket/a/b.parquet"))
  expect_equal(output_uri("main/bucket/a.csv"), file.path(dir, "main/bucket/a.csv"))
  # The parent folder exists, ready for duckdb to write into.
  expect_true(dir.exists(file.path(dir, "bucket/a")))
})

test_that("log_step() timestamps and interpolates", {
  n <- 3
  expect_message(log_step("Scoring {n} groups"), "\\] Scoring 3 groups")
})

test_that("ping_healthcheck() skips jobs without a URL and dry runs", {
  cfg <- list(ci = list(healthchecks = list(job = "https://example.invalid/ping")))
  expect_false(ping_healthcheck(cfg, "other-job"))
  withr::local_envvar(DRY_RUN = "true")
  expect_false(ping_healthcheck(cfg, "job"))
})

test_that("an unreachable healthcheck warns instead of failing the job", {
  path <- withr::local_tempfile()
  cfg <- list(ci = list(healthchecks = list(job = file.path(path, "missing"))))
  local_mocked_bindings(with_retry = function(expr, ...) expr)
  expect_warning(sent <- ping_healthcheck(cfg, "job"), "could not be reached")
  expect_false(sent)
})
