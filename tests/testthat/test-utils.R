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

  withr::local_envvar(DRY_RUN = "true", DRY_RUN_DIR = "out")
  expect_equal(output_uri("s3://bucket/a/b.parquet"), "out/bucket/a/b.parquet")
  expect_equal(output_uri("main/bucket/a.csv"), "out/main/bucket/a.csv")
})

test_that("upload_file() copies locally in a dry run", {
  dir <- withr::local_tempdir()
  withr::local_envvar(DRY_RUN = "true", DRY_RUN_DIR = dir)
  from <- withr::local_tempfile(lines = "x,y")
  expect_message(dest <- upload_file(from, "main/bucket/targets/t.csv"), "DRY RUN")
  expect_equal(dest, file.path(dir, "main/bucket/targets/t.csv"))
  expect_equal(readLines(dest), "x,y")
  expect_error(upload_file("missing.csv", "main/bucket/t.csv"), "does not exist")
})

test_that("log_step() timestamps and interpolates", {
  n <- 3
  expect_message(log_step("Scoring {n} groups"), "\\] Scoring 3 groups")
})
