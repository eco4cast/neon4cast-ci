test_that("mc_to_s3() swaps the alias for s3://", {
  expect_equal(mc_to_s3(c("main/b/x.parquet", "main/b/y.parquet")),
               c("s3://b/x.parquet", "s3://b/y.parquet"))
  expect_error(mc_to_s3("other/b/x.parquet"), "alias")
})

test_that("upload_dir() copies a folder locally in a dry run", {
  dir <- withr::local_tempdir()
  withr::local_envvar(DRY_RUN = "true", DRY_RUN_DIR = dir)
  from <- withr::local_tempdir()
  dir.create(file.path(from, "a=1"))
  writeLines("x", file.path(from, "a=1", "f.parquet"))
  expect_message(upload_dir(from, "main/bucket/parquet"), "DRY RUN")
  expect_true(file.exists(file.path(dir, "main/bucket/parquet/a=1/f.parquet")))
})

test_that("moves and deletes only log in a dry run", {
  withr::local_envvar(DRY_RUN = "true")
  expect_message(move_remote("main/b/x", "main/b/y"), "would move")
  expect_message(remove_remote("main/b/x"), "would delete")
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
