# Write a parquet file of forecast rows and return its path.
write_rows <- function(dir, name, ...) {
  defaults <- list(
    site_id = "AAAA",
    datetime = as.POSIXct("2024-03-02", tz = "UTC"),
    parameter = "mu",
    prediction = 1,
    family = "normal",
    model_id = "m",
    reference_datetime = as.POSIXct("2024-03-01", tz = "UTC"),
    pub_datetime = as.POSIXct("2024-03-01 12:00:00", tz = "UTC")
  )
  rows <- tibble::as_tibble(utils::modifyList(defaults, list(...)))
  path <- file.path(dir, name)
  duckdbfs::write_dataset(rows, path)
  path
}

read_all <- function(path) {
  duckdbfs::open_dataset(path, recursive = FALSE) |> dplyr::collect()
}

test_that("a model with no bundle yet gets one from its new rows", {
  dir <- withr::local_tempdir()
  new <- write_rows(dir, "new.parquet")
  out <- merge_bundle(new, NULL, file.path(dir, "bundle.parquet"))
  expect_equal(nrow(read_all(out)), 1)
})

test_that("new rows are appended to the existing bundle", {
  dir <- withr::local_tempdir()
  old <- write_rows(dir, "old.parquet")
  new <- c(
    write_rows(dir, "new1.parquet", prediction = 2,
               pub_datetime = as.POSIXct("2024-03-05", tz = "UTC")),
    write_rows(dir, "new2.parquet", prediction = 3,
               reference_datetime = as.POSIXct("2024-03-02", tz = "UTC"))
  )
  out <- read_all(merge_bundle(new, old, file.path(dir, "bundle.parquet")))
  expect_equal(sort(out$prediction), c(1, 2, 3))
})

test_that("rows already in the bundle are not appended again", {
  dir <- withr::local_tempdir()
  old <- write_rows(dir, "old.parquet")
  # Same reference_datetime and pub_datetime: bundled by an earlier run.
  new <- write_rows(dir, "new.parquet", prediction = 99)
  out <- read_all(merge_bundle(new, old, file.path(dir, "bundle.parquet")))
  expect_equal(out$prediction, 1)
})

test_that("incomplete forecast rows are dropped, but not for summaries", {
  dir <- withr::local_tempdir()
  new <- write_rows(dir, "new.parquet", note = 1)
  bad <- write_rows(dir, "bad.parquet", note = 2)
  duckdbfs::write_dataset(dplyr::mutate(read_all(bad), prediction = NA_real_), bad)
  expect_equal(nrow(read_all(merge_bundle(c(new, bad), NULL, file.path(dir, "f.parquet")))), 1)
  expect_equal(nrow(read_all(merge_bundle(c(new, bad), NULL, file.path(dir, "s.parquet"),
                                          drop_incomplete = FALSE))), 2)
})

test_that("files are grouped by model folder", {
  files <- c(
    "main/b/parquet/project_id=p/model_id=a/reference_date=2024-01-01/x.parquet",
    "main/b/parquet/project_id=p/model_id=a/reference_date=2024-01-02/y.parquet",
    "main/b/parquet/project_id=p/model_id=b/reference_date=2024-01-01/x.parquet"
  )
  groups <- files_by_model(files)
  expect_named(groups, c("main/b/parquet/project_id=p/model_id=a",
                         "main/b/parquet/project_id=p/model_id=b"))
  expect_length(groups[[1]], 2)
})

test_that("bundle_layout() names the folders for each kind", {
  expect_equal(bundle_layout("forecasts")$archive, "archive-parquet")
  expect_equal(bundle_layout("summaries")$bucket, "summaries_bucket")
  expect_error(bundle_layout("scores"))
})
