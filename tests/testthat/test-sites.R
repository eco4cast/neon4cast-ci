test_that("read_sites() renames columns to standard names", {
  sites <- read_sites(test_config())
  expect_equal(nrow(sites), 3)
  expect_true(all(c("site_id", "site_name", "latitude", "longitude") %in% names(sites)))
  expect_false("field_site_id" %in% names(sites))
})

test_that("read_sites() filters to a group's sites", {
  cfg <- test_config()
  # Default membership column: the lower-case group name.
  expect_equal(read_sites(cfg, "Lakes")$site_id, c("AAAA", "BBBB"))
  # Overridden with sites_column.
  expect_equal(read_sites(cfg, "Birds")$site_id, c("BBBB", "CCCC"))
})

test_that("read_sites() falls back to catalog_config$site_metadata_url", {
  cfg <- test_config()
  cfg$catalog_config$site_metadata_url <- cfg$sites$metadata_url
  cfg$sites$metadata_url <- NULL
  expect_equal(nrow(read_sites(cfg)), 3)
})

test_that("read_sites() explains missing columns", {
  cfg <- test_config()
  cfg$sites$columns$site_id <- "siteID"
  expect_error(read_sites(cfg), "siteID")

  cfg <- test_config()
  cfg$target_groups$Birds$sites_column <- "birds"
  expect_error(read_sites(cfg, "Birds"), "sites_column")
})
