catalog_test_config <- function() {
  cfg <- test_config()
  cfg$challenge_long_name <- "Test Challenge"
  cfg$challenge_url <- "https://example.org/challenge"
  cfg$model_metadata_bucket <- "test-bucket/metadata/model_id"
  cfg$catalog_config <- list(
    catalog_path = "test-catalog", forecast_path = "test-catalog/forecasts",
    citation_doi = "10.1/challenge", citation_doi_link = "https://doi.org/10.1/challenge",
    citation_text = "The challenge paper", about_string = "https://example.org/about",
    about_title = "About", documentation_url = "https://example.org/docs",
    catalog_title = "Test Catalog", catalog_repo = "org/test-catalog",
    host_url = "https://example.org", host_name = "Host"
  )
  cfg
}

# One row per model/variable/site, as catalog_inventory() returns.
fake_inventory <- function(first, last) {
  tibble::tibble(
    model_id = c("m1", "m1", "m2"), variable = "temperature", duration = "P1D",
    site_id = c("AAAA", "BBBB", "AAAA"),
    first = as.POSIXct(first, tz = "UTC"), last = as.POSIXct(last, tz = "UTC"),
    last_reference = as.POSIXct("2024-05-01", tz = "UTC"),
    # One of m1's sites lacks pub_datetime; it must not hide the other.
    last_pub = as.POSIXct(c("2024-05-02 10:00", NA, NA), tz = "UTC")
  )
}

build_test_tree <- function(out) {
  cfg <- catalog_test_config()
  inventory <- list(
    summaries = fake_inventory(c("2024-02-01", "2024-02-01", "2024-03-01"), "2024-06-01"),
    scores = fake_inventory(c("2024-01-15", "2024-01-20", "2024-03-01"), "2024-04-01")
  )
  registered <- tibble::tibble(model_id = "m1", code_url = NA_character_, public_contact = "Yes",
                               contact_name = "Ana", description = "A test model")
  metadata <- tibble::tibble(
    variable = c("temperature", "oxygen", "richness"), duration = c("P1D", "P1D", "P1W"),
    long_name = c("Water_temperature", "Dissolved_oxygen", "Bird_richness"),
    description = "a variable"
  )
  schema <- tibble::tibble(name = c("datetime", "prediction"),
                           type = list("timestamp[us]", "double"))
  build_root_catalog(cfg, out)
  build_model_collection("forecasts", cfg, inventory, read_sites(cfg), registered,
                         catalog_variables(cfg, metadata), out, schema = schema)
}

read_json <- function(...) jsonlite::read_json(file.path(...))

test_that("duration_label() names common durations and keeps the rest", {
  expect_equal(duration_label(c("P1D", "PT30M", "P1W", "P1M")),
               c("Daily", "30min", "Weekly", "P1M"))
  cfg <- list(catalog_config = list(duration_labels = list(P1M = "Monthly")))
  expect_equal(duration_label("P1M", cfg), "Monthly")
})

test_that("catalog_subdir() strips the catalog folder from configured paths", {
  cfg <- list(catalog_config = list(catalog_path = "cat", targets_path = "cat/targets/"))
  expect_equal(catalog_subdir(cfg, "targets_path", "x"), "targets")
  expect_equal(catalog_subdir(cfg, "noaa_path", "noaa_forecasts"), "noaa_forecasts")
})

test_that("stac helpers build bboxes, datetimes, columns and optional links", {
  sites <- tibble::tibble(site_id = c("A", "B", "C"), latitude = c(1, 2, 3),
                          longitude = c(-1, -2, -3))
  expect_equal(stac_bbox(sites, c("A", "B")), c(-2, 1, -1, 2))
  expect_null(stac_bbox(sites, "Z"))
  expect_equal(stac_datetime(as.POSIXct("2024-01-02 13:00", tz = "UTC")), "2024-01-02T00:00:00Z")
  expect_null(stac_link("about", NULL))
  cols <- stac_columns(tibble::tibble(name = c("a", "b"), type = list("string", "double")),
                       c(a = "column a"))
  expect_equal(cols[[1]], list(name = "a", type = "string", description = "column a"))
  expect_equal(cols[[2]], list(name = "b", type = "double"))
})

test_that("column descriptions ship for every collection kind", {
  for (kind in c("forecasts", "summaries", "scores", "noaa", "targets")) {
    expect_true("datetime" %in% names(column_descriptions(kind)), info = kind)
  }
})

test_that("build_model_collection() writes groups, variables and registered models only", {
  out <- withr::local_tempdir()
  # A stale variable from an earlier build must disappear.
  dir.create(file.path(out, "forecasts", "Lakes", "Daily_Old"), recursive = TRUE)
  groups <- build_test_tree(out)

  expect_equal(groups, "Lakes")
  root <- file.path(out, "forecasts")
  models <- file.path(root, "Lakes", "Daily_Water_temperature", "models")
  expect_true(file.exists(file.path(models, "m1.json")))
  expect_false(file.exists(file.path(models, "m2.json")))
  expect_false(dir.exists(file.path(root, "Lakes", "Daily_Old")))
  expect_false(dir.exists(file.path(root, "Lakes", "Daily_Dissolved_oxygen")))
  expect_false(dir.exists(file.path(root, "Birds")))

  top <- read_json(root, "collection.json")
  children <- Filter(function(l) l$rel == "child", top$links)
  expect_equal(vapply(children, `[[`, "", "href"), "Lakes/collection.json")
  # Dates span both inventories: scores start earlier, summaries end later.
  expect_equal(unlist(top$extent$temporal$interval),
               c("2024-01-15T00:00:00Z", "2024-06-01T00:00:00Z"))

  group <- read_json(root, "Lakes", "collection.json")
  expect_equal(vapply(group$`sci:publications`, `[[`, "", "doi"), c("10.1/challenge", "10.1/x"))
})

test_that("model items carry the model's dates, sites and links", {
  out <- withr::local_tempdir()
  build_test_tree(out)
  item <- read_json(out, "forecasts", "Lakes", "Daily_Water_temperature", "models", "m1.json")
  expect_equal(item$id, "m1_temperature_P1D_forecast")
  expect_equal(item$properties$start_datetime, "2024-01-15T00:00:00Z")
  expect_equal(item$properties$end_datetime, "2024-06-01T00:00:00Z")
  expect_equal(item$properties$datetime, "2024-05-01T00:00:00Z")
  expect_equal(item$properties$updated, "2024-05-02T00:00:00Z")
  expect_length(item$geometry$coordinates, 2)
  expect_equal(item$properties$providers[[1]]$name, "Ana")
  # No code link registered: fall back to the challenge's page.
  expect_equal(item$assets$`2`$href, "https://example.org/challenge")
  hrefs <- vapply(item$links, `[[`, "", "href")
  expect_true(paste0("https://raw.githubusercontent.com/org/test-catalog/main/forecasts/Lakes/",
                     "Daily_Water_temperature/models/m1.json") %in% hrefs)
  partition <- "project_id=testcast/duration=P1D/variable=temperature/model_id=m1?"
  expect_match(item$assets$`4`$href, paste0("forecasts/bundled-parquet/", partition), fixed = TRUE)
})

test_that("every relative link in the catalog points to a file that exists", {
  out <- withr::local_tempdir()
  build_test_tree(out)
  jsons <- list.files(file.path(out, "forecasts"), "\\.json$", recursive = TRUE, full.names = TRUE)
  for (file in jsons) {
    for (link in read_json(file)$links) {
      if (grepl("^https?://", link$href)) next
      target <- file.path(dirname(file), sub("^\\./", "", link$href))
      expect_true(file.exists(target), info = paste(file, link$rel, link$href))
    }
  }
})

test_that("a challenge without NOAA drivers gets no NOAA catalog part or link", {
  cfg <- catalog_test_config()
  expect_true("noaa" %in% default_catalog_parts(cfg))
  cfg$noaa_forecasts <- NULL
  expect_false("noaa" %in% default_catalog_parts(cfg))
  out <- withr::local_tempdir()
  build_root_catalog(cfg, out)
  titles <- vapply(read_json(out, "catalog.json")$links, function(l) l$title %||% "", "")
  expect_false("NOAA Forecasts" %in% titles)
})
