# Six-hourly GEFS-like rows for one site, two members and one day.
gefs_fixture <- function() {
  times <- as.POSIXct("2024-03-01", tz = "UTC") + 3600 * c(0, 6, 12, 18, 24)
  tidyr::expand_grid(site_id = "AAAA", family = "ensemble", ensemble = c("gec00", "gep01"),
                     datetime = times,
                     variable = c("TMP", "RH", "PRES", "UGRD", "VGRD", "APCP", "DSWRF", "DLWRF")) |>
    dplyr::mutate(
      horizon = sprintf("%03d", as.integer(difftime(datetime, min(datetime), units = "hours"))),
      reference_datetime = "2024-03-01",
      prediction = dplyr::case_when(variable == "TMP" ~ 10,
                                    variable == "RH" ~ 50,
                                    variable == "APCP" ~ 21.6,
                                    variable == "DSWRF" ~ 200,
                                    .default = 1)
    )
}

test_that("to_hourly() interpolates to hourly and converts units and names", {
  skip_if_not_installed("imputeTS")
  sites <- tibble::tibble(site_id = "AAAA", latitude = 40, longitude = -100)
  out <- to_hourly(gefs_fixture(), sites = sites)
  expect_equal(dplyr::n_distinct(out$datetime), 25)
  expected <- c("air_temperature", "relative_humidity", "air_pressure", "northward_wind",
                "eastward_wind", "precipitation_flux",
                "surface_downwelling_shortwave_flux_in_air",
                "surface_downwelling_longwave_flux_in_air")
  expect_setequal(unique(out$variable), expected)
  value <- function(v) unique(out$prediction[out$variable == v & !is.na(out$prediction)])
  expect_equal(value("air_temperature"), 283)      # +273 (not 273.15), kept as stored
  expect_equal(value("relative_humidity"), 0.5)
  expect_equal(value("precipitation_flux"), 21.6 / (6 * 3600))
  # Shortwave follows the sun: zero at night, so not constant.
  expect_gt(length(value("surface_downwelling_shortwave_flux_in_air")), 1)
  expect_error(to_hourly(gefs_fixture()), "sites")
})

test_that("pseudo rows get datetimes from their cycle and horizon", {
  df <- tibble::tibble(variable = c("TMP", "SNOW"), reference_datetime = "2024-03-01",
                       cycle = "06", horizon = "003",
                       datetime = as.POSIXct("2000-01-01", tz = "UTC"))
  out <- prepare_gefs(df, psuedo = TRUE)
  expect_equal(out$variable, "TMP")
  expect_equal(out$datetime, as.POSIXct("2024-03-01 09:00", tz = "UTC"))
})

test_that("ensemble members are numbered", {
  out <- number_members(tibble::tibble(ensemble = c("gec00", "gep30")))
  expect_equal(out$parameter, c(0, 30))
})

test_that("noaa_store() checks the product and writes locally in a dry run", {
  skip_if_not_installed("arrow")
  cfg <- test_config()
  expect_error(noaa_store(cfg, "stage9"), "noaa_forecast_group_paths")
  dir <- withr::local_tempdir()
  withr::local_envvar(DRY_RUN = "true", DRY_RUN_DIR = dir)
  fs <- noaa_store(cfg, "stage2", write = TRUE)
  expect_true(dir.exists(file.path(dir, "test-bucket/drivers/noaa/gefs-v12/stage2")))
  expect_equal(fs$ls(), character(0))
})

test_that("driver_sites() can use a separate stage 1 site list", {
  cfg <- test_config()
  expect_equal(nrow(driver_sites(cfg)), 3)
  list_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(site_id = "ZZZZ", latitude = "1.5", longitude = 2), list_file)
  cfg$noaa_forecasts$stage1_site_list_url <- list_file
  expect_equal(driver_sites(cfg, stage1 = TRUE)$latitude, 1.5)
  expect_equal(nrow(driver_sites(cfg)), 3)
})
