test_that("file names parse, including model ids with dashes and prefixes", {
  info <- parse_submission_filename(c(
    "aquatics-2024-03-01-my-model.csv.gz",
    "folder/ticks-2025-01-06-tg_arima.csv",
    "phenology-2024-03-01-m.nc"
  ))
  expect_equal(info$theme, c("aquatics", "ticks", "phenology"))
  expect_equal(info$model_id, c("my-model", "tg_arima", "m"))
  expect_equal(info$extension, c("csv.gz", "csv", "nc"))
  expect_equal(format(info$reference_datetime[2]), "2025-01-06")
})

test_that("bad file names and impossible dates give NA", {
  info <- parse_submission_filename(c("notes.txt", "a-2024-02-30-m.csv",
                                      "a-2024-03-01-m.xlsx"))
  expect_true(all(is.na(info$model_id)))
  expect_true(all(is.na(info$reference_datetime)))
})

test_that("a good forecast has no problems", {
  expect_equal(nrow(validate_forecast(example_forecast(), test_config())), 0)
  expect_equal(nrow(validate_forecast(example_forecast(family = "sample"),
                                      test_config())), 0)
})

test_that("format errors make a forecast invalid", {
  cfg <- test_config()
  errors <- function(fc) {
    p <- validate_forecast(fc, cfg)
    p$message[p$severity == "error"]
  }
  fc <- example_forecast()
  expect_match(errors(fc[0, ]), "no rows", all = FALSE)
  expect_match(errors(dplyr::mutate(fc, model_id = c("a", rep("b", nrow(fc) - 1)))),
               "more than one model_id")
  expect_match(errors(dplyr::mutate(fc, model_id = NA)), "model_id is NA")
  expect_match(errors(dplyr::select(fc, -"prediction")), "prediction")
  expect_match(errors(dplyr::mutate(fc, prediction = "high")), "not numeric")
  expect_match(errors(dplyr::rename(fc, ensemble = "parameter")), "ensemble")
  expect_match(errors(dplyr::select(fc, -"family")), "family")
  expect_match(errors(dplyr::select(fc, -"parameter")), "parameter")
  expect_match(errors(dplyr::mutate(fc, family = c("normal", rep("sample", nrow(fc) - 1)))),
               "more than one family")
  expect_match(errors(dplyr::mutate(fc, datetime = "soon")), "datetime")
  expect_match(errors(dplyr::select(fc, -"reference_datetime")), "reference_datetime")

  big <- tidyr::expand_grid(dplyr::distinct(dplyr::select(fc, -"parameter", -"prediction")),
                            parameter = as.character(1:501)) |>
    dplyr::mutate(family = "sample", prediction = 1)
  expect_match(errors(big), "500")
})

test_that("recoverable problems are warnings", {
  cfg <- test_config()
  warnings <- function(fc) {
    p <- validate_forecast(fc, cfg)
    expect_false(any(p$severity == "error"))
    p$message
  }
  fc <- example_forecast()
  expect_match(warnings(dplyr::select(fc, -"model_id")), "file name")
  expect_match(warnings(dplyr::mutate(fc, variable = "salinity")), "salinity")
  expect_match(warnings(dplyr::select(fc, -"duration")), "duration")
  expect_match(warnings(dplyr::mutate(fc, duration = "P1M")), "P1M")
  expect_match(warnings(dplyr::rename(fc, start_time = "reference_datetime")), "start_time")
  expect_match(warnings(dplyr::select(fc, -"site_id")), "site_id")
})

test_that("standardize_forecast() fills in and fixes columns", {
  cfg <- test_config()
  pub <- as.POSIXct("2024-03-02 10:11:12.7", tz = "UTC")
  fc <- example_forecast(family = "sample") |>
    dplyr::mutate(family = "ensemble", model_id = "null", project_id = NULL) |>
    dplyr::select(-"duration", -"reference_datetime")
  out <- standardize_forecast(fc, cfg, example_file_info(), pub_datetime = pub)

  expect_equal(unique(out$duration), "P1D")        # from the config
  expect_equal(unique(out$family), "sample")
  expect_equal(unique(out$model_id), "my-model")   # from the file name
  expect_equal(unique(out$reference_date), as.Date("2024-03-01"))
  expect_equal(unique(out$project_id), "testcast")
  expect_equal(unique(out$pub_datetime), as.POSIXct("2024-03-02 10:11:12", tz = "UTC"))
  expect_type(out$parameter, "character")
})

test_that("standardize_forecast() fixes the old PT30 spelling", {
  fc <- dplyr::mutate(example_forecast(), duration = "PT30")
  out <- standardize_forecast(fc, test_config(), example_file_info())
  expect_equal(unique(out$duration), "PT30M")
})

test_that("rows before the reference date or beyond the horizon are dropped", {
  cfg <- test_config()   # temperature: P1D, max_horizon 35
  fc <- example_forecast(n_days = 1) |>
    dplyr::select(-"datetime") |>
    tidyr::expand_grid(datetime = as.POSIXct("2024-03-01", tz = "UTC") +
                         c(-1, 0, 35, 35.5, 36) * 86400)
  expect_message(out <- standardize_forecast(fc, cfg, example_file_info()), "Dropped")
  horizons <- as.numeric(difftime(unique(out$datetime), as.POSIXct("2024-03-01", tz = "UTC"),
                                  units = "days"))
  expect_equal(sort(horizons), c(0, 35))

  # A weekly variable with a long horizon keeps rows a daily one would drop.
  weekly <- dplyr::mutate(fc, variable = "richness", duration = "P1W")
  out <- suppressMessages(standardize_forecast(weekly, cfg, example_file_info()))
  expect_equal(length(unique(out$datetime)), 4)

  # Variables not in the config get 35 days.
  other <- dplyr::mutate(fc, variable = "salinity")
  out <- suppressMessages(standardize_forecast(other, cfg, example_file_info()))
  expect_equal(length(unique(out$datetime)), 2)
})

test_that("each submission writes its own parquet files", {
  dir <- withr::local_tempdir()
  out <- file.path(dir, "nested", "parquet")
  fc <- standardize_forecast(example_forecast(), test_config(), example_file_info())
  write_forecast_parquet(fc, out, "sub_a")
  write_forecast_parquet(fc, out, "sub_b")
  write_forecast_parquet(fc, out, "sub_a")   # reprocessing replaces its own file
  files <- list.files(out, recursive = TRUE)
  expect_equal(basename(files), c("sub_a_0.parquet", "sub_b_0.parquet"))
  expect_match(files[1], paste0("project_id=testcast/duration=P1D/variable=temperature/",
                                "model_id=my-model/reference_date=2024-03-01"))
})

test_that("summaries have one row per forecast row", {
  skip_if_not_installed("score4cast")
  fc <- standardize_forecast(example_forecast(), test_config(), example_file_info())
  s <- summarize_submission(fc)
  expect_equal(nrow(s), nrow(fc) / 2)
  expect_equal(unique(s$mean), 10)
  expected <- c("median", "sd", "quantile02.5", "quantile97.5", "reference_date")
  expect_true(all(expected %in% names(s)))
})
