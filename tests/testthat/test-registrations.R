registration <- function(...) {
  base <- list(model_id = "m", long_name = "M", code_url = "https://x", model_type = "Empirical",
               dynamic = "No", ic_uncertainty = "No", has_parameters = "No",
               parameter_uncertainty = "No", assimilation = "No", assimilation_method = NA,
               uses_drivers = "No", driver_uncertainty = "No", process_uncertainty = "No",
               obs_uncertainty = "No", structural_uncertainty = "No",
               random_effects_uncertainty = "No")
  tibble::as_tibble(utils::modifyList(base, list(...)))
}

test_that("read_google_sheet() rejects addresses that are not sheets", {
  expect_error(read_google_sheet("https://example.org/data.csv"), "not a Google Sheets")
})

test_that("read_registered_models() keeps this challenge's real models, once each", {
  cols <- registration_columns()
  sheet <- tibble::tibble(
    a = c("m1", "m1", "m2", "example_m", "m3"),
    b = c("testcast", "testcast", "testcast", "testcast", "othercast"),
    c = c("First", "Second", "Two", "Ex", "Three"),
    d = c("desc", "desc", NA, NA, NA)
  )
  names(sheet) <- unname(cols[c("model_id", "project_id", "long_name", "description")])
  reg <- read_registered_models(list(project_id = "testcast"), sheet = sheet)
  expect_equal(reg$model_id, c("m1", "m2"))
  # Equally complete duplicates: the earliest registration wins.
  expect_equal(reg$long_name[reg$model_id == "m1"], "First")
  expect_true(all(names(cols) %in% names(reg)))
})

test_that("registration_columns() takes overrides from the config", {
  cfg <- list(catalog_config = list(registration_columns = list(long_name = "Model name")))
  expect_equal(registration_columns(cfg)[["long_name"]], "Model name")
})

test_that("model_metadata() records each source of uncertainty", {
  md <- model_metadata(registration(
    ic_uncertainty = "Yes and they were not estimated from data (e.g., assumed equilibrium)",
    has_parameters = "Yes and at least one is estimated from data",
    parameter_uncertainty = "Yes", assimilation = "Parameters",
    assimilation_method = "Kalman filter", uses_drivers = "Yes", driver_uncertainty = "No",
    process_uncertainty = "Yes",
    random_effects_uncertainty = "Yes and the uncertainty was estimated from data"
  ))
  u <- md$uncertainty
  expect_equal(u$initial_conditions[c("present", "data_driven")],
               list(present = TRUE, data_driven = FALSE))
  expect_equal(u$parameters$data_driven, TRUE)
  expect_equal(u$parameters$assimilation$type, "Kalman filter")
  expect_false(is.null(u$parameters$progagates))
  expect_equal(u$drivers, list(present = TRUE, data_driven = TRUE))
  expect_equal(u$process_error$data_driven, TRUE)
  expect_equal(u$random_effects$data_driven, TRUE)
  expect_equal(u$obs_error, list(present = FALSE))
  expect_equal(names(u), c("initial_conditions", "parameters", "drivers", "process_error",
                           "obs_error", "structural_error", "random_effects"))
})

test_that("model_metadata() handles dynamic models and missing answers", {
  dynamic <- model_metadata(registration(dynamic = "Yes"))
  expect_equal(dynamic$uncertainty$initial_conditions, list(present = TRUE, data_driven = FALSE))
  expect_null(dynamic$uncertainty$parameters)

  # A blank answer is "Unknown" rather than an error.
  partial <- model_metadata(registration(obs_uncertainty = NA))
  expect_equal(partial$uncertainty$obs_error, list(present = "Unknown"))

  unanswered <- model_metadata(registration(model_type = NA))
  expect_equal(unique(unlist(unanswered$uncertainty)), "Unknown")
  expect_true("process_error" %in% names(unanswered$uncertainty))
})
