# Read the minimal test config, with the site metadata path made absolute so
# tests do not depend on the working directory.
test_config <- function() {
  cfg <- read_config(test_path("fixtures", "minimal_config.yaml"))
  cfg$sites$metadata_url <- test_path("fixtures", "sites.csv")
  cfg
}

# Write a modified copy of the test config and return its path.
write_config <- function(cfg) {
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
  yaml::write_yaml(cfg, path)
  path
}

# A small valid forecast in the submitted format.
example_forecast <- function(n_days = 3, family = "normal") {
  ref <- as.Date("2024-03-01")
  grid <- expand.grid(
    site_id = c("AAAA", "BBBB"),
    datetime = ref + 0:(n_days - 1),
    parameter = if (family == "normal") c("mu", "sigma") else as.character(1:3),
    stringsAsFactors = FALSE
  )
  tibble::as_tibble(grid) |>
    dplyr::mutate(
      reference_datetime = as.POSIXct(ref, tz = "UTC"),
      datetime = as.POSIXct(datetime, tz = "UTC"),
      model_id = "my-model",
      variable = "temperature",
      family = family,
      duration = "P1D",
      project_id = "testcast",
      prediction = dplyr::if_else(parameter == "sigma", 1, 10)
    )
}

# A parsed file name to go with example_forecast().
example_file_info <- function() {
  parse_submission_filename("lakes-2024-03-01-my-model.csv.gz")
}
