# Model registrations and variable descriptions, read from the challenge's
# Google Sheets, and the model metadata JSON built from the registrations.
#
# Both sheets are public, so they are read as CSV exports: no Google login and
# no googlesheets4/gsheet dependency.

#' Read a public Google Sheet
#'
#' @param url The sheet's address as shown in a browser
#'   (`https://docs.google.com/spreadsheets/d/<id>/edit...`).
#' @return The first tab as a tibble, with the sheet's column names.
#' @export
read_google_sheet <- function(url) {
  id <- regmatches(url, regexpr("(?<=/spreadsheets/d/)[^/]+", url, perl = TRUE))
  if (length(id) == 0) cli::cli_abort("{.url {url}} is not a Google Sheets address.")
  csv <- paste0("https://docs.google.com/spreadsheets/d/", id, "/export?format=csv")
  with_retry(readr::read_csv(csv, show_col_types = FALSE, progress = FALSE,
                             col_types = readr::cols(.default = "c")))
}

#' Column names in the model registration form
#'
#' Maps the short names used in this package to the questions in the EFI
#' model registration form. A challenge whose form words a question
#' differently overrides it in `catalog_config$registration_columns`, e.g.
#' `long_name: "Name of the model"`.
#'
#' @inheritParams config_get
#' @return A named character vector: short name = form question.
#' @export
registration_columns <- function(cfg = list()) {
  defaults <- c(
    model_id = "model_id",
    project_id = "What forecasting challenge are you registering for?",
    long_name = "Long name of the model",
    description = "Describe your modeling approach in your own words.",
    code_url = "Web link to model code",
    contact_name = "Contact name",
    public_contact = "Can we publicly list your name and email as part of the model metadata?",
    model_type = "Which category best matches your modeling approach?",
    dynamic = paste("Is your forecast model dynamic? (i.e. is tomorrow\u2019s forecast",
                    "dependent on today\u2019s forecast)?"),
    ic_uncertainty = "Do your forecasts include uncertainty from initial conditions?",
    has_parameters = "Does your model include parameters?",
    parameter_uncertainty = "Does your forecast include uncertainty from the model parameters?",
    assimilation = paste("Do you update your initial conditions or parameters between",
                         "forecast submissions using newly available data (i.e., data",
                         "assimilation)?"),
    assimilation_method = paste("What method did you use if you updated your initial",
                                "conditions or parameters using data assimilation?"),
    uses_drivers = "Does the forecast use drivers?",
    driver_uncertainty = paste("Does your forecast include uncertainty from drivers",
                               "(i.e., ensemble weather forecasts)?"),
    process_uncertainty = paste("Does your forecast include uncertainty from the model",
                                "(process uncertainty)?"),
    obs_uncertainty = "Does your forecast include uncertainty from measurement noise?",
    structural_uncertainty = "Does your forecast include uncertainty from using different models?",
    random_effects_uncertainty = paste("Does your forecast include uncertainty from parameter",
                                       "random effects?")
  )
  overrides <- unlist(cfg$catalog_config$registration_columns)
  defaults[names(overrides)] <- overrides
  defaults
}

#' Models registered for this challenge
#'
#' Reads the registration sheet (`model_metadata_gsheet`), keeps this
#' challenge's rows, drops example models (ids containing `example_model_id`,
#' default `"example"`) and keeps one row per model: the most complete one,
#' and the earliest of those if several are equally complete (later rows with
#' the same id are usually accidental re-registrations).
#'
#' @inheritParams config_get
#' @param sheet The registration sheet, if already read. `NULL` reads it.
#' @return A tibble with the short column names of [registration_columns()].
#'   Questions missing from the sheet are `NA` columns.
#' @export
read_registered_models <- function(cfg, sheet = NULL) {
  sheet <- sheet %||% read_google_sheet(config_get(cfg, "model_metadata_gsheet"))
  columns <- registration_columns(cfg)
  for (col in setdiff(columns, names(sheet))) sheet[[col]] <- NA_character_
  example <- cfg$example_model_id %||% "example"
  sheet |>
    dplyr::select(dplyr::all_of(columns)) |>
    dplyr::mutate(.filled = rowSums(!is.na(dplyr::pick(dplyr::everything()))),
                  .row = dplyr::row_number()) |>
    dplyr::filter(.data$project_id == cfg$project_id, !is.na(.data$model_id),
                  !grepl(example, .data$model_id, fixed = TRUE)) |>
    dplyr::arrange(dplyr::desc(.data$.filled), .data$.row) |>
    dplyr::distinct(.data$model_id, .keep_all = TRUE) |>
    dplyr::arrange(.data$model_id) |>
    dplyr::select(-".filled", -".row")
}

#' Long names and descriptions of the challenge's variables
#'
#' Reads the target metadata sheet (`target_metadata_gsheet`). The sheet has
#' one row per variable and duration.
#'
#' @inheritParams config_get
#' @return A tibble with columns `variable`, `duration`, `long_name` and
#'   `description`, plus the sheet's other columns (such as horizon and
#'   latency), one row per variable and duration.
#' @export
read_variable_metadata <- function(cfg) {
  columns <- c(long_name = "Variable name", variable = "\"official\" targets name",
               description = "Description", duration = "duration")
  overrides <- unlist(cfg$catalog_config$variable_metadata_columns)
  columns[names(overrides)] <- overrides
  sheet <- read_google_sheet(config_get(cfg, "target_metadata_gsheet"))
  missing <- setdiff(columns, names(sheet))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "The target metadata sheet has no column{?s} {.field {missing}}.",
      "i" = "Map its columns in {.field catalog_config$variable_metadata_columns}."
    ))
  }
  sheet |>
    dplyr::rename(dplyr::all_of(columns)) |>
    dplyr::filter(!is.na(.data$variable)) |>
    dplyr::distinct(.data$variable, .data$duration, .keep_all = TRUE)
}

#' Metadata record for one registered model
#'
#' Turns the uncertainty questions of a registration into the EFI model
#' metadata structure. Models registered without answers get `"Unknown"` for
#' every source of uncertainty, so every registered model has a record.
#'
#' The key `progagates` (sic) is kept because published records use it.
#'
#' @param reg One row of [read_registered_models()].
#' @return A nested list, written as JSON by [write_model_metadata()].
#' @export
model_metadata <- function(reg) {
  reg <- as.list(reg)
  answer <- function(field) tolower(trimws(reg[[field]] %||% NA_character_))
  is_yes <- function(field) answer(field) %in% "yes"
  propagates <- list(type = "Infer from family column in archived forecasts")
  # One source of uncertainty: present/data_driven from the answer, and
  # whether it is propagated. Answers "Yes", "Yes and ... estimated from data"
  # and "Yes and ... not estimated from data".
  source_from <- function(field, yes_data_driven = TRUE, if_no = list(present = FALSE)) {
    a <- answer(field)
    if (is.na(a)) return(list(present = "Unknown"))
    if (a == "no") return(if_no)
    if (startsWith(a, "yes")) {
      data_driven <- yes_data_driven
      if (grepl("estimated", a, fixed = TRUE)) {
        data_driven <- !grepl("not estimated", a, fixed = TRUE)
      }
      return(list(present = TRUE, data_driven = data_driven, progagates = propagates))
    }
    list(present = "Unknown")
  }

  md <- list(
    creator = list(individual_name = "Pending", electronicMailAddress = "Pending",
                   organizationName = "Pending"),
    model_id = reg$model_id,
    model_description = list(
      intellectualRights = "https://creativecommons.org/licenses/by/4.0/",
      name = reg$long_name,
      type = reg$model_type,
      repository = reg$code_url
    )
  )
  if (is.na(reg$model_type)) {
    unknown <- list(present = "Unknown")
    md$uncertainty <- list(initial_conditions = unknown, drivers = unknown,
                           process_error = unknown, obs_error = unknown,
                           structural_error = unknown, random_effects = unknown)
    return(md)
  }

  u <- list()
  # A dynamic model carries its initial conditions forward even when the
  # forecast does not sample their uncertainty.
  ic_if_no <- list(present = FALSE)
  if (is_yes("dynamic")) ic_if_no <- list(present = TRUE, data_driven = FALSE)
  u$initial_conditions <- source_from("ic_uncertainty", if_no = ic_if_no)
  assimilated <- answer("assimilation")
  if (assimilated %in% c("initial conditions", "both initial conditions and parameters")) {
    u$initial_conditions$assimilation <- list(type = reg$assimilation_method)
  }

  has_parameters <- answer("has_parameters")
  if (!is.na(has_parameters) && startsWith(has_parameters, "yes")) {
    u$parameters <- list(
      present = TRUE,
      data_driven = grepl("at least one is estimated", has_parameters, fixed = TRUE)
    )
    if (is_yes("parameter_uncertainty")) u$parameters$progagates <- propagates
    if (assimilated %in% c("parameters", "parameter", "both initial conditions and parameters")) {
      u$parameters$assimilation <- list(type = reg$assimilation_method)
    }
  }

  # Drivers without driver uncertainty are still data.
  drivers_if_no <- list(present = FALSE)
  if (is_yes("uses_drivers")) drivers_if_no <- list(present = TRUE, data_driven = TRUE)
  u$drivers <- source_from("driver_uncertainty", if_no = drivers_if_no)
  u$process_error <- source_from("process_uncertainty")
  u$obs_error <- source_from("obs_uncertainty")
  u$structural_error <- source_from("structural_uncertainty", yes_data_driven = FALSE)
  u$random_effects <- source_from("random_effects_uncertainty")
  md$uncertainty <- u
  md
}

#' Publish a metadata record for every registered model
#'
#' Writes `<model_id>.json` for each model and mirrors them to the
#' `model_metadata_bucket`. The mirror only uploads files that changed.
#'
#' The bucket is keyed by model id alone and may be shared with other
#' challenges, so a model registered without answers (an all-`"Unknown"`
#' record) is only written when no record exists yet: it never replaces a
#' record another challenge wrote from a complete registration.
#'
#' @inheritParams config_get
#' @param registered Output of [read_registered_models()].
#' @return The number of records written, invisibly.
#' @export
write_model_metadata <- function(cfg, registered = read_registered_models(cfg)) {
  mc_connect(cfg, "main", anonymous = !has_credentials(cfg, "main"))
  dest <- bucket_uri(cfg, "model_metadata_bucket", scheme = "mc")
  existing <- sub("\\.json$", "", remote_files(dest)$key)
  keep <- !is.na(registered$model_type) | !registered$model_id %in% existing
  registered <- registered[keep, ]

  dir <- tempfile("model_metadata")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  purrr::walk(seq_len(nrow(registered)), function(i) {
    reg <- registered[i, ]
    # No auto_unbox: published records hold every value in an array.
    jsonlite::write_json(model_metadata(reg), file.path(dir, paste0(reg$model_id, ".json")),
                         pretty = TRUE)
  })
  upload_dir(dir, dest)
  log_step("Model metadata: {nrow(registered)} records written, ",
           "{sum(!keep)} unanswered registrations left as they are")
  invisible(nrow(registered))
}
