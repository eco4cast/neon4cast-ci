# Reading, checking and standardising one submitted forecast file.
#
# These replace neon4cast::forecast_output_validator() and
# read4cast::read_forecast(). The validator read the NEON config from GitHub
# main on every call, so it could not check another challenge's rules, and the
# file was read twice (once to validate, once to process).

#' Columns a forecast is partitioned by in the forecasts bucket
#' @noRd
forecast_partitions <- c("project_id", "duration", "variable", "model_id",
                         "reference_date")

#' Parse a submission file name
#'
#' Submissions are named `<theme>-<YYYY-MM-DD>-<model_id>.<ext>`, e.g.
#' `aquatics-2024-03-01-climatology.csv.gz`. The model id may contain dashes.
#' Only the file name is used, so keys under a prefix (`folder/aquatics-...`)
#' parse the same way.
#'
#' @param path File paths or bucket keys.
#' @return A tibble with one row per path and columns `file`, `theme`,
#'   `reference_datetime` (POSIXct, UTC midnight), `model_id` and `extension`.
#'   All but `file` are `NA` when the name does not follow the pattern or the
#'   date is not a real date.
#' @export
#' @examples
#' parse_submission_filename("aquatics-2024-03-01-my-model.csv.gz")
parse_submission_filename <- function(path) {
  file <- basename(path)
  pattern <- "^([^-]+)-([0-9]{4}-[0-9]{2}-[0-9]{2})-(.+)\\.(csv\\.gz|csv|nc|parquet)$"
  parts <- regmatches(file, regexec(pattern, file))
  part <- function(i) {
    vapply(parts, function(p) if (length(p) == 0) NA_character_ else p[i], character(1))
  }
  date <- suppressWarnings(as.Date(part(3), format = "%Y-%m-%d"))
  ok <- !is.na(date)
  tibble::tibble(
    file = file,
    theme = ifelse(ok, part(2), NA_character_),
    reference_datetime = as.POSIXct(ifelse(ok, format(date), NA_character_),
                                    tz = "UTC"),
    model_id = ifelse(ok, part(4), NA_character_),
    extension = ifelse(ok, part(5), NA_character_)
  )
}

#' Read a submitted forecast CSV
#'
#' @param path Path to a `.csv` or `.csv.gz` file.
#' @return A tibble. Column types are guessed from up to a million rows so a
#'   long file of `NA`s does not make a column logical.
#' @export
read_submission <- function(path) {
  readr::read_csv(path, guess_max = 1e6, lazy = FALSE,
                  show_col_types = FALSE, progress = FALSE)
}

#' Check a forecast against the challenge's format rules
#'
#' The rules match those of `neon4cast::forecast_output_validator()` but take
#' the variables and durations from `cfg`, and two checks are added (marked
#' *new*).
#'
#' Errors (the file is rejected and archived unprocessed):
#' * no rows (*new*);
#' * more than one `model_id`, or a `model_id` that is `NA`;
#' * no `variable` or `prediction` column, or a `prediction` column that is not
#'   numeric (*new*);
#' * an `ensemble` column (it must be called `parameter`);
#' * no `family` or `parameter` column, more than one `family`, or more than 500
#'   ensemble members;
#' * no `datetime` column, or one that is not a date or date-time;
#' * neither a `reference_datetime` nor a `start_time` column.
#'
#' Warnings (the file is processed, and the problem is logged):
#' * no `model_id` column (it is taken from the file name);
#' * variables that are not in the challenge;
#' * no `site_id`, `duration` or `project_id` column, or a duration the
#'   challenge does not use;
#' * a `start_time` column instead of `reference_datetime` (the reference date
#'   is taken from the file name).
#'
#' Rows beyond a variable's `max_horizon` are not an error here.
#' [standardize_forecast()] drops them.
#'
#' @param fc Forecast from [read_submission()].
#' @inheritParams config_get
#' @return A tibble with columns `severity` (`"error"` or `"warning"`) and
#'   `message`, with one row per problem. No rows means the file is valid.
#' @export
validate_forecast <- function(fc, cfg) {
  problems <- character()
  severity <- character()
  add <- function(sev, msg) {
    severity <<- c(severity, sev)
    problems <<- c(problems, msg)
  }
  cols <- names(fc)
  vars <- target_variables(cfg)

  if (nrow(fc) == 0) add("error", "the file has no rows")

  if ("model_id" %in% cols) {
    ids <- unique(fc$model_id)
    if (length(ids) > 1) {
      add("error", "more than one model_id; each file must hold one model")
    } else if (length(ids) == 1 && is.na(ids)) {
      add("error", "model_id is NA")
    }
  } else {
    add("warning", "no model_id column; using the model_id in the file name")
  }

  if (!all(c("variable", "prediction") %in% cols)) {
    add("error", "missing the variable and/or prediction column")
  } else {
    if (!is.numeric(fc$prediction)) {
      add("error", "the prediction column is not numeric")
    }
    unknown <- setdiff(unique(fc$variable), vars$variable)
    if (length(unknown) > 0) {
      add("warning", paste("not challenge variables:", paste(unknown, collapse = ", ")))
    }
  }

  if ("ensemble" %in% cols) {
    add("error", "the ensemble column must be named parameter")
  } else if (!"family" %in% cols) {
    add("error", "missing the family column")
  } else if (!"parameter" %in% cols) {
    add("error", "missing the parameter column")
  } else {
    families <- unique(fc$family)
    if (length(families) > 1) {
      add("error", "more than one family; each file must use one")
    } else if (length(families) == 1 && families %in% c("ensemble", "sample")) {
      if (dplyr::n_distinct(fc$parameter) > 500) {
        add("error", "more than 500 ensemble members")
      }
    }
  }

  if (!"datetime" %in% cols) {
    add("error", "missing the datetime column")
  } else if (!inherits(fc$datetime, c("Date", "POSIXct"))) {
    add("error", "datetime must be YYYY-MM-DD or YYYY-MM-DD hh:mm:ss")
  }

  if (!"reference_datetime" %in% cols) {
    if ("start_time" %in% cols) {
      add("warning", paste("start_time should be named reference_datetime;",
                           "using the date in the file name"))
    } else {
      add("error", "missing the reference_datetime column")
    }
  }

  if (!"site_id" %in% cols) add("warning", "missing the site_id column")
  if (!"project_id" %in% cols) add("warning", "missing the project_id column")
  if (!"duration" %in% cols) {
    add("warning", "missing the duration column; using the variable's duration")
  } else {
    unknown <- setdiff(unique(fc$duration), c(vars$duration, "PT30"))
    if (length(unknown) > 0) {
      add("warning", paste("durations the challenge does not use:",
                           paste(unknown, collapse = ", ")))
    }
  }

  tibble::tibble(severity = severity, message = problems)
}

#' Put a valid forecast into the stored format
#'
#' * Adds `duration` when it is missing: the variable's duration in the config
#'   when it has exactly one, otherwise `P1D`. Fixes the old `PT30` spelling.
#' * Renames family `ensemble` to `sample`.
#' * Takes `model_id` and `reference_datetime` from the file name when the
#'   columns are missing (or `model_id` is `"null"`).
#' * Adds `pub_datetime`, `reference_date` and the challenge's `project_id`.
#' * Drops rows before the reference datetime, and rows beyond the variable's
#'   `max_horizon` (in days; 35 for variables not in the config).
#'
#' @param fc Forecast that passed [validate_forecast()].
#' @inheritParams config_get
#' @param file_info One row of [parse_submission_filename()] for the file.
#' @param pub_datetime When the forecast was published (processed).
#' @return The standardised forecast as a tibble.
#' @export
standardize_forecast <- function(fc, cfg, file_info, pub_datetime = Sys.time()) {
  vars <- target_variables(cfg)

  if (!"duration" %in% names(fc)) {
    one_duration <- vars |>
      dplyr::distinct(.data$variable, .data$duration) |>
      dplyr::filter(dplyr::n() == 1, .by = "variable")
    fc <- fc |>
      dplyr::left_join(one_duration, by = "variable") |>
      dplyr::mutate(duration = dplyr::coalesce(.data$duration, "P1D"))
  }
  if (!"model_id" %in% names(fc) || isTRUE(fc$model_id[1] == "null")) {
    fc$model_id <- file_info$model_id
  }
  if (!"reference_datetime" %in% names(fc)) {
    fc$reference_datetime <- file_info$reference_datetime
  }

  # Whole seconds, UTC, as the bundles have always stored it.
  pub_datetime <- lubridate::as_datetime(
    format(pub_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
  max_horizons <- vars |>
    dplyr::summarise(max_horizon = max(.data$max_horizon),
                     .by = c("variable", "duration"))

  n_before <- nrow(fc)
  fc <- fc |>
    # as.character() first: a column that is all NA is read as logical.
    dplyr::mutate(
      duration = as.character(.data$duration),
      family = as.character(.data$family),
      duration = dplyr::if_else(.data$duration == "PT30", "PT30M", .data$duration),
      family = dplyr::if_else(.data$family == "ensemble", "sample", .data$family),
      pub_datetime = pub_datetime,
      datetime = lubridate::as_datetime(.data$datetime),
      reference_datetime = lubridate::as_datetime(.data$reference_datetime),
      reference_date = lubridate::as_date(.data$reference_datetime),
      parameter = as.character(.data$parameter),
      project_id = cfg$project_id
    ) |>
    dplyr::filter(.data$datetime >= .data$reference_datetime) |>
    dplyr::left_join(max_horizons, by = c("variable", "duration")) |>
    # difftime needs explicit units: left to choose, it picks the unit of the
    # smallest gap, which once let every row through this filter.
    dplyr::mutate(
      horizon = as.numeric(difftime(.data$datetime, .data$reference_datetime,
                                    units = "days"))
    ) |>
    dplyr::filter(.data$horizon <= dplyr::coalesce(.data$max_horizon, 35)) |>
    dplyr::select(-"horizon", -"max_horizon")

  dropped <- n_before - nrow(fc)
  if (dropped > 0) {
    cli::cli_inform(
      "Dropped {dropped} row{?s} before the reference date or beyond the maximum horizon."
    )
  }
  fc
}

#' Summary statistics of a forecast
#'
#' Mean, median, standard deviation and 80% and 95% intervals for each forecast
#' row, as stored in the summaries bucket. Duplicate rows for the same ensemble
#' member are averaged first.
#'
#' @param fc Forecast from [standardize_forecast()].
#' @return A tibble of summaries with a `reference_date` column.
#' @export
summarize_submission <- function(fc) {
  rlang::check_installed("score4cast")
  keys <- c("site_id", "datetime", "reference_datetime", "family", "duration",
            "model_id", "parameter", "pub_datetime", "reference_date",
            "variable", "project_id")
  fc |>
    dplyr::summarise(prediction = mean(.data$prediction),
                     .by = dplyr::any_of(keys)) |>
    score4cast::summarize_forecast(extra_groups = c("duration", "project_id")) |>
    dplyr::mutate(reference_date = lubridate::as_date(.data$reference_datetime))
}

#' Write a forecast as partitioned parquet
#'
#' Writes under `dir`, partitioned by project, duration, variable, model and
#' reference date. Files are named `<file_id>_0.parquet`. Giving each
#' submission its own `file_id` stops two submissions for the same model and
#' date overwriting each other's file. Deriving it from the file's contents
#' makes reprocessing the same file replace its own output, not duplicate it.
#'
#' @param df Forecast or summaries.
#' @param dir Local output folder.
#' @param file_id File name stem, unique to the submission.
#' @return `dir`, invisibly.
#' @export
write_forecast_parquet <- function(df, dir, file_id) {
  # duckdb's partitioned write fails, with an empty error message, when the
  # parent folder does not exist.
  dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
  duckdbfs::write_dataset(
    df, dir,
    format = "parquet",
    partitioning = forecast_partitions,
    options = list("PER_THREAD_OUTPUT false",
                   paste0("FILENAME_PATTERN '", file_id, "_{i}'"))
  )
  invisible(dir)
}
