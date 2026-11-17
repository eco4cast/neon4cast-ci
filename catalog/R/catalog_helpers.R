# Helpers shared by the forecast, scores and summaries catalog scripts in
# catalog/build_catalog_scripts/.

#' Look up a variable's long name in the target metadata sheet
#'
#' Finds the row for one variable and duration, matching by name rather than
#' by position. The sheet and the config list variables in different orders,
#' and the sheet has one row per duration, so pairing them by index can attach
#' one variable's long name to another.
#'
#' @param variable_gsheet Target metadata sheet (`config$target_metadata_gsheet`).
#'   Its first column holds the long name.
#' @param var_name Variable name as used in the config and forecasts, e.g. "nee".
#' @param duration_name ISO 8601 duration, e.g. "P1D".
#' @return The long name, e.g. "Net_ecosystem_exchange". If the sheet has no
#'   row for the variable and duration, `var_name` with a warning.
lookup_var_name_full <- function(variable_gsheet, var_name, duration_name) {
  var_name_full <- variable_gsheet |>
    dplyr::filter(.data[['"official" targets name']] == var_name,
                  .data$duration == duration_name) |>
    dplyr::pull(1)

  if (length(var_name_full) == 0) {
    warning("No row for ", var_name, " (", duration_name, ") in the target ",
            "metadata sheet; using '", var_name, "' as its long name.",
            call. = FALSE)
    return(var_name)
  }
  var_name_full[1]
}
