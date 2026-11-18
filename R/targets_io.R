# The shared last steps of every targets script: format_targets() puts the
# targets in the standard format, check_targets() compares them with the
# published file, and write_targets() uploads them.
#
# Downloading and cleaning the data stays in each challenge's targets/*.R
# scripts, because it is specific to the data source.

#' Where a target group's targets file is stored
#'
#' The group's `targets_file` setting is a public URL on the main store's
#' endpoint. This returns the same object as a minio client path.
#'
#' @inheritParams group_config
#' @param alias Minio client alias of the main store.
#' @return A minio client path.
#' @export
targets_object <- function(cfg, group, alias = "main") {
  url <- group_config(cfg, group)$targets_file
  prefix <- paste0("https://", config_get(cfg, "endpoint"), "/")
  if (!rlang::is_string(url) || !startsWith(url, prefix)) {
    cli::cli_abort(
      "{.field target_groups${group}$targets_file} must be a URL starting {.url {prefix}}."
    )
  }
  paste0(alias, "/", substring(url, nchar(prefix) + 1))
}

#' Put targets in the standard format
#'
#' @param targets A data frame with `site_id`, `datetime`, `variable` and
#'   `observation`.
#' @inheritParams config_get
#' @param duration ISO 8601 duration of the targets, e.g. `"P1D"`.
#' @return A tibble with columns `project_id`, `site_id`, `datetime` (POSIXct,
#'   UTC), `duration`, `variable` and `observation`, sorted, with `NaN`
#'   observations as `NA`.
#' @export
format_targets <- function(targets, cfg, duration) {
  missing <- setdiff(c("site_id", "datetime", "variable", "observation"), names(targets))
  if (length(missing) > 0) cli::cli_abort("Targets are missing column{?s} {.field {missing}}.")
  targets |>
    dplyr::transmute(
      project_id = cfg$project_id,
      site_id = as.character(.data$site_id),
      datetime = lubridate::as_datetime(.data$datetime),
      duration = duration,
      variable = as.character(.data$variable),
      observation = dplyr::if_else(is.nan(.data$observation), NA_real_,
                                   as.numeric(.data$observation))
    ) |>
    dplyr::arrange(.data$variable, .data$site_id, .data$datetime)
}

#' Read the targets file currently published for a group
#'
#' @inheritParams group_config
#' @return The targets as a tibble, or `NULL` when no file has been published
#'   yet. Read errors are raised, never taken to mean "no file".
#' @export
read_published_targets <- function(cfg, group) {
  url <- group_config(cfg, group)$targets_file
  mc_connect(cfg, "main", alias = "main_public", anonymous = TRUE)
  if (length(minioclient::mc_ls(targets_object(cfg, group, alias = "main_public"))) == 0) {
    return(NULL)
  }
  readr::read_csv(url, guess_max = 1e6, show_col_types = FALSE, progress = FALSE)
}

#' Check new targets before they replace the published ones
#'
#' Stops, so nothing is uploaded, when the new targets:
#' * lack a standard column or contain duplicate site/datetime/variable rows;
#' * hold variables the group does not have in the config;
#' * have fewer than `min_fraction` of the published file's rows; or
#' * have lost sites or variables the published file had.
#'
#' A failed or partial download usually shows up as one of the last two, so
#' this check stops a bad run from overwriting good targets.
#'
#' @param targets New targets from [format_targets()].
#' @inheritParams group_config
#' @param published The currently published targets, or `NULL` if there are
#'   none. Defaults to [read_published_targets()].
#' @param min_fraction Smallest allowed size of the new targets relative to the
#'   published ones.
#' @return `targets`, invisibly, if they pass.
#' @export
check_targets <- function(targets, cfg, group, published = read_published_targets(cfg, group),
                          min_fraction = 0.95) {
  problems <- character()
  standard <- c("project_id", "site_id", "datetime", "duration", "variable", "observation")
  if (!identical(names(targets), standard)) {
    problems <- c(problems, "columns are not the standard ones; use format_targets()")
  }
  n_dup <- sum(duplicated(targets[c("site_id", "datetime", "variable")]))
  if (n_dup > 0) problems <- c(problems, paste(n_dup, "duplicated site/datetime/variable rows"))

  config_vars <- dplyr::filter(target_variables(cfg), .data$group == !!group)$variable
  extra <- setdiff(unique(targets$variable), config_vars)
  if (length(extra) > 0) {
    problems <- c(problems, paste("variables not in the config:", toString(extra)))
  }

  if (!is.null(published) && nrow(published) > 0) {
    n_old <- nrow(published)
    n_new <- nrow(targets)
    log_step("{group} targets: {n_old} rows published, {n_new} new ",
             "({sprintf('%+.1f%%', 100 * (n_new - n_old) / n_old)}), ",
             "latest {max(targets$datetime, na.rm = TRUE)}")
    if (n_new < min_fraction * n_old) {
      problems <- c(problems, paste0(n_new, " rows, fewer than ", 100 * min_fraction,
                                     "% of the ", n_old, " published"))
    }
    lost_sites <- setdiff(unique(published$site_id), unique(targets$site_id))
    if (length(lost_sites) > 0) {
      problems <- c(problems, paste("sites missing:", toString(lost_sites)))
    }
    lost_vars <- setdiff(unique(published$variable), unique(targets$variable))
    if (length(lost_vars) > 0) {
      problems <- c(problems, paste("variables missing:", toString(lost_vars)))
    }
  }

  if (length(problems) > 0) {
    cli::cli_abort(c("{group} targets failed their checks; not uploading:",
                     rlang::set_names(problems, "x")))
  }
  invisible(targets)
}

#' Upload a group's targets
#'
#' Writes `<group's targets file name>` and uploads it to the location in the
#' group's `targets_file` setting, or to [dry_run_dir()] in a dry run.
#'
#' @param targets Targets that passed [check_targets()].
#' @inheritParams group_config
#' @return The destination written, invisibly.
#' @export
write_targets <- function(targets, cfg, group) {
  dest <- targets_object(cfg, group)
  file <- file.path(tempfile("targets"), basename(dest))
  dir.create(dirname(file))
  on.exit(unlink(dirname(file), recursive = TRUE))
  readr::write_csv(targets, file)
  if (!is_dry_run()) mc_connect(cfg, "main")
  upload_file(file, dest)
}
