# Reading, validating and querying challenge_configuration.yaml.
#
# The config file is also read by tools outside this repo (for example the
# neon4cast package reads it from GitHub main), so new settings are always
# added as new keys. Existing keys keep their names and meanings.

#' Path to the challenge configuration file
#'
#' Uses the environment variable `CHALLENGE_CONFIG` when it is set and
#' `challenge_configuration.yaml` otherwise. The default path is relative to
#' the working directory, which in CI is the repository root.
#'
#' @return A file path (character scalar).
#' @export
#' @examples
#' config_path()
config_path <- function() {
  Sys.getenv("CHALLENGE_CONFIG", "challenge_configuration.yaml")
}

#' Read the challenge configuration
#'
#' Reads the YAML file, adds defaults for optional settings (see
#' [config_defaults()]) and, by default, checks it with [validate_config()].
#'
#' @param path Path to the config file. Defaults to [config_path()].
#' @param validate If `TRUE` (the default), stop when the config has errors
#'   and warn about inconsistencies.
#' @return The configuration as a nested list.
#' @export
#' @examples
#' \dontrun{
#' cfg <- read_config()
#' cfg$project_id
#' }
read_config <- function(path = config_path(), validate = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Config file {.path {path}} does not exist.",
      "i" = "Run from the repository root or set {.envvar CHALLENGE_CONFIG}."
    ))
  }
  cfg <- utils::modifyList(config_defaults(), yaml::read_yaml(path))
  # Defaults that depend on other settings.
  cfg$summaries_bucket <- cfg$summaries_bucket %||% cfg$forecasts_bucket
  if (validate) validate_config(cfg)
  cfg
}

#' Defaults for optional config settings
#'
#' These apply only where `challenge_configuration.yaml` leaves a setting out.
#' Values in the file always win.
#'
#' * `credentials`: for each store, the *names* of the environment variables
#'   holding its access key and secret. These are the GitHub secret names the
#'   workflows already use. The secrets themselves never go in the config.
#' * `sites$columns`: how columns in the site metadata file map to the
#'   standard names used by [read_sites()]. The names are standard, the values
#'   are the file's columns. The defaults match the NEON site metadata file.
#'
#' @return A nested list with the same shape as the config.
#' @export
config_defaults <- function() {
  list(
    credentials = list(
      main = list(key_env = "OSN_KEY", secret_env = "OSN_SECRET"),
      submissions = list(key_env = "AWS_ACCESS_KEY_SUBMISSIONS",
                         secret_env = "AWS_SECRET_ACCESS_KEY_SUBMISSIONS"),
      scratch = list(key_env = "EFI_NRP_KEY", secret_env = "EFI_NRP_SECRET")
    ),
    sites = list(
      columns = list(site_id = "field_site_id",
                     site_name = "field_site_name",
                     latitude = "latitude",
                     longitude = "longitude")
    )
  )
}

#' Get a config value, with a clear error if it is missing
#'
#' @param cfg Configuration from [read_config()].
#' @param ... Keys giving the path to the value, e.g.
#'   `"catalog_config", "catalog_path"`.
#' @param default Value to return when the key is missing. With no default, a
#'   missing key is an error that names the key.
#' @return The value at that path.
#' @export
#' @examples
#' cfg <- list(scratch = list(bucket = "efi-scores/tmp"))
#' config_get(cfg, "scratch", "bucket")
#' config_get(cfg, "scratch", "endpoint", default = NULL)
config_get <- function(cfg, ..., default) {
  keys <- c(...)
  value <- purrr::pluck(cfg, !!!keys)
  if (is.null(value)) {
    if (!missing(default)) return(default)
    cli::cli_abort(
      "Setting {.field {paste(keys, collapse = '$')}} is missing from the challenge configuration."
    )
  }
  value
}

#' Settings for one target group
#'
#' @inheritParams config_get
#' @param group Name of a target group, e.g. `"Aquatics"`.
#' @return The group's settings as a list.
#' @export
group_config <- function(cfg, group) {
  groups <- names(cfg$target_groups)
  if (!rlang::is_string(group) || !group %in% groups) {
    cli::cli_abort(c(
      "{.val {group}} is not a target group.",
      "i" = "Target groups in the config: {.val {groups}}."
    ))
  }
  cfg$target_groups[[group]]
}

#' Every variable the challenge forecasts, as a tidy table
#'
#' Each target group lists `variable`, `duration` and `max_horizon` as parallel
#' arrays. This turns them into one row per variable and duration, and adds the
#' DOI and citation from the group's `group_vars` entry when there is one.
#'
#' @inheritParams config_get
#' @return A tibble with columns `group`, `variable`, `duration`,
#'   `max_horizon` (in days), `targets_file`, `var_doi` and `var_citation`.
#' @export
#' @examples
#' \dontrun{
#' target_variables(read_config())
#' }
target_variables <- function(cfg) {
  # `settings`, not `group`: tibble() would mask it with the `group` column.
  purrr::imap(cfg$target_groups, function(settings, group_name) {
    variables <- as.character(unlist(settings$variable))
    var_info <- function(field) {
      purrr::map_chr(variables, function(v) {
        as.character(purrr::pluck(settings, "group_vars", v, field, .default = NA))
      })
    }
    tibble::tibble(
      group = group_name,
      variable = variables,
      duration = as.character(unlist(settings$duration)),
      max_horizon = as.numeric(unlist(settings$max_horizon)),
      targets_file = settings$targets_file %||% NA_character_,
      var_doi = var_info("var_doi"),
      var_citation = var_info("var_citation")
    )
  }) |>
    purrr::list_rbind()
}

#' Check a challenge configuration
#'
#' `config_problems()` lists every problem it finds. `validate_config()` stops
#' if any of them are errors and warns about the rest.
#'
#' Errors (the CI cannot run correctly):
#' * a required setting is missing or is not a single string;
#' * there are no target groups;
#' * a group's `variable`, `duration` and `max_horizon` arrays differ in length;
#' * a duration is not an ISO 8601 duration such as `P1D`, `P1W` or `PT30M`;
#' * a `max_horizon` is not a positive number;
#' * the same variable and duration appear twice, so its horizon is ambiguous.
#'
#' Warnings (probably a mistake, but the CI still runs):
#' * a variable has no `group_vars` entry, or the `group_vars` entry gives a
#'   different `duration` or `max_horizon` from the group's arrays;
#' * a `group_vars` entry names a variable that is not in the group.
#'
#' @inheritParams config_get
#' @return `config_problems()`: a tibble with columns `severity` (`"error"` or
#'   `"warning"`), `key` and `message`, with one row per problem.
#'   `validate_config()`: `cfg`, invisibly.
#' @export
validate_config <- function(cfg) {
  problems <- config_problems(cfg)
  warnings <- problems[problems$severity == "warning", ]
  errors <- problems[problems$severity == "error", ]
  if (nrow(warnings) > 0) {
    cli::cli_warn(c(
      "The challenge configuration has {nrow(warnings)} inconsistenc{?y/ies}:",
      rlang::set_names(paste0(warnings$key, ": ", warnings$message), "!")
    ))
  }
  if (nrow(errors) > 0) {
    cli::cli_abort(c(
      "The challenge configuration has {nrow(errors)} error{?s}:",
      rlang::set_names(paste0(errors$key, ": ", errors$message), "x")
    ))
  }
  invisible(cfg)
}

#' @rdname validate_config
#' @export
config_problems <- function(cfg) {
  problems <- list()
  add <- function(severity, key, message) {
    problems[[length(problems) + 1]] <<- tibble::tibble(
      severity = severity, key = key, message = message
    )
  }

  required <- c("project_id", "endpoint", "forecasts_bucket",
                "scores_bucket", "targets_bucket")
  for (key in required) {
    if (!rlang::is_string(cfg[[key]]) || !nzchar(cfg[[key]])) {
      add("error", key, "required, and must be a single string")
    }
  }
  site_url <- cfg$sites$metadata_url %||% cfg$catalog_config$site_metadata_url
  if (!rlang::is_string(site_url)) {
    add("error", "catalog_config$site_metadata_url",
        "required (or sites$metadata_url), and must be a single string")
  }

  groups <- cfg$target_groups
  if (length(groups) == 0) {
    add("error", "target_groups", "at least one target group is required")
  }

  for (group_name in names(groups)) {
    group <- groups[[group_name]]
    key <- paste0("target_groups$", group_name)
    variables <- as.character(unlist(group$variable))
    durations <- as.character(unlist(group$duration))
    horizons <- unlist(group$max_horizon)

    if (length(variables) == 0) {
      add("error", key, "`variable` is missing or empty")
      next
    }
    lengths_differ <- length(durations) != length(variables) ||
      length(horizons) != length(variables)
    if (lengths_differ) {
      add("error", key, paste0(
        "`variable`, `duration` and `max_horizon` must have the same length ",
        "(", length(variables), ", ", length(durations), ", ",
        length(horizons), ")"
      ))
      next
    }
    bad_duration <- durations[!is_iso_duration(durations)]
    if (length(bad_duration) > 0) {
      add("error", paste0(key, "$duration"), paste0(
        "not an ISO 8601 duration: ", paste(bad_duration, collapse = ", ")
      ))
    }
    if (!is.numeric(horizons) || any(is.na(horizons) | horizons <= 0)) {
      add("error", paste0(key, "$max_horizon"),
          "every value must be a positive number of days")
    }

    # group_vars repeats per-variable settings; flag where the copies disagree.
    group_vars <- group$group_vars
    for (i in seq_along(variables)) {
      v <- variables[i]
      v_key <- paste0(key, "$group_vars$", v)
      info <- group_vars[[v]]
      if (is.null(info)) {
        add("warning", v_key, "no entry for this variable")
        next
      }
      if (!is.null(info$duration) && !durations[i] %in% unlist(info$duration)) {
        add("warning", v_key, paste0(
          "duration ", paste(unlist(info$duration), collapse = ", "),
          " differs from the group's ", durations[i]
        ))
      }
      if (is.null(info$max_horizon)) {
        add("warning", v_key, "no max_horizon")
      } else if (!horizons[i] %in% unlist(info$max_horizon)) {
        add("warning", v_key, paste0(
          "max_horizon ", paste(unlist(info$max_horizon), collapse = ", "),
          " differs from the group's ", horizons[i]
        ))
      }
    }
    for (extra in setdiff(names(group_vars), variables)) {
      add("warning", paste0(key, "$group_vars$", extra),
          "names a variable that is not in the group's `variable` list")
    }
  }

  if (length(groups) > 0) {
    pairs <- purrr::map(groups, function(settings) {
      variables <- as.character(unlist(settings$variable))
      durations <- as.character(unlist(settings$duration))
      tibble::tibble(variable = variables,
                     duration = durations[seq_along(variables)])
    }) |>
      purrr::list_rbind()
    dupes <- pairs[duplicated(pairs), ]
    for (i in seq_len(nrow(dupes))) {
      add("error", "target_groups", paste0(
        "variable ", dupes$variable[i], " with duration ", dupes$duration[i],
        " appears more than once"
      ))
    }
  }

  if (length(problems) == 0) {
    return(tibble::tibble(severity = character(), key = character(),
                          message = character()))
  }
  purrr::list_rbind(problems)
}

#' Is a string an ISO 8601 duration?
#'
#' Accepts the single-unit durations the challenges use, such as `P1D`,
#' `P1W`, `P1M`, `PT1H` and `PT30M`.
#'
#' @param x Character vector.
#' @return Logical vector the same length as `x`.
#' @keywords internal
#' @noRd
is_iso_duration <- function(x) {
  grepl("^P([0-9]+[YMWD]|T[0-9]+[HMS])$", x)
}
