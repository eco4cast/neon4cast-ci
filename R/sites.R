#' Read the challenge's site metadata
#'
#' Reads the site metadata file named by `sites$metadata_url` in the config.
#' If that is not set, it uses `catalog_config$site_metadata_url`. The file's
#' columns are renamed to the standard names in `sites$columns`, which by
#' default are `site_id`, `site_name`, `latitude` and `longitude`. Every other
#' column is kept.
#'
#' With `group`, only sites in that target group are returned. The metadata
#' file marks membership with a column holding `1` (or `TRUE`). That column is
#' named by the group's `sites_column` setting and defaults to the lower-case
#' group name, e.g. `aquatics` for `Aquatics`.
#'
#' @inheritParams group_config
#' @param group Optional target group name.
#' @return A tibble with one row per site.
#' @export
#' @examples
#' \dontrun{
#' cfg <- read_config()
#' read_sites(cfg, "Aquatics")
#' }
read_sites <- function(cfg, group = NULL) {
  source <- config_get(cfg, "sites", "metadata_url",
                       default = cfg$catalog_config$site_metadata_url)
  if (is.null(source)) {
    cli::cli_abort("Set {.field sites$metadata_url} in the challenge configuration.")
  }
  sites <- readr::read_csv(source, show_col_types = FALSE, progress = FALSE)

  columns <- unlist(config_get(cfg, "sites", "columns"))
  missing <- setdiff(columns, names(sites))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Site metadata {.path {source}} is missing column{?s} {.field {missing}}.",
      "i" = "Map the file's columns to standard names in {.field sites$columns}."
    ))
  }
  sites <- dplyr::rename(sites, dplyr::all_of(columns))

  if (is.null(group)) return(sites)

  column <- group_config(cfg, group)$sites_column %||% tolower(group)
  if (!column %in% names(sites)) {
    cli::cli_abort(c(
      "Site metadata has no {.field {column}} column marking {group} sites.",
      "i" = "Set {.field target_groups${group}$sites_column} in the config."
    ))
  }
  dplyr::filter(sites, .data[[column]] == 1)
}
