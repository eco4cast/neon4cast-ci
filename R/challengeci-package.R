#' challengeci: shared plumbing for forecasting-challenge CI
#'
#' The workflows in `.github/workflows/` run short entry-point scripts. Those
#' scripts load this package with `pkgload::load_all()` and call its
#' functions, so every challenge-specific value (buckets, endpoints, variables,
#' horizons, sites) comes from `challenge_configuration.yaml` instead of being
#' written into the code.
#'
#' Start with [read_config()]. Then use [target_variables()] for the variables a
#' challenge forecasts, [s3_connect()] and [bucket_uri()] for storage, and
#' [read_sites()] for site metadata.
#'
#' Set the environment variable `CHALLENGE_CONFIG` to use a different config
#' file. Set `DRY_RUN=true` to send writes to a local folder instead of S3 (see
#' [is_dry_run()]).
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data %||%
NULL
