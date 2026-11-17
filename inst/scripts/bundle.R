# Bundle newly processed forecasts and/or summaries.
#
# Appends each model's new parquet files to its single bundle file, then moves
# them to the archive folder. See ?challengeci::run_bundling for details.
#
# Run from the repository root:
#   Rscript inst/scripts/bundle.R            # forecasts, then summaries
#   Rscript inst/scripts/bundle.R summaries  # one kind only
#
# Environment variables
#   OSN_KEY, OSN_SECRET   main store (names set in config `credentials`)
#   CHALLENGE_CONFIG      config file (default challenge_configuration.yaml)
#   DRY_RUN=true          write to a local folder instead of S3, move nothing
#   VERBOSE=true          count rows in all bundles before and after (slow)

pkgload::load_all(quiet = TRUE)
minioclient::install_mc()
cfg <- read_config()

kinds <- commandArgs(trailingOnly = TRUE)
if (length(kinds) == 0) kinds <- c("forecasts", "summaries")

# Bundle every kind before failing, so a failure in one does not hold up the
# other.
ok <- vapply(kinds, function(kind) {
  tryCatch({
    run_bundling(cfg, kind)
    TRUE
  }, error = function(e) {
    message(conditionMessage(e))
    FALSE
  })
}, logical(1))
if (!all(ok)) stop("Bundling failed for: ", toString(kinds[!ok]))
