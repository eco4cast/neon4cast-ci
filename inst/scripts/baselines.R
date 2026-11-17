# Run baseline (null) model forecasts and submit them.
#
# Runs the baselines listed under each target group's `baselines` in the config
# and uploads each forecast to the submissions bucket, where the submissions
# stage processes it like any participant's. See ?challengeci::run_baselines.
#
# Run from the repository root:
#   Rscript inst/scripts/baselines.R daily              # every group's daily baselines
#   Rscript inst/scripts/baselines.R weekly Ticks       # one group's weekly baselines
#
# Environment variables
#   AWS_ACCESS_KEY_SUBMISSIONS,
#   AWS_SECRET_ACCESS_KEY_SUBMISSIONS   key allowed to upload to the submissions
#                                       bucket (names set in config `credentials`)
#   CHALLENGE_CONFIG                    config file (default challenge_configuration.yaml)
#   DRY_RUN=true                        write forecasts to a local folder instead

pkgload::load_all(quiet = TRUE)
minioclient::install_mc()
cfg <- read_config()

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage: Rscript inst/scripts/baselines.R <schedule> [group ...]")
schedule <- args[1]
groups <- if (length(args) > 1) args[-1] else names(cfg$target_groups)

# Run every group before failing, so one group's failure does not hold up the
# others.
ok <- vapply(groups, function(group) {
  tryCatch({
    run_baselines(cfg, group, schedule = schedule)
    TRUE
  }, error = function(e) {
    message(conditionMessage(e))
    FALSE
  })
}, logical(1))
if (!all(ok)) stop("Baselines failed for: ", toString(groups[!ok]))
