# Score forecasts against new observations.
#
# Finds forecast rows that have an observation but no score, scores them one
# model/variable at a time, and merges the scores into the bundled scores.
# See ?challengeci::run_scoring for details.
#
# Run from the repository root:  Rscript inst/scripts/score.R [since]
#   since   only consider forecasts and observations after this date
#           (YYYY-MM-DD); default six months ago. An earlier date scores older
#           forecasts, e.g. after a backlog or a rescore (docs/operations.md).
#
# Environment variables
#   OSN_KEY, OSN_SECRET            main store, for writing scores
#   EFI_NRP_KEY, EFI_NRP_SECRET    scratch store (names set in config `credentials`)
#   CHALLENGE_CONFIG               config file (default challenge_configuration.yaml)
#   DRY_RUN=true                   keep scratch and scores in a local folder

pkgload::load_all(quiet = TRUE)
minioclient::install_mc()
since <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(since)) {
  run_scoring(read_config())
} else {
  run_scoring(read_config(), since = as.Date(since))
}
