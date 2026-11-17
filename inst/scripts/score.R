# Score forecasts against new observations.
#
# Finds forecast rows from the last six months that have an observation but no
# score, scores them one model/variable at a time, and merges the scores into
# the bundled scores. See ?challengeci::run_scoring for details.
#
# Run from the repository root:  Rscript inst/scripts/score.R
#
# Environment variables
#   OSN_KEY, OSN_SECRET            main store, for writing scores
#   EFI_NRP_KEY, EFI_NRP_SECRET    scratch store (names set in config `credentials`)
#   CHALLENGE_CONFIG               config file (default challenge_configuration.yaml)
#   DRY_RUN=true                   keep scratch and scores in a local folder

pkgload::load_all(quiet = TRUE)
minioclient::install_mc()
run_scoring(read_config())
