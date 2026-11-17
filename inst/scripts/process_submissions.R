# Process new forecast submissions.
#
# Downloads new files from the submissions bucket, checks them against the
# challenge's format rules, and writes valid forecasts (and their summaries) to
# the forecasts bucket as parquet. Every file handled is archived in
# <forecasts_bucket>/raw/ and recorded in the processed-submissions record. See
# ?challengeci::run_submissions for details.
#
# Run from the repository root:  Rscript inst/scripts/process_submissions.R
#
# Environment variables
#   OSN_KEY, OSN_SECRET            main store (names set in config `credentials`)
#   AWS_ACCESS_KEY_SUBMISSIONS,
#   AWS_SECRET_ACCESS_KEY_SUBMISSIONS   submissions store (listing only)
#   CHALLENGE_CONFIG               config file (default challenge_configuration.yaml)
#   DRY_RUN=true                   write to a local folder instead of S3, delete nothing

pkgload::load_all(quiet = TRUE)
minioclient::install_mc()
run_submissions(read_config())
