# Download NOAA GEFS forecasts (stage 1 and its ensemble statistics) for days
# not yet downloaded, then build hourly stage 2 for recent days.
# See ?challengeci::download_gefs and ?challengeci::update_stage2.
#
# Run from the repository root:  Rscript inst/scripts/drivers_stage1_2.R
#
# Environment variables
#   OSN_KEY, OSN_SECRET   main store (the NOAA bucket uses its credentials)
#   CHALLENGE_CONFIG      config file (default challenge_configuration.yaml)
#   DRY_RUN=true          write to a local folder instead of S3

pkgload::load_all(quiet = TRUE)
cfg <- read_config()

gdalcubes::gdalcubes_options(parallel = 2 * parallel::detectCores())
download_gefs(cfg, "stage1-stats")
download_gefs(cfg, "stage1")
update_stage2(cfg)
