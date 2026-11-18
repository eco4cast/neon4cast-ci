# Download NOAA GEFS pseudo-observations for days not yet downloaded, then
# extend each site's hourly stage 3 series.
# See ?challengeci::download_gefs and ?challengeci::update_stage3.
#
# Run from the repository root:
#   Rscript inst/scripts/drivers_stage3.R            # daily update
#   Rscript inst/scripts/drivers_stage3.R rebuild    # rebuild every site from all of pseudo
#
# Environment variables
#   OSN_KEY, OSN_SECRET   main store (the NOAA bucket uses its credentials)
#   CHALLENGE_CONFIG      config file (default challenge_configuration.yaml)
#   DRY_RUN=true          write to a local folder instead of S3

pkgload::load_all(quiet = TRUE)
cfg <- read_config()
rebuild <- identical(commandArgs(trailingOnly = TRUE), "rebuild")

gdalcubes::gdalcubes_options(parallel = 20)
if (!rebuild) download_gefs(cfg, "pseudo")
update_stage3(cfg, overlap_days = if (rebuild) Inf else 3)
