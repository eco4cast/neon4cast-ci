# Build the STAC catalog and the model metadata records.
#
# Writes the catalog JSON under ../<catalog_config$catalog_path> (the catalog
# repository, cloned next to this one by the workflow, which then commits it)
# and uploads one metadata JSON per registered model to model_metadata_bucket.
# See ?challengeci::run_catalog.
#
# Run from the repository root:  Rscript inst/scripts/catalog.R [part ...]
#   part   any of metadata, catalog, forecasts, summaries, scores, noaa,
#          sites, targets (default: all)
#
# Environment variables
#   OSN_KEY, OSN_SECRET   main store, for uploading model metadata
#   CHALLENGE_CONFIG      config file (default challenge_configuration.yaml)
#   DRY_RUN=true          write the catalog and metadata under DRY_RUN_DIR

pkgload::load_all(quiet = TRUE)
minioclient::install_mc()
parts <- commandArgs(trailingOnly = TRUE)
cfg <- read_config()
if (length(parts) == 0) run_catalog(cfg) else run_catalog(cfg, parts)
