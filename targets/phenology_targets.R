# Phenology targets (daily gcc_90 and rcc_90) from the PhenoCam network.
#
# Run from the repository root:  Rscript targets/phenology_targets.R
#
# Environment variables
#   OSN_KEY, OSN_SECRET  main store, for the upload
#   DRY_RUN=true         write the targets to a local folder instead

pkgload::load_all(quiet = TRUE)
library(tidyverse)
source("targets/R/downloadPhenoCam.R")

cfg <- read_config()
minioclient::install_mc()

# Each phenology site's PhenoCam camera and region of interest.
sites <- read_sites(cfg, "Phenology")

message(paste0("Downloading and generating phenology targets ", Sys.time()))

all_data <- purrr::map(seq_len(nrow(sites)), function(i) {
  camera <- sites$phenocam_code[i]
  message(camera)
  url <- paste0("https://phenocam.nau.edu/data/archive/", camera, "/ROI/", camera, "_",
                sites$phenocam_roi[i], "_1day.csv")
  download.phenocam(URL = url) |>
    mutate(site_id = stringr::str_sub(camera, 10, 13), time = date) |>
    select(time, site_id, gcc_90, rcc_90) |>
    pivot_longer(-c("time", "site_id"), names_to = "variable", values_to = "observation")
}) |>
  list_rbind()

# Every site and variable gets a row for every day in the record, so days
# without an image are explicit NAs.
all_days <- expand_grid(
  site_id = sites$site_id,
  time = seq(min(all_data$time), max(all_data$time), by = "1 day"),
  variable = c("gcc_90", "rcc_90")
)

all_days |>
  left_join(all_data, by = c("time", "site_id", "variable")) |>
  rename(datetime = time) |>
  format_targets(cfg, duration = "P1D") |>
  check_targets(cfg, "Phenology") |>
  write_targets(cfg, "Phenology")
