# Shared setup for every dashboard page. Quarto renders each page with
# dashboard/ as the working directory. The data come from
# `Rscript inst/scripts/dashboard.R stage`, run from the repository root first.

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
pkgload::load_all("..", quiet = TRUE)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
source("R/plot-utils.R")

# CHALLENGE_CONFIG is relative to the repository root, like everywhere else.
config_file <- Sys.getenv("CHALLENGE_CONFIG", "challenge_configuration.yaml")
if (!file.exists(config_file)) config_file <- file.path("..", config_file)
cfg <- read_config(config_file, validate = FALSE)
settings <- dashboard_settings(cfg)

data_file <- function(name) {
  path <- file.path("data", name)
  if (!file.exists(path)) {
    stop("Missing ", path, ". Run `Rscript inst/scripts/dashboard.R stage` first.")
  }
  path
}

variables <- readr::read_csv(data_file("variables.csv"), show_col_types = FALSE)
variables$tab_title <- paste0(variables$group, ": ", gsub("_", " ", variables$long_name))

# Model ids not to plot for a variable.
excluded_models <- function(variable) {
  unique(unlist(settings$excluded_models[c("all", variable)]))
}
