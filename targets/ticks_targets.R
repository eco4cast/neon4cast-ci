# Tick (Amblyomma americanum nymph) density targets (weekly) from NEON's tick
# drag product, DP1.10093.001.
#
# Run from the repository root:  Rscript targets/ticks_targets.R
#
# Environment variables
#   NEON_TOKEN           NEON API token (optional; raises rate limits)
#   OSN_KEY, OSN_SECRET  main store, for the upload
#   DRY_RUN=true         write the targets to a local folder instead

pkgload::load_all(quiet = TRUE)
library(tidyverse)
source("targets/R/resolve_taxonomy.R")
source("targets/R/neon_os_table.R")

cfg <- read_config()
minioclient::install_mc()

# select target species and life stage
target_species <- c("Amblyomma americanum") # NEON species name
target_lifestage <- "Nymph"

target_sites <- read_sites(cfg, "Ticks")$site_id

# Read with neon_os_table() rather than neonUtilities::datasetQuery(): in the CI
# container datasetQuery()'s DuckDB/httpfs reader fails on NEON's HTTPS URLs and
# returns NULL. See targets/R/neon_os_table.R.
tick_field_raw <- neon_os_table("DP1.10093.001", "tck_fielddata", package = "expanded") |>
  select(totalSampledArea, collectDate, namedLocation, nlcdClass, siteID)

tick_taxon_raw <- neon_os_table("DP1.10093.001", "tck_taxonomyProcessed", package = "expanded") |>
  select(sampleCondition, sexOrAge, collectDate, namedLocation,
         acceptedTaxonID, individualCount, scientificName, taxonRank)


# there are lots of reasons why sampling didn't occur (logistics, too wet, too cold, etc.)
# so, keep records when sampling occurred
tick_field <- tick_field_raw %>%
  filter(totalSampledArea > 0) %>%
  mutate(collectDate = lubridate::ymd_hms(as.character(collectDate),
                                          truncated = 3, quiet = TRUE),
         time = floor_date(collectDate, unit = "day")) %>%
  unite(namedLocation, time, col = "occasionID", sep = "_")

# combine adults into single category and make wide to get zero counts
tick_taxon_wide <- tick_taxon_raw %>%
  filter(sampleCondition == "OK") %>% # remove taxonomy samples with quality issues
  mutate(sexOrAge = if_else(sexOrAge == "Female" | sexOrAge == "Male",
                            "Adult",     # convert to Adult
                            sexOrAge),
         collectDate = lubridate::as_datetime(collectDate),
         time = floor_date(collectDate, unit = "day")) %>%
  unite(namedLocation, time, col = "occasionID", sep = "_") %>%
  pivot_wider(id_cols = occasionID, # make wide by species and life stage
              names_from = c(acceptedTaxonID, sexOrAge),
              values_from = individualCount,
              names_sep = "_",
              values_fn = {sum}, # duplicates occur because of Adults that where F/M - add them
              values_fill = 0)

# join taxonomy and field data
tick_joined <- left_join(tick_taxon_wide, tick_field, by = "occasionID") |>
  select(-any_of("NA_NA"))

# all the species column names
spp_cols <- tick_joined %>%
  select(contains("Larva"), contains("Nymph"), contains("Adult")) %>%
  colnames()

# get matching taxon ids
taxon_ids <- tick_taxon_raw %>%
  filter(!is.na(acceptedTaxonID)) %>%
  select(acceptedTaxonID, scientificName, taxonRank) %>%
  distinct()

# make longer
tick_long <- tick_joined %>%
  pivot_longer(cols = all_of(spp_cols),
               names_to = "taxonAge",
               values_to = "processedCount",
               values_drop_na = TRUE) %>%
  separate(col = taxonAge, into = c("acceptedTaxonID", "lifeStage"), sep = "_")

# add taxon ids
tick_long <- left_join(tick_long, taxon_ids, by = "acceptedTaxonID")

# standardize the data and subset to targets
tick_standard <- tick_long %>%
  filter(siteID %in% target_sites, # sites we want
         lifeStage == target_lifestage, # life stage we want
         scientificName %in% target_species,
         grepl("Forest", nlcdClass), # forest plots
         !is.na(collectDate)) %>%  # drop rows w/ unparseable dates (provisional data schema gaps)
  mutate(date = floor_date(collectDate, unit = "day"),
         date = ymd(date),
         year = year(date),
         iso_week = ISOweek::ISOweek(collectDate),
         time = ISOweek::ISOweek2date(paste0(iso_week, "-1"))) %>%
  select(time, processedCount, totalSampledArea, siteID, scientificName) %>%
  mutate(totalSampledArea = as.numeric(totalSampledArea)) %>%
  summarise(totalCount = sum(processedCount), # all counts in a week
            totalArea = sum(totalSampledArea),# total area surveyed in a week
            observation = totalCount / totalArea * 1600, .by = c(siteID, time, scientificName)) %>% # scale to the size of a plot
  mutate(iso_week = ISOweek::ISOweek(time)) %>%
  arrange(siteID, time) %>%
  select(time, iso_week, siteID, scientificName, observation)


tick_targets <- tick_standard %>%
  rename(site_id = siteID) |>
  mutate(variable = scientificName) |>
  mutate(variable = ifelse(variable == "Amblyomma americanum", "amblyomma_americanum", "ixodes_scapularis")) |>
  select(time, site_id, variable, observation, iso_week)



tick_targets |>
  rename(datetime = time) |>
  format_targets(cfg, duration = "P1W") |>
  check_targets(cfg, "Ticks") |>
  write_targets(cfg, "Ticks")
