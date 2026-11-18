# Beetle abundance and richness targets (weekly) from NEON's ground beetle
# product, DP1.10022.001.
#
# Run from the repository root:  Rscript targets/beetles_targets.R
#
# Environment variables
#   NEON_TOKEN           NEON API token (optional; raises rate limits)
#   OSN_KEY, OSN_SECRET  main store, for the upload
#   DRY_RUN=true         write the targets to a local folder instead

if (file.exists(".Renviron")) readRenviron(".Renviron")

pkgload::load_all(quiet = TRUE)
library(tidyverse)
source("targets/R/resolve_taxonomy.R")
source("targets/R/neon_os_table.R")

cfg <- read_config()
minioclient::install_mc()

# Read with neon_os_table() rather than neonUtilities::datasetQuery(): in the CI
# container datasetQuery()'s DuckDB/httpfs reader fails on NEON's HTTPS URLs and
# returns NULL. See targets/R/neon_os_table.R.
sorting <- neon_os_table("DP1.10022.001", "bet_sorting") |>
  select(collectDate, siteID, taxonID, individualCount, subsampleID,
         scientificName, morphospeciesID, taxonRank, sampleType,
         nativeStatusCode, sampleCondition)

para <- neon_os_table("DP1.10022.001", "bet_parataxonomistID") |>
  select(subsampleID, individualID, scientificName, taxonRank, taxonID, morphospeciesID)

expert <- neon_os_table("DP1.10022.001", "bet_expertTaxonomistIDProcessed") |>
  select(-uid, -namedLocation, -domainID, -siteID, -collectDate, -plotID, -setDate)

field <- neon_os_table("DP1.10022.001", "bet_fielddata") |>
  select(collectDate, siteID, setDate)

#### Generate derived richness table  ####################


beetles <- resolve_taxonomy(sorting, para, expert) %>%
  mutate(iso_week = ISOweek::ISOweek(collectDate),
         time = ISOweek::ISOweek2date(paste0(iso_week, "-1"))) %>%
  as_tibble()

richness <- beetles %>%
  select(taxonID, siteID, collectDate, time) %>%
  distinct() %>%
  count(siteID, time) %>%
  rename(richness = n)  %>%
  ungroup()

#### Generate derived abundance table ####################

## Using 'field' instead of 'beetles' Does not reflect taxonomic corrections!
## Allows for some counts even when richness is NA

effort <- field %>%
  mutate(iso_week = ISOweek::ISOweek(collectDate),
         time = ISOweek::ISOweek2date(paste0(iso_week, "-1"))) %>%
  group_by(siteID, time) %>%
  summarise(trapnights = as.integer(sum(collectDate - setDate)),
            .groups = "drop")

counts <- beetles %>%
  mutate(iso_week = ISOweek::ISOweek(collectDate),
         time = ISOweek::ISOweek2date(paste0(iso_week, "-1"))) %>%
  group_by(siteID, time) %>%
  summarise(count = sum(as.numeric(individualCount), na.rm = TRUE),
            .groups = "drop")

abund <- counts %>%
  left_join(effort, by = join_by(siteID, time)) %>%
  arrange(time) %>%
  mutate(abundance = count / trapnights) %>%
  select(siteID, time, abundance) %>%
  ungroup()

targets_na <- full_join(abund, richness, by = join_by(siteID, time))

## site-dates that have sampling effort but no counts should be
## treated as explicit observation 0s

## FIXME some may have effort but no sorting due only to latency, should not be treated as zeros

targets <- effort %>%
  select(siteID, time) %>%
  left_join(targets_na, by = join_by(siteID, time)) %>%
  tidyr::replace_na(list(richness = 0L, abundance = 0)) |>
  pivot_longer(-c("time","siteID"), names_to = "variable", values_to = "observation") |>
  rename(site_id = siteID) |>
  mutate(iso_week = ISOweek::ISOweek(time)) |>
  select(time, site_id, variable, observation, iso_week)

targets <- targets |>
  rename(datetime = time)

targets |>
  format_targets(cfg, duration = "P1W") |>
  check_targets(cfg, "Beetles") |>
  write_targets(cfg, "Beetles")
