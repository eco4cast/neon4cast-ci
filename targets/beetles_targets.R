if (file.exists(".Renviron")) readRenviron(".Renviron")

library(neonstore)
library(tidyverse)
library(ISOweek)
library(minioclient)
library(neonUtilities)
source("targets/R/resolve_taxonomy.R")
source("targets/R/neon_os_table.R")

install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

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

targets2 <- targets |>
  mutate(datetime = lubridate::as_datetime(datetime),
         duration = "P1W",
         project_id = "neon4cast") |>
  select(project_id, site_id, datetime, duration, variable, observation)

write_csv(targets2, "beetles-targets.csv.gz")

mc_cp("beetles-targets.csv.gz", "osn/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1W/beetles-targets.csv.gz")

#mc_mirror(path.expand("~/beetles-data"), "osn/bio230014-bucket01/beetles-data/", overwrite = TRUE, remove = TRUE)

RCurl::getURL("https://hc-ping.com/ed35da4e-01d3-4750-ae5a-ad2f5dfa6e99")

targets2 |>
  filter(site_id == "OSBS") |>
  ggplot(aes(x = datetime, y = observation)) +
  geom_point() +
  facet_wrap(~variable)
