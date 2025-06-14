library(tidyverse) # for data wrangling and piping (dplyr probably ok)
library(lubridate) # for finding year from dates
library(neonstore)

# select target species and life stage
target_species <- c("Amblyomma americanum") # NEON species name
target_lifestage <- "Nymph"

sites_df <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |>
  dplyr::filter(ticks == 1)
target_sites <- sites_df %>% pull(field_site_id)



# get data from neon
#product <- "DP1.10093.001"
#neon_download(product = product,
#              site = target.sites)


df <-  neonstore:::neon_data(product = "DP1.10093.001",
                             #start_date = "2023-06-01",
                             #end_date = "2023-08-01",
                             site = target_sites,
                             type="expanded")

fielddata_urls <- df |>
  dplyr::filter(grepl("tck_fielddata", name)) |>
  pull(url)

taxonomyProcessed_urls <- df |>
  dplyr::filter(grepl("tck_taxonomyProcessed", name)) |>
  pull(url)

tick_field_raw <- duckdbfs::open_dataset(fielddata_urls, format="csv") |>
  select(totalSampledArea, collectDate, namedLocation, nlcdClass, siteID) |>
  collect()

tick_taxon_raw <- duckdbfs::open_dataset(taxonomyProcessed_urls, format="csv") |>
  select(sampleCondition, sexOrAge, collectDate, namedLocation,
         acceptedTaxonID, individualCount, scientificName, taxonRank) |>
  collect()

# there are lots of reasons why sampling didn't occur (logistics, too wet, too cold, etc.)
# so, keep records when sampling occurred
tick_field <- tick_field_raw %>%
  filter(totalSampledArea > 0) %>%
  mutate(collectDate = lubridate::fast_strptime(collectDate, "%Y-%m-%dT%H:%MZ"),
         time = floor_date(collectDate, unit = "day")) %>%
  unite(namedLocation, time, col = "occasionID", sep = "_")

# combine adults into single category and make wide to get zero counts
tick_taxon_wide <- tick_taxon_raw %>%
  filter(sampleCondition == "OK") %>% # remove taxonomy samples with quality issues
  mutate(sexOrAge = if_else(sexOrAge == "Female" | sexOrAge == "Male",
                            "Adult",     # convert to Adult
                            sexOrAge),
         collectDate = lubridate::fast_strptime(collectDate, "%Y-%m-%dT%H:%MZ"),
         time = floor_date(collectDate, unit = "day")) %>%
  unite(namedLocation, time, col = "occasionID", sep = "_") %>%
  pivot_wider(id_cols = occasionID, # make wide by species and life stage
              names_from = c(acceptedTaxonID, sexOrAge),
              values_from = individualCount,
              names_sep = "_",
              values_fn = {sum}, # duplicates occur because of Adults that where F/M - add them
              values_fill = 0)

# join taxonomy and field data
tick_joined <- left_join(tick_taxon_wide, tick_field, by = "occasionID") %>%
  select(-NA_NA)

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
         #scientificName %in% target.species, # species we want
         grepl("Forest", nlcdClass)) %>%  # forest plots
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
  #filter(time < challenge.time) |>
  rename(site_id = siteID) |>
  mutate(variable = scientificName) |>
  mutate(variable = ifelse(variable == "Amblyomma americanum", "amblyomma_americanum", "ixodes_scapularis")) |>
  select(time, site_id, variable, observation, iso_week)

ggplot(tick_targets, aes(x = time, y = observation, color = variable)) +
  geom_line() +
  facet_wrap(~site_id, scale = "free")

tick_targets <- tick_targets |>
  rename(datetime = time)

#s3 <- arrow::s3_bucket("neon4cast-targets/ticks",
#                       endpoint_override = "data.ecoforecast.org",
#                       access_key = Sys.getenv("AWS_ACCESS_KEY_SUBMISSIONS"),
#                       secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY_SUBMISSIONS"))
#
#arrow::write_csv_arrow(tick_targets, sink = s3$path("ticks-targets.csv.gz"))

tick_targets2 <- tick_targets |>
  mutate(datetime = lubridate::as_datetime(datetime),
         duration = "P1W",
         project_id = "neon4cast") |>
  select(project_id, site_id, datetime, duration, variable, observation)

s3 <- arrow::s3_bucket("bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1W",
                       endpoint_override = "sdsc.osn.xsede.org",
                       access_key = Sys.getenv("OSN_KEY"),
                       secret_key = Sys.getenv("OSN_SECRET"))

arrow::write_csv_arrow(tick_targets2, sink = s3$path("ticks-targets.csv.gz"))

RCurl::getURL("https://hc-ping.com/09c7ab10-eb4e-40ef-a029-7a4addc3295b")

