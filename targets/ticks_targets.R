library(tidyverse) # for data wrangling and piping (dplyr probably ok)
library(lubridate) # for finding year from dates
library(neonstore)
library(minioclient)
library(neonUtilities)

install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))
#mc_mirror("osn/bio230014-bucket01/ticks-data/",  path.expand("~/ticks-data/"))

# select target species and life stage
target_species <- c("Amblyomma americanum") # NEON species name
target_lifestage <- "Nymph"

sites_df <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/main/neon4cast_field_site_metadata.csv", show_col_types = FALSE) |>
  dplyr::filter(ticks == 1)
target_sites <- sites_df %>% pull(field_site_id)


source("targets/R/resolve_taxonomy.R")

# pass <- TRUE
# iter <- 0
# while(pass & iter < 10){
#   iter <- iter + 1
#
#   df <-  neonstore:::neon_data(product = "DP1.10093.001",
#                                #start_date = paste0(curr_year, "-01-01"),
#                                #end_date = paste0(curr_year, "-12-31"),
#                                site = target_sites,
#                                type="expanded")
#
#   if(file.exists(path.expand("~/ticks-data/DP1.10093.001.csv"))){
#     full_df_old <- read_csv(path.expand("~/ticks-data/DP1.10093.001.csv"), show_col_types = FALSE)
#   }else{
#     full_df_old <- NULL
#   }
#
#   full_df <- bind_rows(full_df_old, df) %>%
#     distinct()
#
#   print(nrow(full_df))
#   print(nrow(full_df_old))
#   pass <- nrow(full_df) != nrow(full_df_old)
#
#   write_csv(full_df, path.expand("~/ticks-data/DP1.10093.001.csv"))
# }


tick_field_raw <- datasetQuery(dpID="DP1.10093.001",
                        package="expanded",
                        tabl="tck_fielddata",
                        release="current",
                        include.provisional = TRUE,
                        token=Sys.getenv("NEON_TOKEN")) |>
  select(totalSampledArea, collectDate, namedLocation, nlcdClass, siteID) |>
  collect()

tick_taxon_raw <- datasetQuery(dpID="DP1.10093.001",
                               package="expanded",
                               tabl="tck_taxonomyProcessed",
                               release="current",
                               include.provisional = TRUE,
                               token=Sys.getenv("NEON_TOKEN")) |>
  select(sampleCondition, sexOrAge, collectDate, namedLocation,
         acceptedTaxonID, individualCount, scientificName, taxonRank) |>
  collect()


# there are lots of reasons why sampling didn't occur (logistics, too wet, too cold, etc.)
# so, keep records when sampling occurred
tick_field <- tick_field_raw %>%
  filter(totalSampledArea > 0) %>%
  mutate(collectDate = lubridate::as_datetime(collectDate),
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



tick_targets <- tick_targets |>
  rename(datetime = time)

tick_targets2 <- tick_targets |>
  mutate(datetime = lubridate::as_datetime(datetime),
         duration = "P1W",
         project_id = "neon4cast") |>
  select(project_id, site_id, datetime, duration, variable, observation)


ggplot(tick_targets2, aes(x = datetime, y = observation, color = variable)) +
  geom_point() +
  facet_wrap(~site_id, scale = "free")

write_csv(tick_targets2, "ticks-targets.csv.gz")
message("Writing targets to S3")
mc_cp("ticks-targets.csv.gz", "osn/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1W/")
#message("Writing data catalog to S3")
#mc_mirror(path.expand("~/ticks-data"), "osn/bio230014-bucket01/ticks-data/", overwrite = TRUE, remove = TRUE)

RCurl::getURL("https://hc-ping.com/09c7ab10-eb4e-40ef-a029-7a4addc3295b")

