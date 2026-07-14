#renv::restore()
print(paste0("Running Creating Terrestrial Targets at ", Sys.time()))


print(sessionInfo())

#install.versions(c('arrow'), c('20.0.0'))
#install.packages("arrow", version='20.0.0')
#remotes::install_version('arrow', version = '20.0.0')

#Sys.setenv("NEONSTORE_HOME" = "/home/rstudio/data/neonstore")

Sys.setenv("NEONSTORE_HOME" = "/home/rstudio/data/neonstore_temp")

non_store_dir <- "/home/rstudio/data/neon_flux_data"
use_5day_data <- TRUE

library(neonUtilities)
library(neonstore)
library(tidyverse)
library(lubridate)
library(contentid)
library(ncdf4)
library(arrow)

library(minioclient)

print(sessioninfo::package_info())

install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

mc_mirror("osn/bio230014-bucket01/neonstore/db", neonstore::neon_db_dir())
mc_mirror("osn/bio230014-bucket01/flux_staging/neonstore_temp", neonstore::neon_dir())


sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/main/neon4cast_field_site_metadata.csv", show_col_types = FALSE) |>
  dplyr::filter(terrestrial == 1)

site_names <- sites$field_site_id

s3 <- arrow::s3_bucket("bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D",
                       endpoint_override = "sdsc.osn.xsede.org",
                       access_key = Sys.getenv("OSN_KEY"),
                       secret_key = Sys.getenv("OSN_SECRET"))


s3_neon_portal <- arrow::s3_bucket("bio230014-bucket01/flux_staging/neon_portal",
                                   endpoint_override = "sdsc.osn.xsede.org",
                                   access_key = Sys.getenv("OSN_KEY"),
                                   secret_key = Sys.getenv("OSN_SECRET"))

s3_current_month <- arrow::s3_bucket("bio230014-bucket01/flux_staging/current_month",
                                     endpoint_override = "sdsc.osn.xsede.org",
                                     access_key = Sys.getenv("OSN_KEY"),
                                     secret_key = Sys.getenv("OSN_SECRET"))


existing_targets_daily <- arrow::read_csv_arrow(s3$path("terrestrial_daily-targets.csv.gz"))

start_date <- as_date(max(existing_targets_daily$datetime)) - dmonths(3)

# Terrestrial

neonstore::neon_download(product = "DP4.00200.001", type = "basic", start_date = start_date)
neonstore::neon_store(product = "DP4.00200.001")
#neon_disconnect()

#DP4.00200.001 & DP1.00094.001
#neon_store(product = "DP4.00200.001")
flux_data <- neon_table(table = "nsae-basic", site = site_names, lazy = TRUE) %>%
  mutate(timeBgn = as_datetime(timeBgn),
         timeEnd = as_datetime(timeEnd)) |>
  select(timeBgn, timeEnd, data.fluxCo2.turb.flux, siteID, data.fluxH2o.turb.flux, qfqm.fluxCo2.turb.qfFinl) |>
  collect()

flux_data |>
  mutate(year = year(timeBgn),
         month = month(timeBgn)) |>
  arrow::write_dataset(s3_neon_portal, partitioning = c("siteID", "year", "month"))

#Get the current unpublished flux data (5-day latency)

if(use_5day_data){
  #files <- readr::read_csv("https://storage.googleapis.com/neon-sae-files/ods/dataproducts/sae_file_url_unpublished.csv", show_col_types = FALSE)
  files <- readr::read_csv("https://storage.googleapis.com/neon-sae-files/ods/sae_files_unpublished/sae_file_url_unpublished.csv", show_col_types = FALSE)
  #Convert old S3 links to GCS
  files$url <- base::gsub("https://s3.data.neonscience.org", "https://storage.googleapis.com", files$url)
  files <- files %>%
    filter(site %in% site_names) %>%
    mutate(file_name = basename(url)) |>
    filter(date > max(lubridate::as_date(flux_data$timeBgn)))

  fs::dir_create(file.path(non_store_dir,"current_month"), recurse = TRUE)
  fs::dir_create(file.path(non_store_dir,"current_month_parquet"), recurse = TRUE)

  fn_parquet_s3 <- s3_current_month$ls()

  if(nrow(files) > 0){
    for(i in 1:nrow(files)){
      destfile <- file.path(non_store_dir,"current_month",files$file_name[i])
      if(!paste0(files$file_name[i],".parquet") %in% fn_parquet_s3){
        download.file(files$url[i], destfile = destfile)
      }
    }
  }


  fn_parquet_s3 <- s3_current_month$ls()

  #Checking for files that have been updated
  if(length(fn_parquet_s3) > 0){
    d <- tibble(file_full = fn_parquet_s3,
                file = tools::file_path_sans_ext(tools::file_path_sans_ext(fn_parquet_s3)),
                date = lubridate::as_datetime(stringr::str_split(fn_parquet_s3, "\\.", 12, simplify = TRUE)[, 10])) |>
      arrange(file, date) |>
      group_by(file) |>
      mutate(max_date = max(date),
             delete = ifelse(date != max(date), 1, 0)) |>
      ungroup()

    for(i in 1:nrow(d)){
      if(d$delete[i] == 1){
        s3_current_month$DeleteFile(d$file[i])
      }
    }
    #remove files that are no longer in the unpublished s3 bucket because they are now in NEON portal

    for(i in 1:length(fn_parquet_s3)){
      if(!tools::file_path_sans_ext(basename(fn_parquet_s3[i])) %in% files$file_name){
        message(paste0("removing: ", basename(fn_parquet_s3[i])))
        s3_current_month$DeleteFile(fn_parquet_s3[i])
      }
    }
  }

  fn_parquet_s3 <- s3_current_month$ls()
  fn <- list.files(file.path(non_store_dir,"current_month"), full.names = TRUE)

  if(length(fn) > 0){
    message(paste0("reading in ", length(fn), " non-NEON portal files"))
    purrr::walk(1:length(fn), function(i, fn){
      message(paste0(i, " of ", length(fn), " reading file ",fn[i]))
      df <- neonstore::neon_read(files = fn[i]) |>
        select(timeBgn, timeEnd, data.fluxCo2.turb.flux, siteID, data.fluxH2o.turb.flux, qfqm.fluxCo2.turb.qfFinl)
      arrow::write_parquet(x = df, s3_current_month$path(paste0(basename(fn[i]),".parquet")))
      unlink(fn[i])
    },
    fn = fn)
  }

  fn_parquet_s3 <- s3_current_month$ls()

  if(length(fn_parquet_s3) > 0){
    flux_data_curr <- arrow::open_dataset(s3_current_month) |>
      select(timeBgn, timeEnd, data.fluxCo2.turb.flux, siteID, data.fluxH2o.turb.flux, qfqm.fluxCo2.turb.qfFinl) |>
      collect()
  }else{
    flux_data_curr <- NULL
  }

  #Combined published and unpublished

  flux_data <- bind_rows(flux_data, flux_data_curr)
}

#flux_data |>
#  group_by(siteID) |>
#  summarize(min = min(timeBgn),
#            max = max(timeBgn), .groups = "drop") |>
#  arrange(min) |>
#  print(n = 50)

#2025-07-15: Per Dave Durden - there was overflagging of QC on SRER in the latest relesae.
#He recommend using the Ameriflux release because it has the correct QC flags.
#This files contains the GC flags for SRER from AMF_US-xSR_FLUXNET_SUBSET_HH_2019-2024_4-7.csv
srer_qc_update <- read_csv("targets/supporting-data/AF_SRER_QC_CODES.csv", show_col_types = FALSE) %>%
  rename(time = datetime,
         siteID = site_id)

flux_data <- flux_data %>%
  mutate(time = as_datetime(timeBgn)) |>
  left_join(srer_qc_update, by = c("time", "siteID")) %>%
  mutate(qfqm.fluxCo2.turb.qfFinl = ifelse(!is.na(nee_AF_qc_flag), nee_AF_qc_flag, qfqm.fluxCo2.turb.qfFinl))


co2_data <- flux_data %>%
  filter(qfqm.fluxCo2.turb.qfFinl == 0 & data.fluxCo2.turb.flux > -50 & data.fluxCo2.turb.flux < 50) %>%
  select(time,data.fluxCo2.turb.flux, siteID, data.fluxH2o.turb.flux) %>%
  rename(nee = data.fluxCo2.turb.flux,
         le = data.fluxH2o.turb.flux,
         site_id = siteID) |>
  mutate(nee = ifelse(site_id == "OSBS" & year(time) < 2019, NA, nee),
         nee = ifelse(site_id == "SRER" & year(time) < 2019, NA, nee),
         nee = ifelse(site_id == "BARR" & year(time) < 2019, NA, nee),
         le = ifelse(site_id == "OSBS" & year(time) < 2019, NA, le),
         le = ifelse(site_id == "SRER" & year(time) < 2019, NA, le),
         le = ifelse(site_id == "BARR" & year(time) < 2019, NA, le)) |>
  pivot_longer(-c("time","site_id"), names_to = "variable", values_to = "observation")

flux_target_30m <- co2_data #left_join(full_time, co2_data, by = c("time", "site_id", "variable"))

valid_dates <- flux_target_30m %>%
  mutate(date = as_date(time)) %>%
  filter(!is.na(observation)) %>%
  group_by(date, site_id, variable) %>%
  summarise(count = n(), .groups = "drop")

flux_target_daily <- flux_target_30m %>%
  mutate(date = as_date(time)) %>%
  group_by(date, site_id, variable) %>%
  summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
  left_join(valid_dates, by = c("date","site_id", "variable")) |>
  mutate(observation = ifelse(count > 24, observation, NA),
         observation = ifelse(is.nan(observation), NA, observation)) %>%
  rename(time = date) %>%
  select(-count) |>
  mutate(observation = ifelse(variable == "nee", (observation * 12 / 1000000) * (60 * 60 * 24), observation))

#Write daily data

flux_target_daily <- flux_target_daily |>
  select(time, site_id, variable, observation) |>
  rename(datetime = time) |>
  mutate(datetime = lubridate::as_datetime(datetime),
         duration = "P1D",
         project_id = "neon4cast") |>
  select(project_id, site_id, datetime, duration, variable, observation) |>
  arrange(variable, datetime) |>
  na.omit()


min_year <- year(min(as_date(flux_target_daily$datetime)))
min_month <- month(min(as_date(flux_target_daily$datetime)))

old_fluxes <- existing_targets_daily |>
  filter(datetime < min(as_date(flux_target_daily$datetime)))

combined_daily <- bind_rows(flux_target_daily, old_fluxes) |>
  arrange(project_id, site_id, datetime, duration, variable) |>
  distinct()

write_csv(combined_daily, "terrestrial_daily-targets.csv.gz")

mc_cp("terrestrial_daily-targets.csv.gz", "osn/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/")

if(fs::dir_exists(fs::path(neon_dir(), "DP4.00200.001"))){
  fs::dir_delete(fs::path(neon_dir(), "DP4.00200.001"))
}
mc_mirror(neonstore::neon_db_dir(), "osn/bio230014-bucket01/neonstore/db", overwrite = TRUE, remove = TRUE)
mc_mirror(neonstore::neon_dir(), "osn/bio230014-bucket01/flux_staging/neonstore_temp", overwrite = TRUE, remove = TRUE)


message(paste0("Completed Terrestrial Target at ", Sys.time()))
