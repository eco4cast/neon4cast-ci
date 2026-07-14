install.packages("remotes")
remotes::install_github("rstudio/reticulate")

library(neonstore)
library(tidyverse)
library(lubridate)
library(contentid)
library(minioclient)
library(reticulate)
library(fs)

py_install("fastavro",pip=TRUE)
py_install("pandas",pip=TRUE)
py_require(c("fastavro"))
py_require(c("pandas"))
source_python("targets/R/read_avro.py")

install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

message(paste0("Running Creating Aquatics Targets at ", Sys.time()))

message("Installing and configuration Google Cloud SDK")

if(!dir.exists(path.expand("~/google-cloud-sdk"))) {
  download.file("https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-cli-linux-x86_64.tar.gz", path.expand("~/google-cloud-cli-linux-x86_64.tar.gz"))
  untar(path.expand("~/google-cloud-cli-linux-x86_64.tar.gz"), exdir = path.expand("~"))
  system2(path.expand("~/google-cloud-sdk/install.sh"))
}
mc_alias_set("efi", "s3-west.nrp-nautilus.io",
             access_key = Sys.getenv("EFI_NRP_KEY"),
             secret_key = Sys.getenv("EFI_NRP_SECRET"))
mc_mirror("efi/gcs-creds", path.expand("~/.config"))
creds <- path.expand("~/.config/phonic-formula-364513-f209e55945f2.json")
gcloud <- path.expand("~/google-cloud-sdk/bin/gcloud")
cmd <- paste0(gcloud," auth activate-service-account neon4cast@phonic-formula-364513.iam.gserviceaccount.com --key-file=", creds," --project=phonic-formula-364513")
system(cmd)

message("Copying over already downloaded raw data")
mc_mirror("efi/aquatics-targets",  path.expand("~/data/"))


Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")

Sys.setenv(TZ = 'UTC')
## 02_generate_targets_aquatics
## Process the raw data into the target variable product

source('targets/R/avro_functions.R')
source('targets/R/data_processing.R')

`%!in%` <- Negate(`%in%`) # not in function

avro_file_directory <- path.expand("~/data/aquatic_avro")
parquet_file_directory <- path.expand("~/data/aquatic_parquet")
EDI_file_directory <- path.expand("~/data/aquatic_EDI")

dir.create(avro_file_directory, showWarnings = FALSE, recursive = TRUE)
dir.create(parquet_file_directory, showWarnings = FALSE, recursive = TRUE)
dir.create(EDI_file_directory, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(avro_file_directory, "wq"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(avro_file_directory, "tsd"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(avro_file_directory, "prt"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(avro_file_directory, "river_tsd"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(parquet_file_directory, "wq"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(parquet_file_directory, "tsd"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(parquet_file_directory, "prt"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(parquet_file_directory, "river_tsd"), showWarnings = FALSE, recursive = TRUE)

#readRenviron("~/.Renviron") # compatible with littler
Sys.setenv("NEONSTORE_HOME" = "~/data/neonstore")
Sys.getenv("NEONSTORE_DB")

#temporary aquatic repo during test of new workflow
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/main/neon4cast_field_site_metadata.csv", show_col_types = FALSE)
sites <- site_data |> dplyr::filter(aquatics == 1)

message(paste0("Running Creating Aquatics Targets at ", Sys.time()))

nonwadable_rivers <- sites$field_site_id[(which(sites$field_site_subtype == "Non-wadeable River"))]
lake_sites <- sites$field_site_id[(which(sites$field_site_subtype == "Lake"))]
stream_sites <- sites$field_site_id[(which(sites$field_site_subtype == "Wadeable Stream"))]
profiling_sites <- c('CRAM', 'LIRO', 'BARC', 'TOOK')

message("Getting list of data from NEON Portal")


# for(y in 2016:year(Sys.Date())){
#   print(y)
pass <- TRUE
iter <- 0
while(pass & iter < 4){
  iter <- iter + 1

  df <-  neonstore:::neon_data(product = "DP1.20288.001",
                               #start_date = paste0(y, "-01-01"),
                               #end_date = paste0(y, "-12-31"),
                               type="basic")

  if(file.exists(path.expand("~/data/aquatics_urls/DP1.20288.001.csv"))){
    full_df_old <- read_csv(path.expand("~/data/aquatics_urls/DP1.20288.001.csv"), show_col_types = FALSE)
  }else{
    full_df_old <- NULL
  }

  full_df <- bind_rows(full_df_old, df) %>%
    distinct()

  print(nrow(full_df))
  print(nrow(full_df_old))
  pass <- nrow(full_df) != nrow(full_df_old)

  write_csv(full_df, path.expand("~/data/aquatics_urls/DP1.20288.001.csv"))
}

#full_df <- read_csv(path.expand("~/data/aquatics_urls/DP1.20288.001.csv"), show_col_types = FALSE)

urls <- full_df |>
  dplyr::filter(grepl("waq_instantaneous", name)) |>
  dplyr::pull(url)

message("Downloading and processing data from NEON Portal")

# Configure SSL for DuckDB so it can verify certificates when accessing
# HTTPS URLs (e.g. storage.googleapis.com).
# 1. SSL_CERT_FILE is read by OpenSSL at connection time and provides the
#    updated CA bundle written by the workflow's update-ca-certificates step.
# 2. Explicitly LOAD httpfs before SET ca_cert_file so the extension is
#    present when the setting is applied.
Sys.setenv(SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt")
duckdb_con <- duckdbfs::cached_connection()
tryCatch(DBI::dbExecute(duckdb_con, "LOAD httpfs"), error = function(e) invisible(NULL))
DBI::dbExecute(duckdb_con, "SET ca_cert_file='/etc/ssl/certs/ca-certificates.crt'")

wq_portal <- duckdbfs::open_dataset(urls, format="csv", filename = TRUE) |>
  dplyr::mutate(siteID = stringr::str_sub(filename, 77,80)) |>
  dplyr::select(siteID, startDateTime, sensorDepth,
                dissolvedOxygen,dissolvedOxygenFinalQF,
                chlorophyll,chlorophyllFinalQF,
                chlaRelativeFluorescence, chlaRelFluoroFinalQF) %>%
  dplyr::mutate(sensorDepth = as.numeric(sensorDepth),
                dissolvedOxygen = as.numeric(dissolvedOxygen),
                chla = as.numeric(chlorophyll),
                chla_RFU = as.numeric(chlaRelativeFluorescence),
                startDateTime = as_datetime(startDateTime),
                time = as_date(startDateTime)) %>%
  # sites that are not profiling do not have accurate depths - set to 0.5
  dplyr::mutate(sensorDepth = ifelse(siteID %in% profiling_sites, sensorDepth,
                                     0.5)) |>
  dplyr::filter(sensorDepth > 0 & sensorDepth < 1) |>
  dplyr::mutate(dissolvedOxygen = ifelse(dissolvedOxygenFinalQF == 1, NA, dissolvedOxygen),
                chla = ifelse(chlorophyllFinalQF == 1, NA, chla),
                chla_RFU = ifelse(chlaRelFluoroFinalQF == 1, NA, chla_RFU)) |>
  dplyr::rename(site_id = siteID) |>
  dplyr::group_by(site_id, time) %>%
  dplyr::summarize(oxygen = mean(dissolvedOxygen, na.rm = TRUE),
                   chla = mean(chla, na.rm = TRUE),
                   chla_RFU = mean(chla_RFU, na.rm = TRUE), .groups = "drop") %>%
  dplyr::select(time, site_id,
                oxygen, chla, chla_RFU) %>%
  pivot_longer(cols = -c("time", "site_id"), names_to = "variable", values_to = "observation") %>%
  dplyr::filter(!((variable == "chla" & site_id %in% stream_sites) |
                    (variable == "chla_RFU" & site_id %in% stream_sites))) |>
  collect()

#====================================================#
##### low latency WQ data =======
message("# download the 24/48hr pre_release data from the Google Cloud")

# where should these files be saved?

fs::dir_create(file.path(avro_file_directory,"DP1.20288.001")) # ignores existing directories unlike dir.create()

# need to figure out which month's data are required
# what is in the NEON store db?
cur_wq_month <- wq_portal %>%
  group_by(site_id) %>%
  summarise(cur_wq_date = as.Date(max(time)),
            new_date = ceiling_date(max(time), unit = 'month'))


# Download any new files from the Google Cloud
download.neon.avro(months = cur_wq_month,
                   data_product = '20288',  # WQ data product
                   path = file.path(avro_file_directory,"DP1.20288.001"))

# Delete superseded files
# Files that have been superseded by the NEON store files can be deleted from the relavent repository
# Look in each repository to see if there are files that exceed the current maximum date of the NEON
# store data
delete.neon.parquet(months = cur_wq_month,
                    path = file.path(parquet_file_directory, "wq"),
                    data_product = '20288')

delete.neon.avro(months = cur_wq_month,
                 path = file.path(avro_file_directory,"DP1.20288.001"),
                 data_product = '20288')

# The variables (term names that should be kept)
wq_vars <- c('siteName',
             'startDate',
             'sensorDepth',
             'dissolvedOxygen',
             'dissolvedOxygenExpUncert',
             'dissolvedOxygenFinalQF',
             'chlorophyll',
             'chlorophyllExpUncert',
             'chlorophyllFinalQF',
             'chlaRelativeFluorescence',
             'chlaRelFluoroFinalQF')
columns_keep <- c('siteName', 'termName', 'startDate', 'Value', 'verticalIndex')

# Generate a list of files to be read
wq_avro_files <- list.files(path = file.path(avro_file_directory, 'DP1.20288.001'),
                            pattern = '*20288',
                            recursive = T, full.names = T)

wq_parquet_files <- list.files(path = file.path(parquet_file_directory, "wq"))

new_files <- map_lgl(wq_avro_files, function(x){
  new_file <- TRUE
  if(basename(x) %in% tools::file_path_sans_ext(wq_parquet_files)){
    new_file <- FALSE
  }
  return(new_file)
})

wq_avro_files <- wq_avro_files[which(new_files)]

if(length(wq_avro_files) > 0){
  #sc <- sparklyr::spark_connect(master = "local")
  # Read in each of the files and then bind by rows
  sc <- NULL
  purrr::walk(.x = wq_avro_files, ~ read.avro.wq(sc= sc,
                                                 path = .x,
                                                 columns_keep = columns_keep,
                                                 dir = file.path(parquet_file_directory, "wq")))
  #spark_disconnect(sc)
}


wq_pre_release <- arrow::open_dataset(file.path(parquet_file_directory, "wq")) |>
  collect()

# Combine the avro files with the portal data
wq_full <- dplyr::bind_rows(wq_portal, wq_pre_release) %>%
  dplyr::arrange(site_id, time)

wq_full <- wq_full |>
  group_by(site_id, time, variable) |>
  summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")

#==============================#

message("##### WQ QC protocol =======")
# additional QC steps implemented (FO, 2022-07-13)
##### check 1 Gross range tests on DO and chlorophyll
# DO ranges for each sensor and each season
DO_max <- 15 # gross max
DO_min <- 2 # gross min

# chlorophyll ranges
chla_max <- 200
chla_min <- 0

# GR flag will be true if either the DO concentration or the chlorophyll are
# outside the ranges specified about

wq_cleaned <- wq_full  |>
  tidyr::pivot_wider(names_from = variable,
                     values_from = observation,
                     id_cols = c(time, site_id)) |>
  tidyr::pivot_longer(cols = -c("time", "site_id"), names_to = 'variable', values_to = 'observation') |>
  dplyr::filter(variable != 'chla_RFU') %>%
  dplyr::mutate(observation = ifelse(is.na(observation),
                                     observation, ifelse(observation >= DO_min & observation <= DO_max & variable == 'oxygen',
                                                         observation, ifelse(observation >= chla_min & observation <= chla_max & variable == 'chla', observation, NA)))) %>%
  # manual cleaning based on visual inspection
  dplyr::mutate(observation = ifelse(site_id == "MAYF" &
                                       between(time, ymd("2019-01-20"), ymd("2019-02-05")) &
                                       variable == "oxygen", NA, observation),
                observation = ifelse(site_id == "WLOU" &
                                       !between(observation, 7.5, 11) &
                                       variable == "oxygen", NA, observation),
                observation = ifelse(site_id == "BARC" &
                                       observation < 4 &
                                       variable == "oxygen", NA, observation),
                observation = ifelse(site_id == "BLDE" &
                                       between(time, ymd("2020-07-01"), ymd("2020-12-31")) &
                                       variable == "oxygen", NA, observation),
                observation = ifelse(site_id == "BIGC" &
                                       between(time, ymd("2021-10-25"), ymd("2021-10-27")) &
                                       variable == "oxygen", NA, observation),
                observation = ifelse(site_id == "REDB" &
                                       time == ymd("2022-04-28") &
                                       variable == "oxygen", NA, observation))
#===============================================#
message("#### Generate hourly temperature profiles for lake #############")
message("##### NEON portal data #####")

# for(y in 2017:year(Sys.Date())){
#   print(y)
pass <- TRUE
iter <- 0
while(pass & iter < 10){
  iter <- iter + 1

  df <-  neonstore:::neon_data(product = "DP1.20264.001",
                               #start_date = paste0(y, "-01-01"),
                               #end_date = paste0(y, "-12-31"),
                               type="basic",
                               site = lake_sites)

  if(file.exists(path.expand("~/data/aquatics_urls/DP1.20264.001_lake_sites.csv"))){
    full_df_old <- read_csv(path.expand("~/data/aquatics_urls/DP1.20264.001_lake_sites.csv"), show_col_types = FALSE)
  }else{
    full_df_old <- NULL
  }

  full_df <- bind_rows(full_df_old, df) %>%
    distinct()

  print(nrow(full_df))
  print(nrow(full_df_old))
  pass <- nrow(full_df) != nrow(full_df_old)

  write_csv(full_df, path.expand("~/data/aquatics_urls/DP1.20264.001_lake_sites.csv"))
}
# }

#full_df <- read_csv(path.expand("~/data/aquatics_urls/DP1.20264.001_lake_sites.csv"), show_col_types = FALSE)



urls <- full_df |>
  dplyr::filter(grepl("TSD_30_min", name)) |>
  dplyr::pull(url)


hourly_temp_profile_portal <-
  duckdbfs::open_dataset(urls, format="csv", filename = TRUE) |>
  dplyr::mutate(site_id = stringr::str_sub(filename, 77,80),
                verticalPosition = stringr::str_sub(filename, 153,155)) |>
  dplyr::select(startDateTime, site_id, tsdWaterTempMean, thermistorDepth,
                tsdWaterTempFinalQF, verticalPosition) |>
  dplyr::mutate(tsdWaterTempMean = as.numeric(tsdWaterTempMean),
                thermistorDepth = as.numeric(thermistorDepth),
                tsdWaterTempFinalQF = as.numeric(tsdWaterTempFinalQF),
                verticalPosition = as.numeric(verticalPosition)) |>
  dplyr::mutate(tsdWaterTempMean = ifelse(tsdWaterTempFinalQF == 1, NA, tsdWaterTempMean)) %>%
  dplyr::rename(depth = thermistorDepth) |>
  dplyr::mutate(date = as_date(startDateTime),
                hour = str_pad(as.character(hour(startDateTime)), width = 2, side = "left", pad = "0"),
                depth = round(depth, 1)) %>% # round to the nearest 0.1 m
  dplyr::summarize(temperature = mean(tsdWaterTempMean, na.rm = TRUE),
                   .by = c("site_id", "depth", "date", "hour")) %>%
  dplyr::select(date, hour, site_id, temperature, depth) |>
  rename(observation = temperature) |>
  mutate(variable = "temperature",
         time = as_datetime(paste0(date, " ",hour, ":00:00"))) |>
  select(-date, - hour) |>
  collect() |>
  QC.temp(range = c(-5, 40), spike = 5, by.depth = T) %>%
  mutate(data_source = 'NEON_portal')

message("##### Sonde EDI data #####")
# Only 6 lake sites available on EDI
edi_url_lake <- c("https://pasta.lternet.edu/package/data/eml/edi/1071/1/7f8aef451231d5388c98eef889332a4b",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/2c8893684d94b9a52394060a76cab798",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/770e2ab9d957991a787a2f990d5a2fad",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/2e52d63ba4dc2040d1e5e2d11114aa93",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/60df35a34bb948c0ca5e5556d129aa98",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/004857d60d6fe7587b112d714e0380d0")
lake_edi_profile <- c("NEON.D03.BARC.DP0.20005.001.01378.csv",
                      "NEON.D05.CRAM.DP0.20005.001.01378.csv",
                      "NEON.D05.LIRO.DP0.20005.001.01378.csv",
                      "NEON.D09.PRLA.DP0.20005.001.01378.csv",
                      "NEON.D09.PRPO.DP0.20005.001.01378.csv",
                      "NEON.D03.SUGG.DP0.20005.001.01378.csv")

fs::dir_create(EDI_file_directory) # ignores existing directories unlike dir.create()
# Download the data

for(i in 1:length(edi_url_lake)){
  if (!file.exists(file.path(EDI_file_directory,  lake_edi_profile[i]))) {
    if (!dir.exists(dirname(file.path(EDI_file_directory,
                                      lake_edi_profile[i])))) {
      dir.create(dirname(file.path(EDI_file_directory,
                                   lake_edi_profile[i])))
    }
    download.file(edi_url_lake[i], destfile = file.path(EDI_file_directory, lake_edi_profile[i]))
  }
}


# List all the files in the EDI directory
edi_data <- list.files(file.path(EDI_file_directory), full.names = T)
# Get the lake sites subset
edi_lake_files <- c(edi_data[grepl(x = edi_data, pattern= lake_sites[1])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[2])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[3])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[4])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[5])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[6])])

# Calculate the hourly average profile
hourly_temp_profile_EDI <- purrr::map_dfr(.x = edi_lake_files, ~ read.csv(file = .x)) %>%
  rename('site_id' = siteID,
         'depth' = sensorDepth,
         'observation' = waterTemp) %>%
  mutate(startDate  = lubridate::ymd_hm(startDate),
         time = lubridate::ymd_h(format(startDate, '%Y-%m-%d %H')),
         depth = round(depth, digits = 1)) %>%
  group_by(site_id, time, depth) %>%
  summarise(observation = mean(observation),.groups = "drop") %>%
  mutate(variable = "temperature") %>%
  # include first QC of data
  QC.temp(range = c(-5, 40), spike = 5, by.depth = T) %>%
  mutate(data_source = 'MS_raw')

message("##### avros data #####")
message("# Download any new files from the Google Cloud")

# need to figure out which data are required
# what is in the NEON store db?
cur_tsd_month <- hourly_temp_profile_portal %>%
  group_by(site_id) %>%
  summarise(cur_wq_date = as.Date(max(time)),
            new_date = as.Date(ceiling_date(max(time), unit = 'month')))

# Download any new files from the Google Cloud
download.neon.avro(months = cur_tsd_month,
                   data_product = '20264',  # TSD data product
                   path = file.path(avro_file_directory,"DP1.20264.001"))
# Start by deleting superseded files
# Files that have been supersed by the NEON store files can be deleted from the relevent repository
# Look in each repository to see if there are files that match the current maximum month of the NEON
# store data

delete.neon.parquet(months = cur_tsd_month,
                    path = file.path(parquet_file_directory, "tsd"),
                    data_product = '20264')

delete.neon.avro(months = cur_tsd_month,
                 path = file.path(avro_file_directory, "DP1.20264.001"),
                 data_product = '20264')

# The variables (term names that should be kept)
tsd_vars <- c('siteName',
              'startDate',
              'tsdWaterTempMean',
              'thermistorDepth',
              'tsdWaterTempExpUncert',
              'tsdWaterTempFinalQF')

columns_keep <- c('siteName', 'termName', 'startDate', 'Value', 'verticalIndex')
thermistor_depths <- readr::read_csv('targets/supporting-data/thermistorDepths.csv', col_types = 'ccd')

# Generate a list of files to be read
tsd_avro_files <- list.files(path = avro_file_directory,
                                    pattern = '*20264',
                                    recursive = T,
                                    full.names = T)

lake_avro_files <- c(tsd_avro_files[grepl(x = tsd_avro_files, pattern= lake_sites[1])],
                     tsd_avro_files[grepl(x = tsd_avro_files, pattern= lake_sites[2])],
                     tsd_avro_files[grepl(x = tsd_avro_files, pattern= lake_sites[3])],
                     tsd_avro_files[grepl(x = tsd_avro_files, pattern= lake_sites[4])],
                     tsd_avro_files[grepl(x = tsd_avro_files, pattern= lake_sites[5])],
                     tsd_avro_files[grepl(x = tsd_avro_files, pattern= lake_sites[6])],
                     tsd_avro_files[grepl(x = tsd_avro_files, pattern= lake_sites[7])])

tsd_parquet_files <- list.files(path = file.path(parquet_file_directory, "tsd"))

new_files <- map_lgl(lake_avro_files, function(x){
  new_file <- TRUE
  if(basename(x) %in% tools::file_path_sans_ext(tsd_parquet_files)){
    new_file <- FALSE
  }
  return(new_file)
})

lake_avro_files <- lake_avro_files[which(new_files)]

if(length(lake_avro_files) > 0){
  #sc <- sparklyr::spark_connect(master = "local")
  message("# Read in each of the files and then bind by rows")
  sc <- NULL
  hourly_temp_profile_avro <- purrr::walk(.x = lake_avro_files,
                                          ~ read.avro.tsd.profile(sc= sc,
                                                                  path = .x,
                                                                  thermistor_depths = thermistor_depths,
                                                                  columns_keep = columns_keep,
                                                                  dir = file.path(parquet_file_directory, "tsd"),
                                                                  delete_files = FALSE))
  #spark_disconnect(sc)
}

# Read in the pre-release
hourly_temp_profile_prerelease <- arrow::open_dataset(file.path(parquet_file_directory, "tsd")) |>
  collect()

# Combine the three data sources
hourly_temp_profile_lakes <- bind_rows(hourly_temp_profile_portal, hourly_temp_profile_EDI, hourly_temp_profile_prerelease) %>%
  arrange(time, site_id, depth) %>%
  group_by(time, site_id, depth) %>%
  summarise(observation = mean(observation, na.rm = T), .groups = "drop") |>
  mutate(variable = "temperature") |>
  select(time, site_id, depth, variable, observation)
#======================================================#

message("#### Generate surface (< 1 m) temperature #############")
message("###### Lake temperatures #####")
# Daily surface lake temperatures generated from the hourly profiles created above

daily_temp_surface_lakes <- hourly_temp_profile_lakes %>%
  dplyr::filter(depth <= 1) %>%
  mutate(time = lubridate::as_date(time)) %>%
  group_by(site_id, time) %>%
  summarise(observation = mean(observation, na.rm = T),.groups = "drop") %>%
  mutate(variable = 'temperature')

message("##### Stream temperatures #####")

# for(y in 2016:year(Sys.Date())){
#   print(y)
pass <- TRUE
iter <- 0
while(pass & iter < 10){
  iter <- iter + 1

  df <-  neonstore:::neon_data(product = "DP1.20053.001",
                               #start_date = paste0(y, "-01-01"),
                               #end_date = paste0(y, "-12-31"),
                               type="basic",
                               site = stream_sites)

  if(file.exists(path.expand("~/data/aquatics_urls/DP1.20053.001.csv"))){
    full_df_old <- read_csv(path.expand("~/data/aquatics_urls/DP1.20053.001.csv"), show_col_types = FALSE)
  }else{
    full_df_old <- NULL
  }

  full_df <- bind_rows(full_df_old, df) %>%
    distinct()

  print(nrow(full_df))
  print(nrow(full_df_old))
  pass <- nrow(full_df) != nrow(full_df_old)

  write_csv(full_df, path.expand("~/data/aquatics_urls/DP1.20053.001.csv"))
}
# }

#full_df <- read_csv(path.expand("~/data/aquatics_urls/DP1.20053.001.csv"), show_col_types = FALSE)


urls <- full_df |>
  dplyr::filter(grepl("TSW_30min", name)) |>
  dplyr::pull(url)

temp_streams_portal <-
  duckdbfs::open_dataset(urls, format="csv", filename = TRUE) |>
  dplyr::mutate(site_id = stringr::str_sub(filename, 77,80),
                verticalPosition = stringr::str_sub(filename, 153,155),
                horizontalPosition = stringr::str_sub(filename, 149,151)) |>
  dplyr::filter(horizontalPosition == "101" |
                  horizontalPosition == "111" | # take upstream to match WQ data
                  (horizontalPosition == "112" & site_id == "BLUE"), # no data at BLUE upstream
                finalQF == 0) %>%
  dplyr::select(startDateTime, site_id, surfWaterTempMean, finalQF) %>%
  dplyr::mutate(time = as_date(startDateTime),
                surfWaterTempMean = as.numeric(surfWaterTempMean)) %>%
  # dplyr::group_by(time, site_id) %>%
  dplyr::summarize(temperature = mean(surfWaterTempMean, na.rm = TRUE), .by = c('time', 'site_id')) %>%
  dplyr::select(time, site_id, temperature) %>%
  rename(observation = temperature) |>
  mutate(variable = "temperature") |>
  collect()

temp_streams_portal_QC <- temp_streams_portal %>%
  QC.temp(range = c(-5, 40), spike = 7, by.depth = F)
#===========================================#
message("##### Stream temperatures2 #####")
#### avros

# need to figure out which month's data are required
# what is in the NEON store db?
cur_prt_month <- temp_streams_portal_QC %>%
  group_by(site_id) %>%
  summarise(cur_wq_date = as.Date(max(time)),
            new_date = as.Date(ceiling_date(max(time), unit = 'month')))


# what is the next month from this plus the current month? These might be the same
# new_month_prt <- unique(format(c((as.Date(max(temp_streams_portal_QC$time)) %m+% months(1)), (Sys.Date() - days(2))), "%Y-%m"))

# Download any new files from the Google Cloud
download.neon.avro(months = cur_prt_month,
                   data_product = '20053',  # PRT data product
                   path = file.path(avro_file_directory,"DP1.20053.001"))

# Start by deleting superseded files
# Files that have been supersed by the NEON store files can be deleted from the relevent repository
# Look in each repository to see if there are files that match the current maximum month of the NEON
# store data

delete.neon.parquet(months = cur_prt_month,
                    path = file.path(parquet_file_directory, "prt"),
                    data_product = '20053')

delete.neon.avro(months = cur_prt_month,
                 path = file.path(avro_file_directory, "DP1.20053.001"),
                 data_product = '20053')

# The variables (term names that should be kept)
prt_vars <- c('siteName',
              'startDate',
              'surfWaterTempMean',
              'surfWaterTempExpUncert',
              'finalQF')

columns_keep <- c('siteName', 'termName', 'startDate', 'Value', 'verticalIndex')


# Generate a list of files to be read
prt_avro_files <- list.files(path = avro_file_directory,
                             pattern = '*20053',
                             recursive = T,full.names = TRUE)



prt_parquet_files <- list.files(path = file.path(parquet_file_directory, "prt"))

new_files <- map_lgl(prt_avro_files, function(x){
  new_file <- TRUE
  if(basename(x) %in% tools::file_path_sans_ext(prt_parquet_files)){
    new_file <- FALSE
  }
  return(new_file)
})

prt_avro_files <- prt_avro_files[which(new_files)]

## check for bad NEON files and remove if present
problem_files <- c('/home/rstudio/data/aquatic_avro/site=BIGC/BIGC_L0_to_L1_Surface_Water_Temperature_DP1.20053.001__2021-12-27.avro',
                   '/home/rstudio/data/aquatic_avro/site=BIGC/BIGC_L0_to_L1_Surface_Water_Temperature_DP1.20053.001__2023-02-01.avro',
                   '/home/rstudio/data/aquatic_avro/site=BIGC/BIGC_L0_to_L1_Surface_Water_Temperature_DP1.20053.001__2023-01-28.avro')

problem_files <- map_lgl(prt_avro_files, function(x){
  problem_file <- FALSE
  if(stringr::str_detect(x, "BIGC_L0_to_L1_Surface_Water_Temperature_DP1.20053.001")){
    problem_file <- TRUE
  }
  return(problem_file)
})

if(any(problem_files == TRUE)){
  prt_avro_files <- prt_avro_files[!problem_files]
  message('Problem files removed')
}


if(length(prt_avro_files > 0)){
  # sc <- sparklyr::spark_connect(master = "local")
  # Read in each of the files and then bind by rows
  sc <- NULL
  purrr::walk(.x = prt_avro_files, ~ read.avro.prt(sc= sc,
                                                   path = .x,
                                                   columns_keep = columns_keep,
                                                   dir = file.path(parquet_file_directory, "prt")))
  #spark_disconnect(sc)
}


temp_streams_prerelease <- arrow::open_dataset(file.path(parquet_file_directory, "prt")) |>
  collect()

#===============================================#

message("##### River temperature ######")
# For non-wadeable rivers need portal, EDI and avro data

# for(y in 2016:year(Sys.Date())){
#   print(y)
pass <- TRUE
iter <- 0
while(pass & iter < 10){
  iter <- iter + 1

  df <-  neonstore:::neon_data(product = "DP1.20264.001",
                               #start_date = paste0(y, "-01-01"),
                               #end_date = paste0(y, "-12-31"),
                               type="basic",
                               site = nonwadable_rivers)

  if(file.exists(path.expand("~/data/aquatics_urls/DP1.20264.001_nonwadable_rivers.csv"))){
    full_df_old <- read_csv(path.expand("~/data/aquatics_urls/DP1.20264.001_nonwadable_rivers.csv"), show_col_types = FALSE)
  }else{
    full_df_old <- NULL
  }

  full_df <- bind_rows(full_df_old, df) %>%
    distinct()

  print(nrow(full_df))
  print(nrow(full_df_old))
  pass <- nrow(full_df) != nrow(full_df_old)

  write_csv(full_df, path.expand("~/data/aquatics_urls/DP1.20264.001_nonwadable_rivers.csv"))
}
# }

#full_df <- read_csv(path.expand("~/data/aquatics_urls/DP1.20264.001_nonwadable_rivers.csv"), show_col_types = FALSE)



urls <- full_df |>
  dplyr::filter(grepl("TSD_30_min", name)) |>
  dplyr::pull(url)


temp_rivers_portal <- duckdbfs::open_dataset(urls, format="csv", filename = TRUE) |>
  dplyr::mutate(site_id = stringr::str_sub(filename, 77,80)) |>
  dplyr::mutate(depth = as.numeric(thermistorDepth),
                tsdWaterTempMean = as.numeric(tsdWaterTempMean),
                tsdWaterTempFinalQF = as.numeric(tsdWaterTempFinalQF)) %>%
  dplyr::select(startDateTime, site_id, tsdWaterTempMean, depth, tsdWaterTempFinalQF) %>%
  rename(time = startDateTime) %>%
  dplyr::filter(tsdWaterTempFinalQF == 0) %>%
  dplyr::summarize(temperature = mean(tsdWaterTempMean, na.rm = TRUE), .by = c("time", "site_id")) %>%
  dplyr::select(time, site_id, temperature) %>%
  rename(observation = temperature) |>
  mutate(variable = "temperature") |>
  collect() %>%
  dplyr::mutate(time = as_date(time))


temp_rivers_portal_QC <- temp_rivers_portal %>%
  QC.temp(range = c(-5, 40), spike = 7, by.depth = F)

# EDI data
edi_url_river <- c("https://pasta.lternet.edu/package/data/eml/edi/1185/1/fb9cf9ba62ee8e8cf94cb020175e9165",
                   "https://pasta.lternet.edu/package/data/eml/edi/1185/1/fac068cff680ae28473c3e13dc75aa9f",
                   "https://pasta.lternet.edu/package/data/eml/edi/1185/1/5567ad7252b598ee40f5653e7b732ff4" )

river_edi_profile <- c("NEON.D03.FLNT.DP0.20005.001.01378.csv",
                       "NEON.D03.BLWA.DP0.20005.001.01378.csv",
                       "NEON.D03.TOMB.DP0.20005.001.01378.csv")

for(i in 1:length(edi_url_river)){
  if (!file.exists(file.path(EDI_file_directory,river_edi_profile[i]))) {
    if (!dir.exists(dirname(file.path(EDI_file_directory,
                                      river_edi_profile[i])))) {
      dir.create(dirname(file.path(EDI_file_directory,
                                   river_edi_profile[i])))
    }
    download.file(edi_url_river[i], destfile = file.path(EDI_file_directory, river_edi_profile[i]))
  }
}

edi_data <- list.files(file.path(EDI_file_directory), full.names = T)

edi_rivers <- c(edi_data[grepl(x = edi_data, pattern= nonwadable_rivers[1])],
                edi_data[grepl(x = edi_data, pattern= nonwadable_rivers[2])],
                edi_data[grepl(x = edi_data, pattern= nonwadable_rivers[3])])

# The hourly data set is for the whole water column.
temp_rivers_EDI <- purrr::map_dfr(.x = edi_rivers, ~ read.csv(file = .x)) %>%
  rename('site_id' = siteID,
         'observation' = waterTemp) %>%
  mutate(startDate  = lubridate::ymd_hm(startDate),
         time = as.Date(startDate)) %>%
  group_by(site_id, time) %>%
  summarise(observation = mean(observation),.groups = "drop") %>%
  # include first QC of data
  QC.temp(range = c(-5, 40), spike = 5, by.depth = F) |>
  mutate(variable = "temperature")


message('download non-wadable rivers avros')
cur_tsd_month <- temp_rivers_portal_QC %>%
  group_by(site_id) %>%
  summarise(cur_wq_date = as.Date(max(time)),
            new_date = as.Date(ceiling_date(max(time), unit = 'month')))

# Download any new files from the Google Cloud
download.neon.avro(months = cur_tsd_month,
                   data_product = '20264',  # TSD data product
                   path = file.path(avro_file_directory, "DP1.20264.001"))

message("Generate a list of nonwadable_rivers avro files to be read")
tsd_avro_files <- list.files(path = avro_file_directory,
                                    pattern = '*20264',
                                    recursive = T,
                                    full.names = T)

river_avro_files <- c(tsd_avro_files[grepl(x = tsd_avro_files, pattern= nonwadable_rivers[1])],
                      tsd_avro_files[grepl(x = tsd_avro_files, pattern= nonwadable_rivers[2])],
                      tsd_avro_files[grepl(x = tsd_avro_files, pattern= nonwadable_rivers[3])])

tsd_parquet_files <- list.files(path = file.path(parquet_file_directory, "river_tsd"))

new_files <- map_lgl(river_avro_files, function(x){
  new_file <- TRUE
  if(basename(x) %in% tools::file_path_sans_ext(tsd_parquet_files)){
    new_file <- FALSE
  }
  return(new_file)
})

river_avro_files <- river_avro_files[which(new_files)]


if(length(river_avro_files) > 0){
  #sc <- sparklyr::spark_connect(master = "local")
  # Read in each of the files and then bind by rows
  sc <- NULL
  purrr::walk(.x = river_avro_files,  ~ read.avro.tsd(sc= sc,
                                                      path = .x,
                                                      thermistor_depths = thermistor_depths,
                                                      dir = file.path(parquet_file_directory, "river_tsd"),
                                                      delete_files = FALSE))
  #spark_disconnect(sc)
}


temp_rivers_prerelease <- arrow::open_dataset(file.path(parquet_file_directory, "river_tsd")) |>
  collect()

#===========================================#

message("#### surface temperatures ####")

# Combine the avro files with the portal data
temp_full <- dplyr::bind_rows(# Lakes surface temperature
  daily_temp_surface_lakes,

  # Stream temperature data
  temp_streams_portal_QC,
  temp_streams_prerelease,

  # River temperature data
  temp_rivers_portal_QC,
  temp_rivers_EDI,
  temp_rivers_prerelease) %>%
  dplyr::arrange(site_id, time) %>%
  group_by(site_id, time) %>%
  summarise(observation = mean(observation, na.rm = T),.groups = "drop") %>%
  mutate(variable = 'temperature')


#### Temp QC protocol=================

# additional QC steps implemented (FO, 2022-07-13)
##### check 1 Gross range tests on temperature
# temperature ranges
T_max <- 40 # gross max
T_min <- -2 # gross min

# GR flag will be true if the temperature is outside the range specified
temp_cleaned <-  temp_full %>%
  dplyr::mutate(observation =ifelse(observation >= T_min & observation <= T_max ,
                                    observation, NA))  %>%
  # manual cleaning based on observation
  dplyr:: mutate(observation = ifelse(site_id == "PRLA" & time <ymd("2019-01-01"),
                                      NA, observation))

#### Targets==========================
targets_long <- dplyr::bind_rows(wq_cleaned, temp_cleaned) %>%
  dplyr::arrange(site_id, time, variable) %>%
  dplyr::mutate(observation = ifelse(is.nan(observation), NA, observation))

message("#### Writing Targets to file ####")


targets_long <- targets_long |>
  rename(datetime = time) |>
  mutate(datetime = lubridate::as_datetime(datetime),
         duration = "P1D",
         project_id = "neon4cast") |>
  select(project_id, site_id, datetime, duration, variable, observation)

write_csv(targets_long, "aquatics-targets.csv.gz")

mc_cp("aquatics-targets.csv.gz", "osn/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/")

message("#### Writing Hourly Targets to file ####")
hourly_temp_profile_lakes <- hourly_temp_profile_lakes |>
  rename(datetime = time) |>
  mutate(datetime = lubridate::as_datetime(datetime),
         duration = "PT1H",
         project_id = "neon4cast") |>
  select(project_id, site_id, depth, datetime, duration, variable, observation)

write_csv(hourly_temp_profile_lakes, "aquatics-expanded-observations.csv.gz")

mc_cp("aquatics-expanded-observations.csv.gz", "osn/bio230014-bucket01/challenges/supporting_data/project_id=neon4cast/")

# sync the data back to the S3 cache
mc_mirror( path.expand("~/data/"), "efi/aquatics-targets", overwrite = TRUE, remove = TRUE)


message(paste0("Completed Aquatics Target at ", Sys.time()))
