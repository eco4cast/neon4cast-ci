library(minioclient)
source("https://raw.githubusercontent.com/eco4cast/neon4cast/main/R/to_hourly.R")

#install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", "", "")
mc_mirror("osn/bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/pseudo", "pseudo")

duckdbfs::duckdb_secrets(
  endpoint = 'sdsc.osn.xsede.org',
  key = Sys.getenv("OSN_KEY"),
  secret = Sys.getenv("OSN_SECRET"),
  url_compatibility_mode = FALSE)

df <- arrow::open_dataset("pseudo") |>
  dplyr::filter(variable %in% c("PRES","TMP","RH","UGRD","VGRD","APCP","DSWRF","DLWRF"))


site_list <- readr::read_csv(paste0("https://github.com/eco4cast/",
                                    "neon4cast-noaa-download/",
                                    "raw/master/noaa_download_site_list.csv"),
                             show_col_types = FALSE) |> dplyr::pull(site_id)

s3 <- arrow::s3_bucket("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12",
                       endpoint_override = "sdsc.osn.xsede.org",
                       access_key= Sys.getenv("OSN_KEY"),
                       secret_key= Sys.getenv("OSN_SECRET"))

s3$CreateDir("stage3")

s3 <- arrow::s3_bucket("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage3",
                       endpoint_override = "sdsc.osn.xsede.org",
                       access_key= Sys.getenv("OSN_KEY"),
                       secret_key= Sys.getenv("OSN_SECRET"))

#site_list <- site_list[1:3]

future::plan("future::multisession", workers = 8)

furrr::future_walk(site_list, function(curr_site_id){

  df <- arrow::open_dataset("pseudo") |>
    dplyr::filter(variable %in% c("PRES","TMP","RH","UGRD","VGRD","APCP","DSWRF","DLWRF")) |>
    dplyr::filter(site_id == curr_site_id) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::as_date(reference_datetime),
                  new_datetime = date + lubridate::hours(as.numeric(horizon)) + lubridate::hours(as.numeric(cycle)),
                  datetime = ifelse(datetime != new_datetime, new_datetime, datetime),
                  datetime = lubridate::as_datetime(datetime)) |>
    dplyr::select(-date, -new_datetime)

  s3 <- arrow::s3_bucket("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage3",
                         endpoint_override = "sdsc.osn.xsede.org",
                         access_key= Sys.getenv("OSN_KEY"),
                         secret_key= Sys.getenv("OSN_SECRET"))

  print(curr_site_id)
  df |>
    #dplyr::filter(site_id == curr_site_id) |>
    #dplyr::collect() |>
    to_hourly(use_solar_geom = TRUE, psuedo = TRUE) |>
    dplyr::mutate(ensemble = as.numeric(stringr::str_sub(ensemble, start = 4, end = 5))) |>
    dplyr::rename(parameter = ensemble) |>
    arrow::write_dataset(path = s3, partitioning = "site_id")
    #duckdbfs::write_dataset(path = "s3://bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage3", format = 'parquet',
    #                        partitioning = "site_id")
})
