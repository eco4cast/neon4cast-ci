# Use this repo's copy so stage 2 and stage 3 share one to_hourly(). This
# script used to source it from the neon4cast package's ci_upgrade branch,
# which could change or vanish independently of this repo.
source("drivers/to_hourly.R")
library(arrow)
print(sessioninfo::package_info())


site_list <- readr::read_csv("neon4cast_field_site_metadata.csv",
                             show_col_types = FALSE) |>
  dplyr::rename(site_id = field_site_id)

s3_stage2 <- arrow::s3_bucket("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage2",
                              endpoint_override = "sdsc.osn.xsede.org",
                              access_key= Sys.getenv("OSN_KEY"),
                              secret_key= Sys.getenv("OSN_SECRET"))

#duckdbfs::duckdb_secrets(
#  endpoint = 'sdsc.osn.xsede.org',
#  key = Sys.getenv("OSN_KEY"),
#  secret = Sys.getenv("OSN_SECRET"))

#df <- arrow::open_dataset(s3_stage2) #|>
  #dplyr::distinct(reference_datetime) |>
  #dplyr::collect()

have_dates <- dplyr::tibble(reference_datetime = gsub("reference_datetime=", "", s3_stage2$ls()))

#stage1_s3 <- arrow::s3_bucket("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage1",
#                       endpoint_override = "sdsc.osn.xsede.org",
#                       anonymous = TRUE)


#efi <- duckdbfs::open_dataset("s3://bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage1",
#                    s3_access_key_id="",
#                    s3_endpoint="sdsc.osn.xsede.org")
#df_stage1 <- arrow::open_dataset(stage1_s3) |>
#  dplyr::summarize(max(reference_datetime)) |>
#  dplyr::collect()

curr_date <- Sys.Date()
last_week <- dplyr::tibble(reference_datetime = as.character(seq(curr_date - lubridate::days(14), curr_date - lubridate::days(1), by = "1 day")))

missing_dates <- dplyr::anti_join(last_week, have_dates, by = "reference_datetime") |> dplyr::pull(reference_datetime)

if(length(missing_dates) > 0){
  for(i in 1:length(missing_dates)){

    print(missing_dates[i])

    bucket <- paste0("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage1/reference_datetime=",missing_dates[i])

    endpoint_override <- "https://sdsc.osn.xsede.org"
    s3 <- arrow::s3_bucket(paste0(bucket),
                           endpoint_override = endpoint_override,
                           anonymous = TRUE)

    site_df <- arrow::open_dataset(s3) |>
      dplyr::filter(variable %in% c("PRES","TMP","RH","UGRD","VGRD","APCP","DSWRF","DLWRF")) |>
      dplyr::filter(site_id %in% site_list$site_id) |>
      dplyr::collect() |>
      dplyr::mutate(reference_datetime = missing_dates[i])

    hourly_df <- to_hourly(site_df, use_solar_geom = TRUE, psuedo = FALSE) |>
      dplyr::mutate(ensemble = as.numeric(stringr::str_sub(ensemble, start = 4, end = 5)),
                    reference_datetime = lubridate::as_date(reference_datetime)) |>
      dplyr::rename(parameter = ensemble)

    arrow::write_dataset(hourly_df, path = s3_stage2, partitioning = c("reference_datetime", "site_id"))
    #duckdbfs::write_dataset(hourly_df, path = "s3://bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage2", format = 'parquet',
     #                       partitioning = c("reference_datetime", "site_id"))
  }
}


