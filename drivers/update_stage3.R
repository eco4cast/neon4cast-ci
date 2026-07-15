library(arrow)

source("drivers/site_list.R")
source("drivers/to_hourly.R")

print(sessioninfo::package_info())

site_list <- read_noaa_download_site_list() |> dplyr::pull(site_id)

#future::plan("future::multisession", workers = 8)

purrr::map(site_list, function(curr_site_id){

  print(curr_site_id)

  s3 <- arrow::s3_bucket("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/stage3",
                         endpoint_override = "sdsc.osn.xsede.org",
                         access_key= Sys.getenv("OSN_KEY"),
                         secret_key= Sys.getenv("OSN_SECRET"))

  stage3_df <- arrow::open_dataset(s3) |>
    dplyr::filter(site_id == curr_site_id) |>
    dplyr::collect()

  max_date <- stage3_df |>
    dplyr::summarise(max = as.character(lubridate::as_date(max(datetime)))) |>
    dplyr::pull(max)

  s3_pseudo <- arrow::s3_bucket("bio230014-bucket01/neon4cast-drivers/noaa/gefs-v12/pseudo",
                                endpoint_override = "sdsc.osn.xsede.org",
                                access_key= Sys.getenv("OSN_KEY"),
                                secret_key= Sys.getenv("OSN_SECRET"))

  vars <- names(stage3_df)

  cut_off <- as.character(lubridate::as_date(max_date) - lubridate::days(3))

  df <- arrow::open_dataset(s3_pseudo) |>
    dplyr::filter(variable %in% c("PRES","TMP","RH","UGRD","VGRD","APCP","DSWRF","DLWRF")) |>
    dplyr::filter(site_id == curr_site_id,
                  reference_datetime >= cut_off) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::as_date(reference_datetime),
                  new_datetime = date + lubridate::hours(as.numeric(horizon)) + lubridate::hours(as.numeric(cycle)),
                  datetime = ifelse(datetime != new_datetime, new_datetime, datetime),
                  datetime = lubridate::as_datetime(datetime)) |>
    dplyr::select(-date, -new_datetime)

  if(nrow(df) > 0){

    df2 <- df |>
      to_hourly(use_solar_geom = TRUE, psuedo = TRUE) |>
    dplyr::mutate(ensemble = as.numeric(stringr::str_sub(ensemble, start = 4, end = 5))) |>
    dplyr::rename(parameter = ensemble)

    stage3_df_update <- stage3_df |>
      dplyr::filter(datetime < min(df2$datetime))

    df2 |>
      dplyr::bind_rows(stage3_df_update) |>
      dplyr::arrange(variable, datetime, parameter) |>
      arrow::write_dataset(path = s3, partitioning = "site_id")
  }
})
