read_noaa_download_site_list <- function() {
  readr::read_csv("neon4cast_field_site_metadata.csv",
                  show_col_types = FALSE) |>
    dplyr::transmute(site_name = field_site_name,
                     site_id = field_site_id,
                     latitude = as.numeric(latitude),
                     longitude = as.numeric(longitude))
}
