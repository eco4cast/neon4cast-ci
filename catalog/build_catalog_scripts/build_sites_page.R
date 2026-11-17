library(arrow)
library(dplyr)
library(gsheet)
library(readr)

#source('catalog/R/stac_functions.R')
config <- yaml::read_yaml('challenge_configuration.yaml')
catalog_config <- config$catalog_config

## CREATE table for column descriptions
site_description_create <- data.frame(field_domain_id = 'domain identifier',
                                      field_site_id = 'site identifier',
                                      field_site_name = 'site name',
                                      terrestrial = 'binary indicator for variable group',
                                      aquatics = 'binary indicator for variable group',
                                      phenology = 'binary indicator for variable group',
                                      ticks = 'binary indicator for variable group',
                                      beetles = 'binary indicator for variable group',
                                      phenocam_code = 'code used for phenocam',
                                      phenocam_roi = 'phenocam region of interest',
                                      phenocam_vegetation = 'phenocam vegetation type',
                                      field_site_type = 'site type',
                                      field_site_subtype = 'site subtype',
                                      field_colocated_site = 'sites that are colocated',
                                      field_site_host = 'host site',
                                      field_site_url = 'host site url',
                                      field_nonneon_research_allowed = 'is non-neon research allowed',
                                      field_access_details = 'details for accessing the site',
                                      field_neon_field_operations_office = 'operations office for neon site',
                                      latitude = 'site latitude',
                                      longitude = 'site longitude',
                                      field_geodetic_datum = 'geodetic coordinates for site',
                                      field_utm_northing = 'northing utm for site',
                                      field_utm_easting = 'easting utm for site',
                                      field_utm_zone = 'utm zone for site',
                                      field_site_county = 'county where site is located',
                                      field_site_state = 'state where site is located',
                                      field_site_country = 'country where site is located',
                                      field_mean_elevation_m = 'mean site elevation in meters',
                                      field_minimum_elevation_m = 'minimum site elevation in meters',
                                      field_maximum_elevation_m = 'maximum site elevation in meters',
                                      field_mean_annual_temperature_C = 'annual temperature of site in degrees C',
                                      field_mean_annual_precipitation_mm = 'mean annual precipitation at site in mm',
                                      field_dominant_wind_direction = 'dominant wind direction at site',
                                      field_mean_canopy_height_m = 'mean canopy height at site',
                                      field_dominant_nlcd_classes = 'dominant nlcd classes at site',
                                      field_domint_plant_species = 'dominant plant species at site',
                                      field_usgs_huc = 'USGS Hyrdrologic Unit Code (HUC) for site',
                                      field_watershed_name = 'watershed name for site',
                                      field_watershed_size_km2 = 'watershed size for site in square kilometers',
                                      field_lake_depth_mean_m = 'mean lake depth at site in meters',
                                      field_lake_depth_max_m = 'maximum lake depth in meters',
                                      field_tower_height_m = 'tower height at site in meters',
                                      field_usgs_geology_unit = 'USGS geology unit for site',
                                      field_megapit_soil_family = 'magapit soil family at site',
                                      field_soil_subgroup = 'soil subgroup at site',
                                      field_avg_number_of_green_days = 'average number of green days at site',
                                      field_avg_grean_increase_doy = 'average green increase for day of year',
                                      field_avg_green_max_doy = 'average green maximum for day of year',
                                      field_avg_green_decrease_doy = 'average green decrease for day of year',
                                      field_avg_green_min_doy = 'average green minimum for day of year',
                                      field_phenocams = 'phenocam details for site',
                                      field_number_tower_levels = 'number of tower levels at site',
                                      neon_url = 'neon URL for site')


site_df <- read_csv(catalog_config$site_metadata_url, show_col_types = FALSE)

target_objects <- c(config$target_groups$Aquatics$targets_file,
                    config$target_groups$Terrestrial$targets_file,
                    config$target_groups$Beetles$targets_file,
                    config$target_groups$Phenology$targets_file,
                    config$target_groups$Ticks$targets_file)

targets <- read_csv(target_objects, show_col_types = FALSE)

target_date_range <- targets |> dplyr::summarise(min(datetime),max(datetime))
target_min_date <- as.Date(target_date_range$`min(datetime)`)
target_max_date <- as.Date(target_date_range$`max(datetime)`)

build_description <- paste0("The catalog contains site metadata for the ", config$challenge_long_name)

if (!file.exists(paste0("../",catalog_config$site_path))){
  dir.create(paste0("../",catalog_config$site_path))
}

stac4cast::build_sites(table_schema = site_df,
                       table_description = site_description_create,
                       start_date = target_min_date,
                       end_date = target_max_date,
                       id_value = "sites",
                       description_string = build_description,
                       about_string = catalog_config$about_string,
                       about_title = catalog_config$about_title,
                       theme_title = "Site Metadata",
                       destination_path = catalog_config$site_path,
                       #link_items = stac4cast::generate_group_values(group_values = names(config$target_groups)),
                       link_items = NULL,
                       thumbnail_link = catalog_config$site_thumbnail,
                       thumbnail_title = catalog_config$site_thumbnail_title)

message('sites generated...')
