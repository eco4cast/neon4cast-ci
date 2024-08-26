library(arrow)
library(dplyr)
library(gsheet)
library(readr)

#source('catalog/R/stac_functions.R')
config <- yaml::read_yaml('challenge_configuration.yaml')
catalog_config <- config$catalog_config

# file.sources = list.files(c("../stac4cast/R"), full.names=TRUE,
#                           ignore.case=TRUE)
# sapply(file.sources,source,.GlobalEnv)

## CREATE table for column descriptions
forecast_description_create <- data.frame(datetime = 'datetime of the forecasted value (ISO 8601)',
                                          site_id = 'For forecasts that are not on a spatial grid, use of a site dimension that maps to a more detailed geometry (points, polygons, etc.) is allowable. In general this would be documented in the external metadata (e.g., alook-up table that provides lon and lat)',
                                          family = 'For ensembles: “ensemble.” Default value if unspecified for probability distributions: Name of the statistical distribution associated with the reported statistics. The “sample” distribution is synonymous with “ensemble.”For summary statistics: “summary.”',
                                          parameter = 'ensemble member or distribution parameter',
                                          variable = 'name of forecasted variable',
                                          prediction = 'predicted value for variable',
                                          pub_datetime = 'datetime that forecast was submitted',
                                          date = 'date of the forecasted value (ISO 8601)',
                                          reference_datetime = 'datetime that the forecast was initiated (horizon = 0)',
                                          model_id = 'unique model identifier',
                                          reference_date = 'date that the forecast was initiated',
                                          project_id = 'unique identifier for the forecast project',
                                          depth_m = 'depth (meters) in water column of prediction',
                                          duration = 'temporal duration of forecast (hourly, daily, etc.); follows ISO 8601 duration convention')


## CHANGE THE WAY TO READ THE SCHEMA
## just read in example forecast to extract schema information -- ask about better ways of doing this
# theme <- 'daily'
# reference_datetime <- '2023-09-01'
# site_id <- 'fcre'
# model_id <- 'climatology'

print('FIND FORECAST TABLE SCHEMA')
forecast_theme_df <- arrow::open_dataset(arrow::s3_bucket(config$forecasts_bucket, endpoint_override = config$endpoint, anonymous = TRUE)) #|>

print('FIND INVENTORY BUCKET')
forecast_s3 <- arrow::s3_bucket(glue::glue("{config$inventory_bucket}/catalog/forecasts/project_id={config$project_id}"),
                              endpoint_override = "sdsc.osn.xsede.org",
                              anonymous=TRUE)

print('OPEN INVENTORY BUCKET')
forecast_data_df <- arrow::open_dataset(forecast_s3) |>
  filter(project_id == config$project_id) |>
  collect()

theme_models <- forecast_data_df |>
  distinct(model_id)

forecast_sites <- forecast_data_df |>
  distinct(site_id)

forecast_date_range <- forecast_data_df |> dplyr::summarise(min(date),max(date))
forecast_min_date <- forecast_date_range$`min(date)`
forecast_max_date <- forecast_date_range$`max(date)`

build_description <- paste0("Forecasts are the raw forecasts that includes all ensemble members or distribution parameters. Due to the size of the raw forecasts, we recommend accessing the scores (summaries of the forecasts) to analyze forecasts (unless you need the individual ensemble members). You can access the forecasts at the top level of the dataset where all models, variables, and dates that forecasts were produced (reference_datetime) are available. The code to access the entire dataset is provided as an asset. Given the size of the forecast catalog, it can be time-consuming to access the data at the full dataset level. For quicker access to the forecasts for a particular model (model_id), we also provide the code to access the data at the model_id level as an asset for each model.")

forecast_sites <- forecast_sites$site_id

stac4cast::build_forecast_scores(table_schema = forecast_theme_df,
                      #theme_id = 'Forecasts',
                      table_description = forecast_description_create,
                      start_date = forecast_min_date,
                      end_date = forecast_max_date,
                      id_value = "daily-forecasts",
                      description_string = build_description,
                      about_string = catalog_config$about_string,
                      about_title = catalog_config$about_title,
                      theme_title = "Forecasts",
                      destination_path = catalog_config$forecast_path,
                      aws_download_path = catalog_config$aws_download_path_forecasts,
                      link_items = stac4cast::generate_group_values(group_values = names(config$variable_groups)),
                      thumbnail_link = catalog_config$forecasts_thumbnail,
                      thumbnail_title = catalog_config$forecasts_thumbnail_title,
                      group_sites = forecast_sites,
                      model_child = FALSE)

## READ IN GSHEET FILES
variable_gsheet <- gsheet2tbl(config$target_metadata_gsheet)

## READ IN MODEL METADATA
# googlesheets4::gs4_deauth()
# registered_model_id <- googlesheets4::read_sheet(config$model_metadata_gsheet)

gsheet_read <- gsheet2tbl(config$model_metadata_gsheet)
gsheet_read$row_non_na <- rowSums(!is.na(gsheet_read))

registered_model_id <- gsheet_read |>
  filter(`What forecasting challenge are you registering for?` == config$project_id) |>
  rename(project_id = `What forecasting challenge are you registering for?`) |>
  arrange(row_non_na) |>
  distinct(model_id, project_id, .keep_all = TRUE)

## BUILD VARIABLE GROUPS (variables and models)

for (i in 1:length(config$variable_groups)){ ## organize variable groups
  print(names(config$variable_groups)[i])

  # check data and skip if no data found
  var_group_data_check <- forecast_data_df |>
    filter(variable %in% config$variable_groups[[i]]$variable)

  if (nrow(var_group_data_check) == 0){
    print('No data available for group')
    next
  }

  ## REMOVE STALE OR UNUSED DIRECTORIES
  current_var_path <- paste0(catalog_config$forecast_path,'/',names(config$variable_groups[i]))
  current_var_dirs <- list.dirs(current_var_path,recursive = FALSE, full.names = TRUE)
  unlink(current_var_dirs, recursive = TRUE)

  if (!dir.exists(paste0(catalog_config$forecast_path,'/',names(config$variable_groups[i])))){
    dir.create(paste0(catalog_config$forecast_path,'/',names(config$variable_groups[i])))
  }

  # match variable with full name in gsheet
  var_gsheet_arrange <- variable_gsheet |>
    arrange(duration)

  var_values <- names(config$variable_groups[[i]]$group_vars)

  var_name_full <- var_gsheet_arrange[which(var_gsheet_arrange$`"official" targets name` %in% var_values),1][[1]]

  ## CREATE VARIABLE GROUP JSONS
  group_description <- paste0('All variables for the ',names(config$variable_groups[i]),' group.')

  ## find group sites
  find_group_sites <- forecast_data_df |>
    filter(variable %in% var_values) |>
    distinct(site_id)

  ## create empty vector to track publication information
  citation_build <- c()
  doi_build <- c()

  ## create empty vector to track variable information
  variable_name_build <- c()

  for(j in 1:length(config$variable_groups[[i]]$group_vars)){ # FOR EACH VARIABLE WITHIN A MODEL GROUP

    var_name <- names(config$variable_groups[[i]]$group_vars[j])
    print(var_name)

    for (k in 1:length(config$variable_groups[[i]]$group_vars[[j]]$duration)){
      duration_value <- config$variable_groups[[i]]$group_vars[[j]]$duration[k]
      print(duration_value)

      ## save original duration name for reference
      duration_name <- config$variable_groups[[i]]$group_vars[[j]]$duration[k]

      ## create formal variable name
      duration_value[which(duration_value == 'P1D')] <- 'Daily'
      duration_value[which(duration_value == 'PT1H')] <- 'Hourly'
      duration_value[which(duration_value == 'PT30M')] <- '30min'
      duration_value[which(duration_value == 'P1W')] <- 'Weekly'

      var_formal_name <- paste0(duration_value,'_',var_name_full[j])

      # check data and skip if no data found
      var_data_check <- forecast_data_df |>
        filter(variable == var_name)

      if (nrow(var_data_check) == 0){
        print('No data available for variable')
        next
      }

      if (!dir.exists(file.path(catalog_config$forecast_path,names(config$variable_groups)[i],var_formal_name))){
        dir.create(file.path(catalog_config$forecast_path,names(config$variable_groups)[i],var_formal_name))
      }

      var_data <- forecast_data_df |>
        filter(variable == var_name,
               duration == duration_name)

      var_date_range <- var_data |> dplyr::summarise(min(date),max(date))
      var_min_date <- var_date_range$`min(date)`
      var_max_date <- var_date_range$`max(date)`

      var_models <- var_data |>
        distinct(model_id) |>
        filter(model_id %in% registered_model_id$model_id,
               !grepl("example",model_id))

      find_var_sites <- forecast_data_df |>
        filter(variable == var_name) |>
        distinct(site_id)

      var_metadata <- variable_gsheet |>
        filter(`"official" targets name` == var_name,
               duration == duration_name)

      var_description <- paste0('All models for the ',var_formal_name,' variable. The variable description is as follows: ',
                                var_metadata$Description)

      #var_path <- gsub('forecasts','scores',var_data$path[1])
      var_path <- var_data$path[1]

      ## build lists for creating publication items
      var_citations <- config$variable_groups[[i]]$group_vars[[j]]$var_citation
      doi_citations <- config$variable_groups[[i]]$group_vars[[j]]$var_doi

      #update group list of publication information
      citation_build <- append(citation_build, var_citations)
      doi_build <- append(doi_build, doi_citations)

      variable_name_build <- append(variable_name_build, var_formal_name)

      #variable_name_build <- append(variable_name_build, var_formal_name)

      stac4cast::build_group_variables(table_schema = forecast_theme_df,
                                       #theme_id = var_formal_name[j],
                                       table_description = forecast_description_create,
                                       start_date = var_min_date,
                                       end_date = var_max_date,
                                       id_value = var_formal_name,
                                       description_string = var_description,
                                       about_string = catalog_config$about_string,
                                       about_title = catalog_config$about_title,
                                       dashboard_string = catalog_config$dashboard_url,
                                       dashboard_title = catalog_config$dashboard_title,
                                       theme_title = var_formal_name,
                                       destination_path = file.path(catalog_config$forecast_path,names(config$variable_groups)[i],var_formal_name),
                                       aws_download_path = var_path,
                                       group_var_items = stac4cast::generate_variable_model_items(model_list = var_models$model_id),
                                       thumbnail_link = config$variable_groups[[i]]$thumbnail_link,
                                       thumbnail_title = "Thumbnail Image",
                                       group_var_vector = NULL,
                                       single_var_name = var_name,
                                       group_duration_value = duration_value,
                                       group_sites = find_var_sites$site_id,
                                       citation_values = var_citations,
                                       doi_values = doi_citations)

      forecast_sites <- c()

      ## LOOP OVER MODEL IDS AND CREATE JSONS
      for (m in var_models$model_id){

        if (!(m %in% registered_model_id$model_id)){
          message(paste0('Omitting model_id "',m,'" due to missing registration'))
          next
        }

        # make model items directory
        if (!dir.exists(paste0(catalog_config$forecast_path,'/',names(config$variable_groups)[i],'/',var_formal_name,"/models"))){
          dir.create(paste0(catalog_config$forecast_path,'/',names(config$variable_groups)[i],'/',var_formal_name,"/models"))
        }

        print(m)
        model_date_range <- forecast_data_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          dplyr::summarise(min(date),max(date))

        model_min_date <- model_date_range$`min(date)`
        model_max_date <- model_date_range$`max(date)`

        model_var_duration_df <- forecast_data_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          distinct(variable,duration, project_id) |>
          mutate(duration_name = ifelse(duration == 'P1D', 'Daily', duration)) |>
          mutate(duration_name = ifelse(duration == 'PT1H', 'Hourly', duration_name)) |>
          mutate(duration_name = ifelse(duration == 'PT30M', '30min', duration_name)) |>
          mutate(duration_name = ifelse(duration == 'P1W', 'Weekly', duration_name))

        model_var_full_name <- model_var_duration_df |>
          left_join((variable_gsheet |>
                       select(variable = `"official" targets name`, full_name = `Variable name`) |>
                       distinct(variable, .keep_all = TRUE)), by = c('variable'))

        model_sites <- forecast_data_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          distinct(site_id)

        model_site_text <- paste(as.character(model_sites$site_id), sep="' '", collapse=", ")

        model_vars <- forecast_data_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          distinct(variable) |>
          left_join(model_var_full_name, by = 'variable')

        model_vars$var_duration_name <- paste0(model_vars$duration_name, " ", model_vars$full_name)

        forecast_sites <- append(forecast_sites,  stac4cast::get_site_coords(site_metadata = catalog_config$site_metadata_url,
                                                                             sites = model_sites$site_id))

        idx = which(registered_model_id$model_id == m)

        stac_id <- paste0(m,'_',var_name,'_',duration_name,'_forecast')

        if (is.null(registered_model_id$`Web link to model code`[idx]) |
            identical(registered_model_id$`Web link to model code`[idx], character(0)) |
            is.na(registered_model_id$`Web link to model code`[idx])){
          model_code_link <- 'https://projects.ecoforecast.org/neon4cast-ci/'
        } else{
          model_code_link <- registered_model_id$`Web link to model code`[idx]
        }

        model_description <- paste0("All forecasts for the ",
                                    var_formal_name,
                                    ' variable for the ',
                                    m,
                                    ' model. Information for the model is provided as follows: ',
                                    registered_model_id[idx,"Describe your modeling approach in your own words."][[1]],
                                    '.
                                    The model predicts this variable at the following sites: ',
                                    model_site_text,
                                    '.
                                    Forecasts are the raw forecasts that includes all ensemble members or distribution parameters. Due to the size of the raw forecasts, we recommend accessing the forecast summaries or scores to analyze forecasts (unless you need the individual ensemble members)')

        model_keywords <- c(list('Forecasts',config$project_id, names(config$variable_groups)[i], m, var_name_full[j], var_name, duration_value, duration_name),
                            as.list(model_sites$site_id))

        ## build radiantearth stac and raw json link
        stac_link <- paste0('https://radiantearth.github.io/stac-browser/#/external/raw.githubusercontent.com/eco4cast/neon4cast-ci/main/catalog/forecasts/',
                            names(config$variable_groups)[i],'/',
                            var_formal_name, '/models/',
                            m,'.json')

        json_link <- paste0('https://raw.githubusercontent.com/eco4cast/neon4cast-ci/main/catalog/forecasts/',
                            names(config$variable_groups)[i],'/',
                            var_formal_name, '/models/',
                            m,'.json')

        stac4cast::build_model(model_id = m,
                               stac_id = stac_id,
                               team_name = registered_model_id$`Long name of the model (can include spaces)`[idx],
                               #model_description = registered_model_id[idx,"Describe your modeling approach in your own words."][[1]],
                               model_description = model_description,
                               start_date = model_min_date,
                               end_date = model_max_date,
                               var_values = model_vars$var_duration_name,
                               duration_names = model_var_duration_df$duration,
                               duration_value = duration_name,
                               site_values = model_sites$site_id,
                               site_table = catalog_config$site_metadata_url,
                               model_documentation = registered_model_id,
                               destination_path = paste0(catalog_config$forecast_path,'/',names(config$variable_groups)[i],'/',var_formal_name,"/models"),
                               aws_download_path = catalog_config$aws_download_path_forecasts, # CHANGE THIS BUCKET NAME
                               collection_name = 'forecasts',
                               thumbnail_image_name = NULL,
                               table_schema = forecast_theme_df,
                               table_description = forecast_description_create,
                               full_var_df = model_vars,
                               code_web_link = model_code_link,
                               model_keywords = model_keywords,
                               stac_web_link = stac_link,
                               raw_json_link = json_link)

      } ## end model loop
    } ## end duration loop

  } ## end variable loop

  ## BUILD THE GROUP PAGES WITH UPDATED VAR/PUB INFORMATION
  stac4cast::build_group_variables(table_schema = forecast_theme_df,
                                   table_description = forecast_description_create,
                                   start_date = forecast_min_date,
                                   end_date = forecast_max_date,
                                   id_value = names(config$variable_groups)[i],
                                   description_string = group_description,
                                   about_string = catalog_config$about_string,
                                   about_title = catalog_config$about_title,
                                   dashboard_string = catalog_config$dashboard_url,
                                   dashboard_title = catalog_config$dashboard_title,
                                   theme_title = names(config$variable_groups[i]),
                                   destination_path = file.path(catalog_config$forecast_path,names(config$variable_groups)[i]),
                                   aws_download_path = catalog_config$aws_download_path_scores,
                                   group_var_items = stac4cast::generate_group_variable_items(variables = variable_name_build),
                                   thumbnail_link = config$variable_groups[[i]]$thumbnail_link,
                                   thumbnail_title = config$variable_groups[[i]]$thumbnail_title,
                                   group_var_vector = unique(var_values),
                                   single_var_name = NULL,
                                   group_duration_value = NULL,
                                   group_sites = find_group_sites$site_id,
                                   citation_values = citation_build,
                                   doi_values = doi_build)
} # end group loop
