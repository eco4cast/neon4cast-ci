library(arrow)
library(dplyr)
library(gsheet)
library(readr)

config <- yaml::read_yaml('challenge_configuration.yaml')
catalog_config <- config$catalog_config

## CREATE table for column descriptions
scores_description_create <- data.frame(reference_datetime ='datetime that the forecast was initiated (horizon = 0)',
                                 site_id = 'For forecasts that are not on a spatial grid, use of a site dimension that maps to a more detailed geometry (points, polygons, etc.) is allowable. In general this would be documented in the external metadata (e.g., alook-up table that provides lon and lat); however in netCDF this could be handled by the CF Discrete Sampling Geometry data model.',
                                 datetime = 'datetime of the forecasted value (ISO 8601)',
                                 family = 'For ensembles: “ensemble.” Default value if unspecified For probability distributions: Name of the statistical distribution associated with the reported statistics. The “sample” distribution is synonymous with “ensemble.” For summary statistics: “summary.”If this dimension does not vary, it is permissible to specify family as a variable attribute if the file format being used supports this (e.g.,netCDF).',
                                 variable = 'name of forecasted variable',
                                 observation = 'observed value for variable',
                                 crps = 'crps forecast score',
                                 logs = 'logs forecast score',
                                 mean = 'mean forecast prediction',
                                 median = 'median forecast prediction',
                                 sd = 'standard deviation forecasts',
                                 quantile97.5 = 'upper 97.5 percentile value of forecast',
                                 quantile02.5 = 'upper 2.5 percentile value of forecast',
                                 quantile90 = 'upper 90 percentile value of forecast',
                                 quantile10 = 'upper 10 percentile value of forecast',
                                 duration = 'temporal duration of forecast (hourly = PT1H, daily = P1D, etc.); follows ISO 8601 duration convention',
                                 depth_m = 'depth (meters) in water column of prediction',
                                 model_id = 'unique model identifier',
                                 date = 'ISO 8601 (ISO 2019) date of the predicted value; follows CF convention http://cfconventions.org/cf-conventions/cf-conventions.html#time-coordinate. This variable was called time before v0.5of the EFI convention. For time-integrated variables (e.g., cumulative net primary productivity), one should specify the start_datetime and end_datetime as two variables, instead of the single datetime. If this is not provided the datetime is assumed to be the MIDPOINT of the integration period.',
                                 pub_datetime = 'datetime that forecast was submitted',
                                 project_id = 'unique project identifier')

site_metadata <- read_csv(catalog_config$site_metadata_url) |>
  distinct(field_site_id) |>
  pull(field_site_id)

print('FIND SCORES TABLE SCHEMA')
scores_theme_df <- arrow::open_dataset(arrow::s3_bucket(paste0(config$scores_bucket,'/bundled-parquet'), endpoint_override = config$endpoint, anonymous = TRUE))

scores_sites <- duckdbfs::open_dataset(paste0("s3://anonymous@",config$scores_bucket,"/bundled-parquet/project_id=",
                                              config$project_id,"/?endpoint_override=",config$endpoint))   |>
  filter(duration %in% c("P1D", "P1W")) |>
  distinct(model_id, variable, duration, site_id) |>
  collect() |>
  filter(site_id %in% site_metadata,
         !is.na(model_id))

all_scores_sites <- unique(scores_sites$site_id)

scores_model_var_max_date_df <- duckdbfs::open_dataset(paste0("s3://anonymous@",config$scores_bucket,"/bundled-parquet/project_id=",
                                                              config$project_id,"/?endpoint_override=",config$endpoint))   |>
  filter(duration %in% c("P1D", "P1W")) |>
  distinct(reference_datetime, model_id, variable, duration, datetime, pub_datetime) |>
  group_by(variable, duration, model_id) |>
  summarize(date = max(datetime, na.rm = TRUE),
            reference_datetime = max(reference_datetime, na.rm = TRUE),
            pub_datetime = max(pub_datetime, na.rm = TRUE)) |>
  collect()

scores_model_var_min_date_df <- duckdbfs::open_dataset(paste0("s3://anonymous@",config$scores_bucket,"/bundled-parquet/project_id=",
                                                              config$project_id,"/?endpoint_override=",config$endpoint))   |>
  filter(duration %in% c("P1D", "P1W")) |>
  distinct(reference_datetime, model_id, variable, duration, datetime, pub_datetime) |>
  group_by(variable, duration, model_id) |>
  summarize(date = min(datetime, na.rm = TRUE),
            reference_datetime = min(reference_datetime, na.rm = TRUE),
            pub_datetime = min(pub_datetime, na.rm = TRUE)) |>
  collect()


scores_min_date <-  min(scores_model_var_min_date_df$date)
scores_max_date <-  max(scores_model_var_max_date_df$date)

build_description <- paste0("The catalog contains scores for the ", config$challenge_long_name,". The scores are summaries of the forecasts (i.e., mean, median, confidence intervals), matched observations (if available), and scores (metrics of how well the model distribution compares to observations). You can access the scores at the top level of the dataset where all models, variables, and dates that forecasts were produced (reference_datetime) are available. The code to access the entire dataset is provided as an asset. Given the size of the scores catalog, it can be time-consuming to access the data at the full dataset level. For quicker access to the scores for a particular model (model_id), we also provide the code to access the data at the model_id level as an asset for each model.")

# scores_sites <- scores_data_df |>
#   distinct(site_id)

if (!file.exists(paste0("../",catalog_config$scores_path))){
  dir.create(paste0("../",catalog_config$scores_path))
}

stac4cast::build_forecast_scores(table_schema = scores_theme_df,
                      #theme_id = 'Scores',
                      table_description = scores_description_create,
                      start_date = as.Date(scores_min_date),
                      end_date = as.Date(scores_max_date),
                      id_value = "daily-scores",
                      description_string = build_description,
                      about_string = catalog_config$about_string,
                      about_title = catalog_config$about_title,
                      theme_title = "Scores",
                      destination_path = catalog_config$scores_path,
                      aws_download_path = paste0(config$scores_bucket,"/bundled-parquet"),
                      link_items = stac4cast::generate_group_values(group_values = names(config$target_groups)),
                      thumbnail_link = catalog_config$scores_thumbnail,
                      thumbnail_title = catalog_config$scores_thumbnail_title,
                      group_sites = all_scores_sites,
                      model_child = FALSE)

## CREATE MODELS

## READ IN MODEL METADATA
variable_gsheet <- gsheet2tbl(config$target_metadata_gsheet)

# read in model metadata and filter for the relevant project
gsheet_read <- gsheet2tbl(config$model_metadata_gsheet)
gsheet_read$row_non_na <- rowSums(!is.na(gsheet_read))

registered_model_id <- gsheet_read |>
  filter(`What forecasting challenge are you registering for?` == config$project_id) |>
  rename(project_id = `What forecasting challenge are you registering for?`) |>
  arrange(row_non_na) |>
  distinct(model_id, project_id, .keep_all = TRUE)

## BUILD VARIABLE GROUPS

for (i in 1:length(config$target_groups)){ # LOOP OVER VARIABLE GROUPS -- BUILD FUNCTION CALLED AFTER ALL VARIABLES HAVE BEEN BUILT (AFTER SECOND LOOP)
  print(names(config$target_groups)[i])

  group_var_values <- config$target_groups[[i]]$variable

  # check data and skip if no data found
  var_group_data_check <- scores_model_var_max_date_df |>
    filter(variable %in% group_var_values) |>
    ungroup() |>
    summarise(n = n()) |>
    pull(n)

  # var_group_data_check <- arrow::open_dataset(arrow::s3_bucket(paste0(config$scores_bucket,'/bundled-parquet'),
  #                                                              endpoint_override = config$endpoint, anonymous=TRUE)) |>
  #   filter(variable %in% c(config$target_groups[[i]]$variable)) |>
  #   summarise(n = n()) |>
  #   collect()

  if (var_group_data_check == 0){
    print('No data available for group')
    next
  }

  ## REMOVE STALE OR UNUSED DIRECTORIES
  current_var_path <- paste0("../",catalog_config$scores_path,'/',names(config$target_groups[i]))
  current_var_dirs <- list.dirs(current_var_path, recursive = FALSE, full.names = TRUE)
  unlink(current_var_dirs, recursive = TRUE)

  if (!dir.exists(file.path(paste0("../",catalog_config$scores_path),names(config$target_groups[i])))){
    dir.create(file.path(paste0("../",catalog_config$scores_path),names(config$target_groups[i])))
  }

  # match variable with full name in gsheet
  var_gsheet_arrange <- variable_gsheet |>
    arrange(duration)

  var_values <- names(config$target_groups[[i]]$group_vars)

  var_name_full <- var_gsheet_arrange[which(var_gsheet_arrange$`"official" targets name` %in% var_values),1][[1]]

  ## CREATE VARIABLE GROUP JSONS
  group_description <- paste0('All variables for the ',names(config$target_groups[i]),' group.')

  ## find group sites
  find_group_sites <- scores_sites |>
    filter(variable %in% var_values) |>
    distinct(site_id) |>
    pull(site_id)


  # find_group_sites <- arrow::open_dataset(arrow::s3_bucket(paste0(config$scores_bucket,'/bundled-parquet'),
  #                                                          endpoint_override = config$endpoint, anonymous = TRUE)) |>
  #   filter(variable %in% var_values) |>
  #   distinct(site_id) |>
  #   collect()

  ## create empty vector to track publication information
  citation_build <- c()
  doi_build <- c()

  ## create empty vector to track variable information
  variable_name_build <- c()

  for(j in 1:length(config$target_groups[[i]]$group_vars)){ # FOR EACH VARIABLE WITHIN A MODEL GROUP

    var_name <- names(config$target_groups[[i]]$group_vars[j])
    print(var_name)

    for (k in 1:length(config$target_groups[[i]]$group_vars[[j]]$duration)){
        duration_value <- config$target_groups[[i]]$group_vars[[j]]$duration[k]
        print(duration_value)

        ## save original duration name for reference
        duration_name <- config$target_groups[[i]]$group_vars[[j]]$duration[k]

        ## create formal variable name
        duration_value[which(duration_value == 'P1D')] <- 'Daily'
        duration_value[which(duration_value == 'PT1H')] <- 'Hourly'
        duration_value[which(duration_value == 'PT30M')] <- '30min'
        duration_value[which(duration_value == 'P1W')] <- 'Weekly'

        var_formal_name <- paste0(duration_value,'_',var_name_full[j])

        # check data and skip if no data found
        var_data_check <- scores_model_var_max_date_df |>
          filter(variable == var_name, duration == duration_name) |>
          ungroup() |>
          summarise(n = n()) |>
          pull(n)

        if (var_data_check == 0){
            print('No data available for variable')
            next
          }

        if (!dir.exists(file.path(paste0("../",catalog_config$scores_path),names(config$target_groups)[i],var_formal_name))){
            dir.create(file.path(paste0("../",catalog_config$scores_path),names(config$target_groups)[i],var_formal_name))
        }

        var_models <- scores_model_var_max_date_df |>
          filter(variable == var_name, duration == duration_name) |>
          distinct(model_id) |>
          filter(model_id %in% registered_model_id$model_id,
                 !grepl("example",model_id)) |>
          pull(model_id)

        var_max_date <- scores_model_var_max_date_df |>
          filter(variable == var_name,
                 model_id %in% var_models,
                 duration == duration_name) |>
          summarize(date = max(date, na.rm = TRUE)) |>
          pull(date)

        var_min_date <- scores_model_var_min_date_df |>
          filter(variable == var_name,
                 model_id %in% var_models,
                 duration == duration_name) |>
          summarize(date = min(date, na.rm = TRUE)) |>
          pull(date)

        find_var_sites <- scores_sites |>
          filter(variable == var_name,
                 duration == duration) |>
          distinct(site_id) |>
          pull(site_id)

        var_metadata <- variable_gsheet |>
          filter(`"official" targets name` == var_name,
                 duration == duration_name)

        var_description <- paste0('All models for the ',var_formal_name,' variable. The variable description is as follows: ',
                                  var_metadata$Description)

        ## build lists for creating publication items
        var_citations <- config$target_groups[[i]]$group_vars[[j]]$var_citation
        doi_citations <- config$target_groups[[i]]$group_vars[[j]]$var_doi

        #update group list of publication information
        citation_build <- append(citation_build, var_citations)
        citation_build <- unique(citation_build)

        doi_build <- append(doi_build, doi_citations)
        doi_build <- unique(doi_build)

        #variable_name_build <- append(variable_name_build, var_formal_name)

        variable_name_build <- append(variable_name_build, var_formal_name)

        stac4cast::build_group_variables(table_schema = scores_theme_df,
                                        #theme_id = var_formal_name[j],
                                        table_description = scores_description_create,
                                        start_date = as.Date(var_min_date),
                                        end_date = as.Date(var_max_date),
                                        id_value = var_formal_name,
                                        description_string = var_description,
                                        about_string = catalog_config$about_string,
                                        about_title = catalog_config$about_title,
                                        dashboard_string = catalog_config$documentation_url,
                                        dashboard_title = catalog_config$catalog_title,
                                        theme_title = var_formal_name,
                                        destination_path = file.path(catalog_config$scores_path,names(config$target_groups)[i],var_formal_name),
                                        aws_download_path = paste0(config$scores_bucket,"/bundled-parquet"),
                                        group_var_items = stac4cast::generate_variable_model_items(model_list = var_models),
                                        thumbnail_link = config$target_groups[[i]]$thumbnail_link,
                                        thumbnail_title = "Thumbnail Image",
                                        group_var_vector = NULL,
                                        single_var_name = var_name,
                                        group_duration_value = duration_name,
                                        group_sites = find_var_sites,
                                        citation_values = var_citations,
                                        doi_values = doi_citations)


        #scores_sites <- c()


        ## loop over model ids and extract components if present in metadata table

        for (m in var_models){

          # make model directory
          if (!dir.exists(paste0("../",catalog_config$scores_path,'/',names(config$target_groups)[i],'/',var_formal_name,"/models"))){
            dir.create(paste0("../",catalog_config$scores_path,'/',names(config$target_groups)[i],'/',var_formal_name,"/models"))
          }

          print(m)

          model_max_date <- scores_model_var_max_date_df |>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            summarize(date = max(date)) |>
            pull(date)

          model_min_date <- scores_model_var_min_date_df |>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            summarize(date = max(date)) |>
            pull(date)

          model_reference_date <- scores_model_var_max_date_df |>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            summarize(date = max(reference_datetime)) |>
            pull(date)

          model_pub_date <- scores_model_var_max_date_df |>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            summarize(date = max(pub_datetime)) |>
            pull(date)

          if(is.na(model_pub_date)){
            model_pub_date <- model_reference_date
          }

          model_var_duration_df <- scores_model_var_max_date_df|>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            distinct(variable,duration) |>
            mutate(duration_name = ifelse(duration == 'P1D', 'Daily', duration)) |>
            mutate(duration_name = ifelse(duration == 'PT1H', 'Hourly', duration_name)) |>
            mutate(duration_name = ifelse(duration == 'PT30M', '30min', duration_name)) |>
            mutate(duration_name = ifelse(duration == 'P1W', 'Weekly', duration_name)) |>
            ungroup()

          model_var_full_name <- model_var_duration_df |>
            left_join((variable_gsheet |>
                         select(variable = `"official" targets name`, full_name = `Variable name`) |>
                         distinct(variable, .keep_all = TRUE)), by = c('variable'))

          model_var_full_name <- model_var_duration_df |>
            left_join((variable_gsheet |>
                         select(variable = `"official" targets name`, full_name = `Variable name`) |>
                         distinct(variable, .keep_all = TRUE)), by = c('variable'))

          model_sites <- scores_sites |>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            distinct(site_id) |>
            pull(site_id)

          model_site_text <- paste(as.character(model_sites), sep="' '", collapse=", ")

          model_vars <- scores_model_var_max_date_df |>
            ungroup() |>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            distinct(variable) |>
            left_join(model_var_full_name, by = 'variable')

          model_vars$var_duration_name <- paste0(model_vars$duration_name, " ", model_vars$full_name)
          model_vars$project_id <- config$project_id

          #scores_sites <- append(scores_sites,  stac4cast::get_site_coords(site_metadata = catalog_config$site_metadata_url,
          #                                                                 sites = model_sites))

          stac_id <- paste0(m,'_',var_name,'_',duration_name,'_scores')

          idx = which(registered_model_id$model_id == m)

          if (is.null(registered_model_id$`Web link to model code`[idx])){
            model_code_link <- 'https://projects.ecoforecast.org/neon4cast-ci/'
          } else{
            model_code_link <- registered_model_id$`Web link to model code`[idx]
          }

          model_description <- paste0("All scores for the ",
                                      var_formal_name,
                                      ' variable for the ',
                                      m,
                                      ' model. Information for the model is provided as follows: ',
                                      registered_model_id[idx,"Describe your modeling approach in your own words."][[1]],
                                      '.
                                    The model predicts this variable at the following sites: ',
                                    model_site_text,
                                    '.
                                    Scores are metrics that describe how well forecasts compare to observations. The scores catalog includes are summaries of the forecasts (i.e., mean, median, confidence intervals), matched observations (if available), and scores (metrics of how well the model distribution compares to observations)')

          model_keywords <- c(list('Scores',config$project_id, names(config$target_groups)[i], m, var_name_full[j], var_name, duration_value, duration_name),
                              as.list(model_sites))

          ## build radiantearth stac and raw json link
          stac_link <- paste0("https://radiantearth.github.io/stac-browser/#/external/raw.githubusercontent.com/",
                              config$github_repo,
                              "/main/scores/",
                              names(config$target_groups)[i],'/',
                              var_formal_name, '/models/',
                              m,'.json')

          json_link <- paste0('https://raw.githubusercontent.com/',
                              config$github_repo,
                              '/main/scores/',
                              names(config$target_groups)[i],'/',
                              var_formal_name, '/models/',
                              m,'.json')

          stac4cast::build_model(model_id = m,
                                 stac_id = stac_id,
                                 team_name = registered_model_id$`Long name of the model (can include spaces)`[idx],
                                 model_description = model_description,
                                 start_date = as.Date(model_min_date),
                                 end_date = as.Date(model_max_date),
                                 pub_date = as.Date(model_pub_date),
                                 forecast_date = model_reference_date,
                                 var_values = model_vars$var_duration_name,
                                 duration_names = model_var_duration_df$duration,
                                 duration_value = duration_name,
                                 site_values = model_sites,
                                 site_table = catalog_config$site_metadata_url,
                                 model_documentation = registered_model_id,
                                 destination_path = paste0(catalog_config$scores_path,'/',names(config$target_groups)[i],'/',var_formal_name,"/models"),
                                 aws_download_path = paste0(config$scores_bucket,"/bundled-parquet"),
                                 collection_name = 'scores',
                                 thumbnail_image_name = NULL,
                                 table_schema = scores_theme_df,
                                 table_description = scores_description_create,
                                 full_var_df = model_vars,
                                 code_web_link = model_code_link,
                                 model_keywords = model_keywords,
                                 stac_web_link = stac_link,
                                 raw_json_link = json_link)
        } ## end model loop

            } ## end duration loop

  } ## end variable loop

  group_max_date <- scores_model_var_max_date_df |>
    filter(variable %in% var_values) |>
    ungroup() |>
    summarize(date = max(date)) |>
    pull(date)

  group_min_date <- scores_model_var_min_date_df |>
    filter(variable %in% var_values) |>
    ungroup() |>
    summarize(date = min(date)) |>
    pull(date)

  ## BUILD THE GROUP PAGES WITH UPDATED VAR/PUB INFORMATION
  stac4cast::build_group_variables(table_schema = scores_theme_df,
                      table_description = scores_description_create,
                      start_date = group_min_date,
                      end_date = group_max_date,
                      id_value = names(config$target_groups)[i],
                      description_string = group_description,
                      about_string = catalog_config$about_string,
                      about_title = catalog_config$about_title,
                      dashboard_string = catalog_config$documentation_url,
                      dashboard_title = catalog_config$catalog_title,
                      theme_title = names(config$target_groups[i]),
                      destination_path = file.path(catalog_config$scores_path,names(config$target_groups)[i]),
                      aws_download_path = paste0(config$scores_bucket,"/bundled-parquet"),
                      group_var_items = stac4cast::generate_group_variable_items(variables = variable_name_build),
                      thumbnail_link = config$target_groups[[i]]$thumbnail_link,
                      thumbnail_title = config$target_groups[[i]]$thumbnail_title,
                      group_var_vector = unique(var_values),
                      group_duration_value = NULL,
                      single_var_name = NULL,
                      group_sites = find_group_sites,
                      citation_values = citation_build,
                      doi_values = doi_build)
} # end group loop
