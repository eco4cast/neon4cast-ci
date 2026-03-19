library(arrow)
library(dplyr)
library(gsheet)
library(readr)
library(duckdbfs)

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
forecast_theme_df <- arrow::open_dataset(arrow::s3_bucket((config$forecasts_bucket), endpoint_override = config$endpoint, anonymous = TRUE)) #|>


message('forecast sites...')

site_metadata <- read_csv(catalog_config$site_metadata_url) |>
  distinct(field_site_id) |>
  pull(field_site_id)

# forecast_sites <- c("BARC", "USGS-14181500", "USGS-05543010", "CARI", "LEWI","ORNL","SOAP", "BIGC","BLDE","BLUE",
#                     "USGS-14211010", "NIWO","NOGP","TEAK","LENO","MLBS","TREE","WALK","SUGG","MCDI","COMO", "USGS-01427510",
#                     "USGS-05553700", "HARV","DSNY","GUAN","LAJA","POSE", "SCBI","DCFS","KONZ","OAES","HOPB","TOOK","USGS-01463500",
#                     "USGS-05558300", "MOAB","PUUM","SERC","SJER","WOOD","ARIK", "GUIL","PRIN","LIRO","USGS-05586300", "BART",
#                     "JERC","KONA", "ONAQ","UNDE","REDB","FLNT","STEI","UKFS","CLBJ", "BONA","BLAN","OKSR","BLWA","STER","CUPE",
#                     "KING", "MAYF","PRLA","PRPO","BARR","OSBS","TALL","TOOL", "TOMB","USGS-14211720", "RMNP","SRER","TECR","DEJU",
#                     "JORN", "YELL","MCRA", "DELA","CPER","HEAL","MART","WLOU","CRAM","GRSM","ABBY","WREF","LECO","SYCA" )

message('forecast dates...')

forecast_sites <- duckdbfs::open_dataset(paste0("s3://anonymous@",config$summaries_bucket,"/bundled-summaries/project_id=",
                                                config$project_id,"/?endpoint_override=",config$endpoint))   |>
  filter(duration %in% c("P1D", "P1W"),
         site_id %in% site_metadata,
         !is.na(model_id)) |>
  distinct(model_id, variable, duration, site_id) |>
  collect()

all_forecast_sites <- unique(forecast_sites$site_id)

forecast_model_var_max_date_df <- duckdbfs::open_dataset(paste0("s3://anonymous@",config$summaries_bucket,"/bundled-summaries/project_id=",
                                                                config$project_id,"/?endpoint_override=",config$endpoint))   |>
  filter(duration %in% c("P1D", "P1W")) |>
  distinct(reference_datetime, model_id, variable, duration, datetime, pub_datetime) |>
  group_by(variable, duration, model_id) |>
  summarize(date = max(datetime, na.rm = TRUE),
            reference_datetime = max(reference_datetime, na.rm = TRUE),
            pub_datetime = max(pub_datetime, na.rm = TRUE)) |>
  collect()

forecast_model_var_min_date_df <- duckdbfs::open_dataset(paste0("s3://anonymous@",config$scores_bucket,"/bundled-parquet/project_id=",
                                                                config$project_id,"/?endpoint_override=",config$endpoint))   |>
  filter(duration %in% c("P1D", "P1W")) |>
  distinct(reference_datetime, model_id, variable, duration, datetime, pub_datetime) |>
  group_by(variable, duration, model_id) |>
  summarize(date = min(datetime, na.rm = TRUE),
            reference_datetime = min(reference_datetime, na.rm = TRUE),
            pub_datetime = min(pub_datetime, na.rm = TRUE)) |>
  collect()

forecast_min_date <-  min(forecast_model_var_min_date_df$date)
forecast_max_date <-  max(forecast_model_var_max_date_df$date)

build_description <- paste0("Forecasts are the raw forecasts that includes all ensemble members or distribution parameters. Due to the size of the raw forecasts, we recommend accessing the scores (summaries of the forecasts) to analyze forecasts (unless you need the individual ensemble members). You can access the forecasts at the top level of the dataset where all models, variables, and dates that forecasts were produced (reference_datetime) are available. The code to access the entire dataset is provided as an asset. Given the size of the forecast catalog, it can be time-consuming to access the data at the full dataset level. For quicker access to the forecasts for a particular model (model_id), we also provide the code to access the data at the model_id level as an asset for each model.")

if (!file.exists(paste0("../",catalog_config$forecast_path))){
  dir.create(paste0("../",catalog_config$forecast_path))
}

stac4cast::build_forecast_scores(table_schema = forecast_theme_df,
                      #theme_id = 'Forecasts',
                      table_description = forecast_description_create,
                      start_date = as.Date(forecast_min_date),
                      end_date = as.Date(forecast_max_date),
                      id_value = "daily-forecasts",
                      description_string = build_description,
                      about_string = catalog_config$about_string,
                      about_title = catalog_config$about_title,
                      theme_title = "Forecasts",
                      destination_path = catalog_config$forecast_path,
                      aws_download_path = paste0(config$forecasts_bucket,"/bundled-parquet"),
                      link_items = stac4cast::generate_group_values(group_values = names(config$target_groups)),
                      thumbnail_link = catalog_config$forecasts_thumbnail,
                      thumbnail_title = catalog_config$forecasts_thumbnail_title,
                      group_sites = all_forecast_sites,
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

for (i in 1:length(config$target_groups)){ ## organize variable groups
  print(names(config$target_groups)[i])

  group_var_values <- config$target_groups[[i]]$variable

  # check data and skip if no data found
  var_group_data_check <- forecast_model_var_max_date_df |>
    filter(variable %in% group_var_values) |>
    ungroup() |>
    summarise(n = n()) |>
    pull(n)

  if (var_group_data_check == 0){
    print('No data available for group')
    next
  }

  ## REMOVE STALE OR UNUSED DIRECTORIES
  current_var_path <- paste0("../",catalog_config$forecast_path,'/',names(config$target_groups[i]))
  current_var_dirs <- list.dirs(current_var_path,recursive = FALSE, full.names = TRUE)
  unlink(current_var_dirs, recursive = TRUE)

  if (!dir.exists(paste0("../",catalog_config$forecast_path,'/',names(config$target_groups[i])))){
    dir.create(paste0("../",catalog_config$forecast_path,'/',names(config$target_groups[i])))
  }

  # match variable with full name in gsheet
  var_gsheet_arrange <- variable_gsheet |>
    arrange(duration)

  var_values <- names(config$target_groups[[i]]$group_vars)

  var_name_full <- var_gsheet_arrange[which(var_gsheet_arrange$`"official" targets name` %in% var_values),1][[1]]

  ## CREATE VARIABLE GROUP JSONS
  group_description <- paste0('All variables for the ',names(config$target_groups[i]),' group.')

  ## find group sites
  find_group_sites <- forecast_sites |>
    filter(variable %in% var_values) |>
    distinct(site_id) |>
    pull(site_id)

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
      var_data_check <- forecast_model_var_max_date_df |>
        filter(variable == var_name, duration == duration_name) |>
        ungroup() |>
        summarise(n = n()) |>
        pull(n)

      if (var_data_check == 0){
        print('No data available for variable')
        next
      }

      if (!dir.exists(file.path(paste0("../",catalog_config$forecast_path),names(config$target_groups)[i],var_formal_name))){
        dir.create(file.path(paste0("../",catalog_config$forecast_path),names(config$target_groups)[i],var_formal_name))
      }


      var_models <- forecast_model_var_max_date_df |>
        filter(variable == var_name, duration == duration_name) |>
        distinct(model_id) |>
        filter(model_id %in% registered_model_id$model_id,
               !grepl("example",model_id)) |>
        pull(model_id)

      var_date_max <- forecast_model_var_max_date_df |>
        filter(variable == var_name,
               model_id %in% var_models,
               duration == duration_name) |>
        summarize(date = max(date, na.rm = TRUE))

      var_date_min <- forecast_model_var_min_date_df |>
        filter(variable == var_name,
               model_id %in% var_models,
               duration == duration_name) |>
        summarize(date = min(date, na.rm = TRUE))

      var_min_date <- var_date_min$date
      var_max_date <- var_date_max$date

      find_var_sites <- forecast_sites |>
        filter(variable == var_name,
               duration == duration) |>
        distinct(site_id) |>
        pull(site_id)

      var_metadata <- variable_gsheet |>
        filter(`"official" targets name` == var_name,
               duration == duration_name)

      var_description <- paste0('All models for the ',var_formal_name,' variable. The variable description is as follows: ',
                                var_metadata$Description)

      #var_path <- gsub('forecasts','scores',var_data$path[1])
      #var_path <- var_data$path[1]

      ## build lists for creating publication items
      var_citations <- config$target_groups[[i]]$group_vars[[j]]$var_citation
      doi_citations <- config$target_groups[[i]]$group_vars[[j]]$var_doi

      #update group list of publication information
      citation_build <- append(citation_build, var_citations)
      citation_build <- unique(citation_build)

      doi_build <- append(doi_build, doi_citations)
      doi_build <- unique(doi_build)

      variable_name_build <- append(variable_name_build, var_formal_name)

      #variable_name_build <- append(variable_name_build, var_formal_name)

      stac4cast::build_group_variables(table_schema = forecast_theme_df,
                                       #theme_id = var_formal_name[j],
                                       table_description = forecast_description_create,
                                       start_date = as.Date(var_min_date),
                                       end_date = as.Date(var_max_date),
                                       id_value = var_formal_name,
                                       description_string = var_description,
                                       about_string = catalog_config$about_string,
                                       about_title = catalog_config$about_title,
                                       dashboard_string = catalog_config$documentation_url,
                                       dashboard_title = catalog_config$catalog_title,
                                       theme_title = var_formal_name,
                                       destination_path = file.path(catalog_config$forecast_path,names(config$target_groups)[i],var_formal_name),
                                       aws_download_path = paste0(config$forecasts_bucket,"/bundled-parquet"),
                                       group_var_items = stac4cast::generate_variable_model_items(model_list = var_models),
                                       thumbnail_link = config$target_groups[[i]]$thumbnail_link,
                                       thumbnail_title = "Thumbnail Image",
                                       group_var_vector = NULL,
                                       single_var_name = var_name,
                                       group_duration_value = duration_name,
                                       group_sites = find_var_sites,
                                       citation_values = var_citations,
                                       doi_values = doi_citations)

      ## LOOP OVER MODEL IDS AND CREATE JSONS
      for (m in var_models){

        if (!(m %in% registered_model_id$model_id)){
          message(paste0('Omitting model_id "',m,'" due to missing registration'))
          next
        }

        # make model items directory
        if (!dir.exists(paste0("../",catalog_config$forecast_path,'/',names(config$target_groups)[i],'/',var_formal_name,"/models"))){
          dir.create(paste0("../",catalog_config$forecast_path,'/',names(config$target_groups)[i],'/',var_formal_name,"/models"))
        }

        print(m)

        model_max_date <- forecast_model_var_max_date_df |>
          filter(model_id == m,
                variable == var_name,
                duration == duration_name) |>
          summarize(date = max(date)) |>
          pull(date)

        model_min_date <- forecast_model_var_min_date_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          summarize(date = max(date)) |>
          pull(date)

        if (length(model_min_date) == 0){ ## add check for models that are missing from scores
          model_min_date <- forecast_model_var_max_date_df |>
            filter(model_id == m,
                   variable == var_name,
                   duration == duration_name) |>
            summarize(date = min(date)) |>
            pull(date)
        }

        model_reference_date <- forecast_model_var_max_date_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          summarize(date = max(reference_datetime)) |>
          pull(date)

        model_pub_date <- forecast_model_var_max_date_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          summarize(date = max(pub_datetime)) |>
          pull(date)


        if(is.na(model_pub_date)){
          model_pub_date <- model_reference_date
        }

        model_var_duration_df <- forecast_model_var_max_date_df|>
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
                       distinct(variable, .keep_all = TRUE)), by = c('variable')) |>
          select(variable, duration_name, full_name)

        model_sites <- forecast_sites |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          distinct(site_id) |>
          pull(site_id)

        model_site_text <- paste(as.character(model_sites), sep="' '", collapse=", ")

        model_vars <- forecast_model_var_max_date_df |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          distinct(variable) |>
          left_join(model_var_full_name, by = 'variable')

        model_vars$var_duration_name <- paste0(model_vars$duration_name, " ", model_vars$full_name)
        model_vars$project_id = config$project_id

        # forecast_sites <- append(forecast_sites,  stac4cast::get_site_coords(site_metadata = catalog_config$site_metadata_url,
        #                                                                      sites = model_sites))

        idx = which(registered_model_id$model_id == m)

        stac_id <- paste0(m,'_',var_name,'_',duration_name,'_forecast')

        if (is.null(registered_model_id$`Web link to model code`[idx]) |
            identical(registered_model_id$`Web link to model code`[idx], character(0)) |
            is.na(registered_model_id$`Web link to model code`[idx])){
          model_code_link <- config$challenge_url
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

        model_keywords <- c(list('Forecasts',config$project_id, names(config$target_groups)[i], m, var_name_full[j], var_name, duration_value, duration_name),
                            as.list(model_sites))

        ## build radiantearth stac and raw json link
        stac_link <- paste0("https://radiantearth.github.io/stac-browser/#/external/raw.githubusercontent.com/",
                            config$github_repo,
                            "/main/forecasts/",
                            names(config$target_groups)[i],'/',
                            var_formal_name, '/models/',
                            m,'.json')

        json_link <- paste0('https://raw.githubusercontent.com/',
                            config$github_repo,
                            '/main/forecasts/',
                            names(config$target_groups)[i],'/',
                            var_formal_name, '/models/',
                            m,'.json')

        stac4cast::build_model(model_id = m,
                               stac_id = stac_id,
                               team_name = registered_model_id$`Long name of the model (can include spaces)`[idx],
                               #model_description = registered_model_id[idx,"Describe your modeling approach in your own words."][[1]],
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
                               destination_path = paste0(catalog_config$forecast_path,'/',names(config$target_groups)[i],'/',var_formal_name,"/models"),
                               aws_download_path = paste0(config$forecasts_bucket,"/bundled-parquet"),
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

  group_max_date <- forecast_model_var_max_date_df |>
    filter(variable %in% var_values) |>
    ungroup() |>
    summarize(date = max(date)) |>
    pull(date)

  group_min_date <- forecast_model_var_min_date_df |>
    filter(variable %in% var_values) |>
    ungroup() |>
    summarize(date = min(date)) |>
    pull(date)

  ## BUILD THE GROUP PAGES WITH UPDATED VAR/PUB INFORMATION
  stac4cast::build_group_variables(table_schema = forecast_theme_df,
                                   table_description = forecast_description_create,
                                   start_date = as.Date(group_min_date),
                                   end_date = as.Date(group_max_date),
                                   id_value = names(config$target_groups)[i],
                                   description_string = group_description,
                                   about_string = catalog_config$about_string,
                                   about_title = catalog_config$about_title,
                                   dashboard_string = catalog_config$documentation_url,
                                   dashboard_title = catalog_config$catalog_title,
                                   theme_title = names(config$target_groups[i]),
                                   destination_path = file.path(catalog_config$forecast_path,names(config$target_groups)[i]),
                                   aws_download_path = paste0(config$forecasts_bucket,"/bundled-parquet"),
                                   group_var_items = stac4cast::generate_group_variable_items(variables = variable_name_build),
                                   thumbnail_link = config$target_groups[[i]]$thumbnail_link,
                                   thumbnail_title = config$target_groups[[i]]$thumbnail_title,
                                   group_var_vector = unique(var_values),
                                   single_var_name = NULL,
                                   group_duration_value = NULL,
                                   group_sites = find_group_sites,
                                   citation_values = citation_build,
                                   doi_values = doi_build)
} # end group loop
