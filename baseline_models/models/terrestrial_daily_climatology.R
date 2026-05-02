#'# Ecological Forecasting Initiative Null Model 

#'## Set-up

print(paste0("Running Creating Daily Terrestrial Forecasts at ", Sys.time()))

#'Load renv.lock file that includes the versions of all the packages used
#'You can generate using the command renv::snapshot()

#' Required packages.  
#' EFIstandards is at remotes::install_github("eco4cast/EFIstandards")
library(tidyverse)
library(lubridate)
library(imputeTS)


#' set the random number for reproducible MCMC runs
set.seed(329)

#'Generate plot to visualized forecast
generate_plots <- TRUE
#'Is the forecast run on the Ecological Forecasting Initiative Server?
#'Setting to TRUE published the forecast on the server.
efi_server <- TRUE

#' List of team members. Used in the generation of the metadata
#team_list <- list(list(individualName = list(givenName = "Quinn", surName = "Thomas"), 
#                       id = "https://orcid.org/0000-0003-1282-7825"),
#                  list(individualName = list(givenName = "Others",  surName ="Pending")),
#)

#'Team name code
team_name <- "climatology"

#'Read in target file.  The guess_max is specified because there could be a lot of
#'NA values at the beginning of the file

url_P1D <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/terrestrial_daily-targets.csv.gz"

targets <- readr::read_csv(url_P1D, guess_max = 10000)

sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(terrestrial == 1)

site_names <- sites$field_site_id

target_clim <- targets %>%  
  mutate(doy = yday(datetime)) %>% 
  group_by(doy, site_id, variable) %>% 
  summarise(clim_mean = mean(observation, na.rm = TRUE),
            clim_sd = sd(observation, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(clim_mean = ifelse(is.nan(clim_mean), NA, clim_mean))

#curr_month <- month(Sys.Date())
curr_month <- month(Sys.Date())
if(curr_month < 10){
  curr_month <- paste0("0", curr_month)
}

curr_year <- year(Sys.Date())
start_date <- Sys.Date() + days(1)

forecast_dates <- seq(start_date, as_date(start_date + days(35)), "1 day")
forecast_doy <- yday(forecast_dates)

forecast_dates_df <- tibble(datetime = forecast_dates,
                            doy = forecast_doy)

forecast <- target_clim %>%
  mutate(doy = as.integer(doy)) %>% 
  filter(doy %in% forecast_doy) %>% 
  full_join(forecast_dates_df, by = 'doy') %>%
  arrange(site_id, datetime)

subseted_site_names <- unique(forecast$site_id)
site_vector <- NULL
for(i in 1:length(subseted_site_names)){
  site_vector <- c(site_vector, rep(subseted_site_names[i], length(forecast_dates)))
}

forecast_tibble1 <- tibble(datetime = rep(forecast_dates, length(subseted_site_names)),
                          site_id = site_vector,
                          variable = "nee")

forecast_tibble2 <- tibble(datetime = rep(forecast_dates, length(subseted_site_names)),
                          site_id = site_vector,
                          variable = "nee")

forecast_tibble <- bind_rows(forecast_tibble1, forecast_tibble2)

foreast <- right_join(forecast, forecast_tibble)

site_count <- forecast %>% 
  select(datetime, site_id, variable, clim_mean, clim_sd) %>% 
  filter(!is.na(clim_mean)) |> 
  group_by(site_id, variable) %>% 
  summarize(count = n(), .groups = "drop") |> 
  filter(count > 2) |> 
  distinct() |> 
  pull(site_id)
  




combined <- forecast %>% 
  filter(site_id %in% site_count) |> 
  select(datetime, site_id, variable, clim_mean, clim_sd) %>% 
  rename(mean = clim_mean,
         sd = clim_sd) %>% 
  group_by(site_id, variable) %>% 
  mutate(mu = imputeTS::na_interpolation(x = mean),
         sigma = median(sd, na.rm = TRUE))
combined <- combined %>%
  pivot_longer(c("mu", "sigma"),names_to = "parameter", values_to = "prediction") |> 
  mutate(family = "normal") |> 
  mutate(reference_datetime = min(combined$datetime) - lubridate::days(1),
         model_id = "climatology") |> 
  select(model_id, datetime, reference_datetime, site_id, variable, family, parameter, prediction)

combined %>% 
  filter(variable == "nee") |> 
  pivot_wider(names_from = parameter, values_from = prediction) %>% 
  ggplot(aes(x = datetime)) +
  geom_ribbon(aes(ymin=mu - sigma*1.96, ymax=mu + sigma*1.96), alpha = 0.1) + 
  geom_point(aes(y = mu)) +
  facet_wrap(~site_id)

file_date <- combined$reference_datetime[1]

forecast_file <- paste("terrestrial_daily", file_date, "climatology.csv.gz", sep = "-")

write_csv(combined, forecast_file)

neon4cast::submit(forecast_file = forecast_file, 
                  metadata = NULL, 
                  ask = FALSE)

unlink(forecast_file)



