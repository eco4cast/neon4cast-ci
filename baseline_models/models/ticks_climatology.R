#=====================================================#
# This script creates the null model for the Tick
# Population challenge
#
# The model is historcial means for each week at each site
#=====================================================#

library(tidyverse)
library(lubridate)

# first load the target data set
url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1W/ticks-targets.csv.gz"
data <- read_csv(url, guess_max = 1e6)
sites <- data %>%
  pull(site_id) %>%
  unique()


# the weeks we need to forecast
curr_iso_week <- as.numeric(stringr::str_sub(ISOweek::ISOweek(Sys.Date()),7, 8))
curr_year <- lubridate::year(Sys.Date())
curr_week <-  ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(Sys.Date()), "-1"))
fx.datetime <- seq.Date(curr_week, by = 7, length.out = 52) # all Sundays in 2021

# Forecast is weekly mean and sd by site
hist_means <- function(df, target.weeks){

  focal_weeks <- (as.numeric(stringr::str_sub(ISOweek::date2ISOweek(target.weeks), 7, 8)))

  weekly.means <- df %>%
    mutate(iso_week = ISOweek::date2ISOweek(datetime)) |>
    mutate(iso_week = as.numeric(stringr::str_sub(iso_week, 7, 8))) |>
    group_by(site_id, iso_week) %>%
    summarise(mean = mean(observation),
              sd = sd(observation))


  # need to fill in NAs and need to do on all data before sub-setting to target weeks.
  # Cover every ISO week (1-53): the forecast spans a full year, and stopping
  # at week 44 silently dropped weeks 45-53 from every forecast.
  all_iso_weeks <- 1:53
  filler.tb <- tibble(site_id = rep(sites, length(all_iso_weeks)),
                      iso_week = rep(all_iso_weeks, each = length(sites)))

  all.weeks <- left_join(filler.tb, weekly.means, by = c("site_id", "iso_week")) %>%
    arrange(site_id, iso_week)

  # not all weeks will be accounted for - linearly interpolate if missing
  gap.filled <- all.weeks %>%
    group_by(site_id) %>%
    mutate(mean = approx(x = iso_week, y = mean, xout = iso_week, rule = 2)$y,
           sd = replace_na(sd, mean(sd, na.rm = TRUE)),
           sd = replace(sd, sd == 0, mean(sd))) |>
    filter(iso_week  %in% focal_weeks)

  # now each week has a mean - sort of
  return(gap.filled)
}



create_ensembles <- function(df, nmc = 500, forecast.year = 2021) {

  # simulate error from the log normal (to keep the zero bound)
  # need to calculate meanLog and sdLog from the normal mean and sd
  log_norm_sim <- function(df, i){
    mu <- df$mean[[i]] + 1
    sd <- df$sd[[i]]
    meanLog <- log(mu^2 / sqrt(sd^2 + mu^2))
    sdLog <- sqrt(log(1 + (sd^2 / mu^2)))
    sim <- data.frame(
      site_id = df$site_id[[i]],
      iso_week = df$iso_week[[i]],
      ensemble = 1:nmc,
      density = rlnorm(nmc, meanLog, sdLog)
    )
    return(sim)
  }

  # make ensemble data frame, create/rename columns as needed
  ens <- 1:nrow(df) %>%
    map_dfr(~ log_norm_sim(df, .x)) %>%
    as_tibble() %>%
    mutate(year = ifelse(iso_week < curr_iso_week, curr_year + 1, curr_year),
           iso_week = stringr::str_pad(iso_week, width = 2,side = "left",pad = 0),
           iso_week = paste0(year, "-W",iso_week,"-1"),
           datetime = ISOweek::ISOweek2date(iso_week))

  return(ens)

}

# get the forecasts we want
forecast <- hist_means(data, fx.datetime)

# generate ensembles for uncertainty and scoring. Use the default 500 members:
# passing curr_iso_week positionally set nmc, so the ensemble size was the
# current week number (1-53).
ensembles <- create_ensembles(forecast)

team_name <- "EFI_avg_null"

# finalize for EFI submission
forecast.submit <- ensembles %>%
  select(-year, -iso_week) %>%
  rename(prediction = density,
         parameter = ensemble) |>
  mutate(variable = "amblyomma_americanum",
         family = "ensemble") |>
  mutate(reference_datetime = lubridate::as_date(min(datetime)) - lubridate::weeks(1),
         model_id = team_name) |>
  select(model_id, datetime, reference_datetime, site_id, variable, family, parameter, prediction) |>
  arrange(site_id, datetime) |>
  mutate(datetime = as_datetime(datetime),
         reference_datetime = as_datetime(reference_datetime),
         project_id = "neon4cast",
         duration = "P1W")

ggplot(forecast.submit, aes(x = datetime, y = prediction)) +
  geom_point() +
  facet_wrap(~site_id)

ggplot(data, aes(x = datetime, y = observation)) +
  geom_point() +
  facet_wrap(~site_id)

# Save file as CSV in the EFI format
# [theme_name]-[time]-[team_name].csv
theme_name <- "ticks"
file_date <- as_date(forecast.submit$reference_datetime[1])
file.name <- paste0(theme_name, "-", file_date, "-", team_name, ".csv.gz")

write_csv(forecast.submit, file.name)

neon4cast::submit(forecast_file = file.name,
                  metadata = NULL,
                  ask = FALSE)

unlink(file.name)
