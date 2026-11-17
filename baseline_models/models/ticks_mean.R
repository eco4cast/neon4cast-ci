
library(fable)
library(distributional)
library(tidyverse)

efi_statistic_format <- function(df){
  ## determine variable name
  var <- attributes(df)$dist
  ## Normal distribution: use distribution mean and variance
  df %>%
    dplyr::mutate(sigma = sqrt( distributional::variance( .data[[var]] ) ) ) %>%
    dplyr::rename(mu = .mean) %>%
    dplyr::select(datetime, site_id, .model, mu, sigma) %>%
    tidyr::pivot_longer(c(mu, sigma), names_to = "parameter", values_to = var) %>%
    pivot_longer(tidyselect::all_of(var), names_to="variable", values_to = "prediction") |>
    mutate(family = "normal")
}

## Get the latest beetle target data.
url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1W/ticks-targets.csv.gz"
download.file(url,
              "ticks-targets.csv.gz")
targets <-  read_csv("ticks-targets.csv.gz")

curr_iso_week <- ISOweek::ISOweek(Sys.Date())

curr_date <- ISOweek::ISOweek2date(paste0(curr_iso_week, "-1"))

site_list <- unique(targets$site_id)

last_day <- tibble(site_id = site_list,
                   datetime = rep(curr_date, length(site_list)),
                   variable = "amblyomma_americanum",
                   observation = NA)



targets <- targets |>
  filter(variable == "amblyomma_americanum") |>
  bind_rows(last_day) |>
  select(-variable) |>
  mutate(datetime = as_date(datetime)) |>
  as_tsibble(index = datetime, key = site_id)

team_name <- "mean"

## a single mean per site... obviously silly
forecast <- targets  %>%
  model(null = MEAN(log(observation + 1))) %>%
  generate(times = 500, h = "1 year", bootstrap = TRUE) |>
  # Standard EFI columns: the ensemble member goes in `parameter` with
  # family "ensemble". Without family and parameter the forecast fails
  # validation, so none of this model's forecasts reached the bundle.
  dplyr::rename(parameter = .rep,
                prediction = .sim) |>
  mutate(variable = "amblyomma_americanum",
         family = "ensemble") |>
  mutate(reference_datetime = lubridate::as_date(min(datetime)) - lubridate::weeks(1),
         model_id = team_name) |>
  select(model_id, datetime, reference_datetime, site_id, family, parameter, variable, prediction) |>
  as_tibble() |>
  mutate(datetime = as_datetime(datetime),
         reference_datetime = as_datetime(reference_datetime),
         project_id = "neon4cast",
         duration = "P1W")


## Create the metadata record, see metadata.Rmd
theme_name <- "ticks"
file_date <- as_date(forecast$reference_datetime[1])

filename <- paste0(theme_name, "-", file_date, "-", team_name, ".csv.gz")

## Store the forecast products
readr::write_csv(forecast, filename)

neon4cast::submit(forecast_file = filename,
                  ask = FALSE)

unlink(c(filename, "ticks-targets.csv.gz"))


