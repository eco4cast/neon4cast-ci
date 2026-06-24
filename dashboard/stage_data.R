library(dplyr)
library(duckdbfs)
library(lubridate)

# OSN (sdsc.osn.xsede.org) intermittently times out / throttles on the large
# bundled-parquet listings. Extend the httpfs timeout and add retries so a slow
# response doesn't abort the whole render.
local({
  con <- duckdbfs::cached_connection()
  try(DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;"), silent = TRUE)
  for (s in c("SET http_timeout=600000",     # 10 min (ms), default 30s
              "SET http_retries=5",           # default 3
              "SET http_retry_wait_ms=500",
              "SET http_keep_alive=true")) {
    try(DBI::dbExecute(con, s), silent = TRUE)
  }
})

config <- yaml::read_yaml("challenge_configuration.yaml")
sites <- open_dataset(config$catalog_config$site_metadata_url) |>
  rename(site_id = field_site_id)

message("P1D forecast summaries")

s3_summaries_P1D <- open_dataset(paste0("s3://", config$summaries_bucket, "/bundled-summaries/project_id=", config$project_id,"/duration=P1D/"), s3_endpoint = config$endpoint, anonymous = TRUE)
cutoff <- Sys.Date() - lubridate::days(2)


reference_datetimes_P1D <- s3_summaries_P1D |>
  select(reference_datetime, variable) |>
  filter(reference_datetime < cutoff) |>
  summarize(reference_datetime = max(reference_datetime), .by = "variable")

df_P1D <- s3_summaries_P1D |>
  select(-project_id, -family, -sd, -duration, -pub_datetime) |>
  filter(datetime > reference_datetime) |>
  inner_join(reference_datetimes_P1D, by = join_by(reference_datetime, variable)) |>
  inner_join(sites, by = "site_id") |>
  write_dataset("forecasts_P1D.parquet")

message("P1W forecast summaries")

s3_summaries_P1W <- open_dataset(paste0("s3://", config$summaries_bucket, "/bundled-summaries/project_id=", config$project_id,"/duration=P1W/"), s3_endpoint = config$endpoint, anonymous = TRUE)

reference_datetimes_P1W <- s3_summaries_P1W |>
  select(reference_datetime, variable) |>
  filter(reference_datetime < cutoff) |>
  summarize(reference_datetime = max(reference_datetime), .by = "variable")

s3_summaries_P1W |>
  select(-project_id, -family, -sd, -duration, -pub_datetime) |>
  filter(datetime > reference_datetime) |>
  inner_join(reference_datetimes_P1W, by = join_by(reference_datetime, variable)) |>
  inner_join(sites, by = "site_id") |>
  write_dataset("forecasts_P1W.parquet")

message("P1D scores")

s3_scores_P1D <- open_dataset(paste0("s3://", config$scores_bucket,"/bundled-parquet/project_id=",  config$project_id,"/duration=P1D/"), s3_endpoint = config$endpoint, anonymous = TRUE)

cutoff <- Sys.Date() - lubridate::days(60)
cutoff2 <- Sys.Date() - lubridate::days(29)
#df <- s3_scores_P1D |>
#  select(-project_id, -family, -sd, -duration, -pub_datetime) |>
#  group_by(variable, model_id) |>
#  summarize(max = max(reference_datetime)) |>
#  arrange(desc(max)) |>
#  collect()

s3_scores_P1D |>
  select(-project_id, -family, -sd, -duration, -pub_datetime) |>
  filter(reference_datetime > cutoff,
         reference_datetime < cutoff2) |>
  inner_join(sites, by = "site_id") |>
  mutate(reference_datetime = lubridate::as_datetime(reference_datetime),
         datetime = lubridate::as_datetime(datetime)) |>
  select(variable, model_id, site_id, datetime, reference_datetime, observation, mean, quantile02.5, quantile97.5) |>
  write_dataset("scores_P1D.parquet")

x <- Sys.time()
x30 <-  Sys.time() - days(35)

s3_scores_P1D |>
  mutate(datetime = lubridate::as_datetime(datetime),
         reference_datetime = lubridate::as_datetime(reference_datetime),
         horizon = datetime - reference_datetime,
         zero = x -x,
         days35 = x - x30)|>
  filter(horizon > zero,
         horizon < days35) |>
  group_by(variable, model_id) |>
  summarise(crps = mean(crps, na.rm = TRUE)) |>
  ungroup() |>
  write_dataset("scores_P1D_by_model_id.parquet")

s3_scores_P1D |>
  mutate(datetime = lubridate::as_datetime(datetime),
         reference_datetime = lubridate::as_datetime(reference_datetime),
         horizon = datetime - reference_datetime,
         zero = x -x,
         days35 = x - x30)|>
  filter(horizon > zero,
         horizon <= days35) |>
  group_by(variable, model_id, reference_datetime) |>
  summarise(crps = mean(crps, na.rm = TRUE)) |>
  ungroup() |>
  write_dataset("scores_P1D_by_reference.parquet")

s3_scores_P1D |>
  mutate(datetime = lubridate::as_datetime(datetime),
         reference_datetime = lubridate::as_datetime(reference_datetime),
         horizon = datetime - reference_datetime,
         zero = x -x,
         days35 = x - x30)|>
  filter(horizon > zero,
         horizon < days35) |>
  select(model_id, variable, crps, horizon) |>
  group_by(variable, model_id, horizon) |>
  summarise(crps = mean(crps, na.rm = TRUE)) |>
  ungroup() |>
  collect() |>
  mutate(horizon = as.numeric(horizon) / 86400) |>
  write_dataset("scores_P1D_by_horizon.parquet")

message("P1W scores")

cutoff <- Sys.Date() - lubridate::days(365)

s3_scores_P1W <- open_dataset(paste0("s3://", config$scores_bucket,"/bundled-parquet/project_id=",  config$project_id,"/duration=P1W/"), s3_endpoint = config$endpoint, anonymous = TRUE)

s3_scores_P1W |>
  select(-project_id, -family, -sd, -duration, -pub_datetime) |>
  filter(reference_datetime > cutoff) |>
  inner_join(sites, by = "site_id") |>
  mutate(reference_datetime = lubridate::as_datetime(reference_datetime),
         datetime = lubridate::as_datetime(datetime)) |>
  select(variable, model_id, site_id, datetime, reference_datetime, observation, mean, crps, quantile02.5, quantile97.5) |>
  write_dataset("scores_P1W.parquet")

message("high level stats")

s3_forecasts_all <- open_dataset(paste0("s3://", config$forecasts_bucket,"/bundled-parquet/project_id=",  config$project_id), s3_endpoint = config$endpoint, anonymous = TRUE)

s3_forecasts_all |>
  select(model_id, variable, duration, reference_datetime) |>
  distinct(model_id, variable, duration, reference_datetime) |>
  write_dataset("stats_all.parquet")



