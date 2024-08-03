library(dplyr)
library(duckdbfs)

project <- "neon4cast"
cut_off_date <- Sys.Date() - lubridate::dmonths(6)
rescore = TRUE
obs_key_cols = c("project_id", "site_id", "datetime", "duration", "variable")

targets <-
  open_dataset("s3://bio230014-bucket01/challenges/targets/",
               format="csv",  # set mode to TABLE to download first
               s3_endpoint = "sdsc.osn.xsede.org",
               anonymous=TRUE) |>
  filter(project_id == {project},
         datetime > {cut_off_date})

# No point in trying to score any forecasts still in future (relative to last observed)
last_observed_date <-
  targets |> select(datetime) |> distinct() |>
  filter(datetime == max(datetime)) |> pull(datetime)

forecasts <-
  open_dataset("s3://bio230014-bucket01/challenges/forecasts/bundled-parquet/",
               s3_endpoint = "sdsc.osn.xsede.org",
               anonymous=TRUE) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         datetime <= {last_observed_date}
  )

scores <- duckdbfs::open_dataset("s3://bio230014-bucket01/challenges/scores/bundled-parquet/",
                                 s3_endpoint = "sdsc.osn.xsede.org",
                                     anonymous=TRUE) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         !is.na(observation)
         )

tol <- 1e-2
if(rescore) {
  # drop those rows where scores and targets disagree
  scores <- scores |>
    inner_join(targets, by = obs_key_cols) |>
    filter( abs(observation.x - observation.y)/observation.x < {tol})
}

# one-shot -- not great with RAM or speed, even with disk-backed conn
# bench::bench_time({ #5.92m (1mo)
#  forecasts |>
#    anti_join(scores) |> # forecast is unscored
#    inner_join(targets) |> # forecast has targets available
#    write_dataset("score_me.parquet")
#})


## Piecewise download, much better for RAM and speed
bench::bench_time({ # 5.4m (6mo)
  forecasts |> write_dataset("forecasts.parquet")
  scores |> write_dataset("scores.parquet")
  targets |> write_dataset("targets.parquet")

})

bench::bench_time({ #13s (6mo)
  forecasts <- open_dataset("forecasts.parquet")
  scores <- open_dataset("scores.parquet")
  targets <- open_dataset("targets.parquet")

  forecasts |>
    anti_join(scores) |> # forecast is unscored
    inner_join(targets) |> # forecast has targets available
    write_dataset("score_me.parquet")
})









## if(!rescore) # detect if stuff is needing a rescore
#scores |>
#  inner_join(targets, by = obs_key_cols) |>
#  filter( abs(observation.x - observation.y)/observation.x >= {tol}) |>
#  collect()

