library(dplyr)
library(duckdbfs)

project <- "neon4cast"
cut_off_date <- Sys.Date() - lubridate::dmonths(6)
rescore = FALSE
obs_key_cols = c("project_id", "site_id", "datetime", "duration", "variable")

### Access the targets, forecasts, and scores subsets
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

## Now score it:

fc <- open_dataset("score_me.parquet") |> filter(!is.na(model_id))

# We need to chunk into pieces small enough that we can safely collect()
groups <- fc |> distinct(project_id, duration, variable, model_id) |> collect()
total <- nrow(groups)
source("R/score_joined_table.R") #crps_logs_score slightly modified

fs::dir_delete("new_scores/")

library(progress)
pb <- progress_bar$new(format = "  scoring [:bar] :percent in :elapsed",
                       total = total, clear = FALSE, width= 60)
for (i in 1:total) {
  pb$tick()
  fc |>
    inner_join(groups[i,], copy=TRUE,
               by = join_by(project_id, duration, variable, model_id)
               ) |>
    collect() |>
    score_joined_table() |>
    group_by(project_id, duration, variable, model_id) |>
    write_dataset("new_scores/")
}



## We could just pull full scores, join, and push...

## And tidy up
unlink("scores.parquet")
unlink("targets.parquet")
unlink("forecasts.parquet")
unlink("score_me.parquet")

