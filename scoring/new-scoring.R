library(dplyr)
library(duckdbfs)
library(progress)
library(bench)
library(minioclient)
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

project <- "neon4cast"
cut_off_date <- Sys.Date() - lubridate::dmonths(6)
rescore <- FALSE
obs_key_cols <- c("project_id", "site_id", "datetime", "duration", "variable")

### Access the targets, forecasts, and scores subsets
targets <-
  open_dataset("s3://bio230014-bucket01/challenges/targets/",
               format="csv",  # set mode to TABLE to download first
               s3_endpoint = "sdsc.osn.xsede.org",
               anonymous=TRUE) |>
  filter(project_id == {project},
         datetime > {cut_off_date})

# No point in trying to score any forecasts still in future (relative to last observed)
# (pull forces eval, can take a minute)
last_observed_date <- targets |> select(datetime) |> distinct() |>
                      filter(datetime == max(datetime)) |> pull(datetime)

forecasts <-
  open_dataset("s3://bio230014-bucket01/challenges/forecasts/bundled-parquet/",
               s3_endpoint = "sdsc.osn.xsede.org",
               anonymous=TRUE) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         datetime <= {last_observed_date}
  )

## Need all bundles to append anyway, so get them all now.
bench::bench_time({
  mc_mirror("osn/bio230014-bucket01/challenges/scores/bundled-parquet",
            "scores/bundled-parquet", overwrite = TRUE)
})
scores <-
  # open_dataset("s3://bio230014-bucket01/challenges/scores/bundled-parquet/", s3_endpoint = "sdsc.osn.xsede.org", anonymous=TRUE)
  open_dataset("scores/bundled-parquet/") |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         !is.na(observation)
         )


tol <- 1e-2
if(rescore) {
  # drop rows from scores if the scores and targets disagree on "observation"
  scores <- scores |>
    inner_join(targets, by = obs_key_cols) |>
    filter( abs(observation.x - observation.y)/observation.x < {tol})

## Note: Only used to anti-join (filter).
## The new observations will come from latest targets

## union() won't overwrite those rows.

}


## NOTE In theory we just want to do this:
# bench::bench_time({
#  forecasts |>
#    anti_join(scores) |> # forecast is unscored
#    inner_join(targets) |> # forecast has targets available
#    write_dataset("score_me.parquet")
#})


## INSTEAD, we pull our subset to local disk first.
## This looks silly but is much better for RAM and speed!!
bench::bench_time({ # ~ 5.4m (w/ 6mo cutoff)
  forecasts |> write_dataset("forecasts.parquet")
  scores |> write_dataset("scores.parquet")
  targets |> write_dataset("targets.parquet")
})

bench::bench_time({
  forecasts <- open_dataset("forecasts.parquet")
  scores <- open_dataset("scores.parquet")
  targets <- open_dataset("targets.parquet")
})

## Magic rock&roll time: Subset unscored + targets available:
bench::bench_time({ # ~ 13s
  forecasts |>
    anti_join(scores) |> # forecast is unscored
    inner_join(targets) |> # forecast has targets available
    write_dataset("score_me.parquet")

})

## Now score it.  score4cast is all in RAM, so we must score in chunks.
## But we can chunk naturally with dplyr distinct
# remotes::install_github("eco4cast/score4cast", upgrade=FALSE)
library(score4cast)

duckdbfs::close_connection()

con <- duckdbfs::cached_connection(tempfile())
fs::dir_delete("new_scores")
fs::dir_create("new_scores")
#
fc <- open_dataset("score_me.parquet", conn=con) |> filter(!is.na(model_id))
groups <- fc |> distinct(project_id, duration, variable, model_id) |> collect()
total <- nrow(groups)


source("R/score_joined_table.R") #crps_logs_score slightly modified
#fs::dir_delete("new_scores/")


pb <- progress_bar$new(format = "  scoring [:bar] :percent in :elapsed",
                       total = total, clear = FALSE, width= 60)
# If we have lots to score this can take a while
for (i in seq_along(row_number(groups))) {
  pb$tick()
  # filtering join, could have used filter on duration/variable/model_id
  new_scores <- fc |>
    inner_join(groups[i,], copy=TRUE,
               by = join_by(project_id, duration, variable, model_id)
               ) |>
    collect() |>
    score_joined_table()

  ## Append to existing scores
  dur <- groups$duration[i]
  var <- groups$variable[i]
  model <- groups$model_id[i]
  path <- glue::glue("scores/bundled-parquet/project_id={project}",
                     "/duration={dur}/variable={var}/model_id={model}")

  if(fs::dir_exists(path)) {
    log <- glue::glue("Joining to existing scores of variable {var} for model {model}")
    #message(log)
    readr::write_lines(log, "new-scoring.log", append=TRUE)
    new_scores <- as_dataset(new_scores, conn = con)
    bundled_scores <- open_dataset(path, conn = con) |>
      anti_join(new_scores,
                by = join_by(reference_datetime, site_id, datetime, family, pub_datetime, observation,
                             crps, logs, mean, median, sd, quantile97.5, quantile02.5, quantile90, quantile10,
                             duration, model_id, project_id, variable)) |>
      compute()
    new_scores <- union_all(bundled_scores, new_scores)
  }

  new_scores |>
    group_by(project_id, duration, variable, model_id) |>
    write_dataset("new_scores/")

  # if we want to clear connection manually we need to re-open fc
  duckdbfs::close_connection(con)
  gc()
  con <- duckdbfs::cached_connection(tempfile())
  fc <- open_dataset("score_me.parquet", conn=con) |> filter(!is.na(model_id))

}

duckdbfs::close_connection(con)
gc()

## Validate no corruption



# checks that we have no corruption
open_dataset("new_scores/") |> count()
open_dataset(fs::path("new_scores/")) |>
  distinct(duration, variable, model_id) |> collect()
open_dataset(fs::path("new_scores/")) |>
  summarise(first_fc = min(reference_datetime), last_fc = max(reference_datetime),
            first_prediction = min(datetime), last_prediction = max(datetime))


## Copy over into existing bundled scores and sync
## Could be done without full mirror of scores(?)
fs::dir_copy("new_scores/", "scores/bundled-parquet/", overwrite =TRUE)


# checks that we have no corruption
open_dataset("scores/bundled-parquet/") |> count()
open_dataset(fs::path("scores/bundled-parquet/")) |>
  distinct(duration, variable, model_id) |> collect()
open_dataset(fs::path("scores/bundled-parquet/")) |>
  summarise(first_fc = min(reference_datetime), last_fc = max(reference_datetime),
            first_prediction = min(datetime), last_prediction = max(datetime))


mc_mirror("scores/bundled-parquet", "osn/bio230014-bucket01/challenges/scores/bundled-parquet", overwrite = TRUE)

## And tidy up
unlink("scores.parquet")
unlink("targets.parquet")
unlink("forecasts.parquet")
unlink("score_me.parquet")
fs::dir_delete("new_scores")
