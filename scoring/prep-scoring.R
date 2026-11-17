#devtools::install_version("duckdb", "1.2.2")
remotes::install_github('cboettig/duckdbfs', upgrade = 'never')

library(dplyr)
library(duckdbfs)
library(progress)
library(bench)
library(yaml)
library(stringr)
library(minioclient)

library(DBI)
con <- duckdbfs::cached_connection(tempfile())
DBI::dbExecute(con, "SET THREADS=64;")
install_mc()
mc_alias_set("osn", "s3-west.nrp-nautilus.io", Sys.getenv("EFI_NRP_KEY"), Sys.getenv("EFI_NRP_SECRET"))

remove_dir <- function(path) {
  tryCatch(
    {
      minioclient::mc_rm(path, recursive = TRUE)
      message('directory successfully removed...')
    },
    error = function(cond) {
      message("The removal directory could not be found...")
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
      NA
    },
    warning = function(cond) {
      message('Deleting the directory caused a warning...')
      message("Here's the original warning message:")
      message(conditionMessage(cond))
      # Choose a return value in case of warning
      NULL
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally = <expression>'
      message("Finished the delete portion...")
    }
  )
}

remove_dir("osn/efi-scores/tmp/score_me")
remove_dir("osn/efi-scores/tmp/forecasts")
remove_dir("osn/efi-scores/tmp/targets")
remove_dir("osn/efi-scores/tmp/scores")


config <- read_yaml("challenge_configuration.yaml")

forecast_bundled_parquet_bucket <- paste0(config$forecasts_bucket, "/bundled-parquet/")
scores_bundled_parquet_bucket <- paste0(config$scores_bucket, "/bundled-parquet/")
scores_bucket_base <- str_split(config$scores_bucket, "/", simplify = TRUE)[1]

#library(minioclient)

#install_mc()
#mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))
#fs::dir_create("new_scores")

project <- config$project_id
cut_off_date <- Sys.Date() - lubridate::dmonths(6)
rescore <- FALSE
obs_key_cols <- c("project_id", "site_id", "datetime", "duration", "variable")
score_key_cols <- c(obs_key_cols, "model_id", "family", "reference_datetime")


duckdbfs::duckdb_secrets(endpoint = config$endpoint,
                         key = "",
                         secret = "",
                         bucket = scores_bucket_base,
                         url_compatibility_mode = FALSE)


# Create vector of targets files
num_target_groups <- length(config$target_groups)
target_files <- NULL
for(i in 1:num_target_groups){
  target_files <- c(target_files, config$target_groups[[i]]$targets_file)
}


### Access the targets, forecasts, and scores subsets
targets <-
  open_dataset(target_files,
               recursive = FALSE,
               format = "csv",
               parser_options = list(nullstr = "NA"),
               anonymous = TRUE,
               ) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         !is.na(observation)
         )


# No point in trying to score any forecasts still in future (relative to last observed)
# (pull forces eval, can take a minute)
last_observed_date <- targets |> select(datetime) |> distinct() |>
  filter(datetime == max(datetime)) |> pull(datetime)

# Omit scoring of daily forecasts that have a horizon > 35
forecasts <-
  open_dataset(paste0("s3://", forecast_bundled_parquet_bucket),
               s3_endpoint = config$endpoint,
               anonymous=TRUE) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         datetime <= {last_observed_date},
         !is.na(model_id),
         !is.na(parameter),
         !is.na(prediction)

  ) |>
  # if necessary, enforce naming convention on "family" to avoid perpetual rescoring
  mutate(family = ifelse(family == 'ensemble', "sample", family)) |>
  # enforce horizon filter
  mutate(horizon = date_diff('day', as.POSIXct(reference_datetime), as.POSIXct(datetime))) |>
  filter(! (duration == "P1D" & horizon > 35))


scores <-
  open_dataset(paste0("s3://", scores_bundled_parquet_bucket),
               s3_endpoint = config$endpoint, anonymous=TRUE) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         !is.na(observation)
  )


tol <- 1e-2
if(rescore) {
  print("rescoring changed observations")
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


print("Caching forecasts, scores, targets...")

duckdbfs::duckdb_secrets(endpoint = "s3-west.nrp-nautilus.io",
                         key = Sys.getenv("EFI_NRP_KEY"),
                         secret = Sys.getenv("EFI_NRP_SECRET"),
                         bucket = "efi-scores",
                         url_compatibility_mode = FALSE)


## INSTEAD, we pull our subset to local disk first.
## This looks silly but is much better for RAM and speed!!
bench::bench_time({ # ~ 5.4m (w/ 6mo cutoff)
  forecasts |> group_by(variable) |> write_dataset("s3://efi-scores/tmp/forecasts")
})

bench::bench_time({
  scores |> group_by(variable) |> write_dataset("s3://efi-scores/tmp/scores")
})

bench::bench_time({
    targets |> group_by(variable) |> write_dataset("s3://efi-scores/tmp/targets")
})

bench::bench_time({
  forecasts <- open_dataset("s3://efi-scores/tmp/forecasts/**")
  scores <- open_dataset("s3://efi-scores/tmp/scores/**")
  targets <- open_dataset("s3://efi-scores/tmp/targets/**")
})

## Magic rock&roll time: Subset unscored + targets available:
print("Compute who needs to be scored...")
bench::bench_time({ # ~ 13s
  forecasts |>
    anti_join(select(scores, all_of(score_key_cols))) |> # forecast is unscored
    inner_join(targets) |> # forecast has targets available
    group_by(variable) |>
    write_dataset("s3://efi-scores/tmp/score_me")

})
