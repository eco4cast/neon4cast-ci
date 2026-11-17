
# devtools::install_version("duckdb", "1.2.2")

library(dplyr)
library(duckdbfs)
library(progress)
library(bench)
library(minioclient)
library(yaml)
install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))
#fs::dir_create("new_scores")

config <- read_yaml("challenge_configuration.yaml")
scores_bundled_parquet_bucket <- paste0(config$scores_bucket, "/bundled-parquet/")

project <- config$project_id

#print("Downloading bundled scores...")
#bench::bench_time({
#  mc_mirror("osn/bio230014-bucket01/challenges/scores/bundled-parquet",
#            "scores/bundled-parquet", overwrite = TRUE, flags = "--retry")
#})

## Now score it.  score4cast is all in RAM, so we must score in chunks.
## But we can chunk naturally with dplyr distinct
library(score4cast)
con <- duckdbfs::cached_connection(tempfile())
duckdbfs::duckdb_secrets(endpoint = "s3-west.nrp-nautilus.io",
                         key = Sys.getenv("EFI_NRP_KEY"),
                         secret = Sys.getenv("EFI_NRP_SECRET"),
                         bucket = "efi-scores",
                         url_compatibility_mode = FALSE)

#
fc <- open_dataset("s3://efi-scores/tmp/score_me", conn=con) |>
  filter(!is.na(model_id))
groups <- fc |> distinct(project_id, duration, variable, model_id, family) |> collect()
total <- nrow(groups)

duckdbfs::close_connection(con)
gc()

#fs::dir_delete("new_scores/")

score_group <- function(i, groups, project = "neon4cast") {


  # if we want to clear connection manually we need to re-open fc.  Maybe not necessary
  source("scoring/R/score_joined_table.R")
  con <- duckdbfs::cached_connection(tempfile())
  duckdbfs::duckdb_secrets(endpoint = "s3-west.nrp-nautilus.io",
                           key = Sys.getenv("EFI_NRP_KEY"),
                           secret = Sys.getenv("EFI_NRP_SECRET"),
                           bucket = "efi-scores",
                           url_compatibility_mode = FALSE)
  fc <- duckdbfs::open_dataset("s3://efi-scores/tmp/score_me/**",
                     conn=con) |>
    dplyr::filter(!is.na(model_id))


  # filtering join, could have used filter on duration/variable/model_id
  new_scores <- fc |>
    dplyr::inner_join(groups[i,], copy=TRUE,
                      by = dplyr::join_by(project_id, duration, variable, model_id, family)
    ) |>
    dplyr::collect() |>
    score_joined_table()

  ## Append to existing scores
  dur <- groups$duration[i]
  var <- groups$variable[i]
  model <- groups$model_id[i]
  path <- glue::glue("s3://",
                     scores_bundled_parquet_bucket,
                     "project_id={project}/duration={dur}/",
                     "variable={var}/model_id={model}")

  path2 <- glue::glue("osn/", scores_bundled_parquet_bucket,
                      "project_id={project}/duration={dur}/",
                      "variable={var}/model_id={model}")


  log <- glue::glue("Joining to existing scores of variable {var} for model {model}")
  message(log)
  #readr::write_lines(log, "new-scoring.log", append=TRUE)

  file_exist <- length(mc_ls(path2))

  duckdbfs::duckdb_secrets(endpoint = "sdsc.osn.xsede.org",
                           key = Sys.getenv("OSN_KEY"),
                           secret = Sys.getenv("OSN_SECRET"),
                           bucket = scores_bundled_parquet_bucket,
                           url_compatibility_mode = FALSE)

  if(file_exist > 0){

  new_scores <- duckdbfs::as_dataset(new_scores, conn = con)
  bundled_scores <- duckdbfs::open_dataset(path, conn = con) |>
    dplyr::anti_join(new_scores,
              by = dplyr::join_by(reference_datetime, site_id, datetime,
                                  family, pub_datetime, observation,
                                  crps, logs, mean, median, sd,
                                  quantile97.5, quantile02.5, quantile90, quantile10,
                                  duration, model_id, project_id, variable)) |>
    dplyr::compute()
  new_scores <- dplyr::union_all(bundled_scores, new_scores)
  }

  new_scores |>
    dplyr::distinct() |>
    dplyr::group_by(project_id, duration, variable, model_id) |>
    # duckdbfs::write_dataset("s3://efi-scores/tmp/bundled-parquet/")
    duckdbfs::write_dataset(paste0("s3://", scores_bundled_parquet_bucket))


  duckdbfs::close_connection(con)
  gc()
}


print("Computing new scores....")
pb <- progress_bar$new(format = "  scoring [:bar] :percent in :elapsed",
                       total = total, clear = FALSE, width= 60)


# If we have lots to score this can take a while
for (i in seq_along(row_number(groups))) {
  pb$tick()
  print(paste("Scoring model:", groups$model_id[i], "variable:", groups$variable[i]))

  score_group(i, groups)

  #go <- purrr::safely( \() callr::r_safe(score_group, args = list(i = i, groups = groups)) )
  #go()
}

# check RAM use for: Scoring model: persistenceRW variable: nee

# checks that we have no corruption
checks <- function() {
  duckdbfs::duckdb_secrets(endpoint = "s3-west.nrp-nautilus.io",
                           key = Sys.getenv("EFI_NRP_KEY"),
                           secret = Sys.getenv("EFI_NRP_SECRET"),
                           bucket = "efi-scores",
                           url_compatibility_mode = FALSE)
  open_dataset("s3://efi-scores/tmp/bundled-parquet/") |> count()
  open_dataset("s3://efi-scores/tmp/bundled-parquet/") |>
    distinct(duration, variable, model_id) |> collect()
  open_dataset("s3://efi-scores/tmp/bundled-parquet/") |>
    summarise(first_fc = min(reference_datetime), last_fc = max(reference_datetime),
              first_prediction = min(datetime), last_prediction = max(datetime))

}

#checks()
