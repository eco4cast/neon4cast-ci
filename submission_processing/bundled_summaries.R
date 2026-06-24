library(tidyverse)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)
library(future.apply)
library(progressr)
library(yaml)
handlers(global = TRUE)
handlers("cli")


config <- read_yaml("challenge_configuration.yaml")

summaries_bundled_bucket <- paste0(config$forecasts_bucket, "/summaries/project_id=", config$project_id, "/")
summaries_bundled_parquet_bucket <- paste0(config$forecasts_bucket, "/bundled-summaries/")
forecasts_bucket_base <- str_split(config$forecasts_bucket, "/", simplify = TRUE)[1]


# bundled count at start
b <- open_dataset(paste0("s3://", summaries_bundled_parquet_bucket),
                      s3_endpoint = config$endpoint,
                      anonymous = TRUE)

b |> count() |> print()
b |> summarise(date = max(reference_datetime)) |> print()


install_mc()
mc_alias_set("osn", config$endpoint, Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

duckdb_secrets(endpoint = config$endpoint,
               key = Sys.getenv("OSN_KEY"),
               secret = Sys.getenv("OSN_SECRET"),
               bucket = forecasts_bucket_base)


remote_path <- paste0("osn/", summaries_bundled_bucket)
contents <- mc_ls(remote_path, recursive = TRUE, details = TRUE)
data_paths <- contents |> filter(!is_folder) |> pull(path)

# model paths are paths with at least one reference_datetime containing data files
model_paths <-
  data_paths |>
  str_replace_all("reference_date=\\d{4}-\\d{2}-\\d{2}/.*", "") |>
  str_replace("^osn\\/", "s3://") |>
  unique()


bundle_me <- function(path) {

  print(path)
  con = duckdbfs::cached_connection(tempfile())
  duckdb_secrets(endpoint = config$endpoint,
                 key = Sys.getenv("OSN_KEY"),
                 secret = Sys.getenv("OSN_SECRET"),
                 bucket = forecasts_bucket_base)


  bundled_path <- path |>
    str_replace(fixed("forecasts/summaries"),
                "forecasts/bundled-summaries")

  open_dataset(path, conn = con) |> write_dataset("tmp_new.parquet")


 # Only if model has bundled entries!
  old <- tryCatch({
  open_dataset(bundled_path, conn = con) |>
     write_dataset("tmp_old.parquet")
  old <- open_dataset("tmp_old.parquet")
  },
  # no new data
  error = function(e) NULL
  )

  # these are both local, so we can stream back.
  new <- open_dataset("tmp_new.parquet")

  if(!is.null(old)) {
    new <- union_all(old,new)
  }

  new |>
    write_dataset(paste0(bundled_path,"data_0.parquet"),
                  options = list("PER_THREAD_OUTPUT false"))

  #We should now archive anything we have bundled:

  mc_path <- path |> str_replace(fixed("s3://"), "osn/")
  dest_path <- mc_path |>
    str_replace(fixed("forecasts/summaries"), "forecasts/archive-summaries")
  mc_mv(mc_path, dest_path, recursive = TRUE)
  # clears up empty folders
  mc_rm(mc_path, recursive = TRUE)

  duckdbfs::close_connection(con); gc()
  invisible(path)
}



# We use future_apply framework to show progress while being robust to OOM kils.
# We are not actually running on multi-core, which would be RAM-inefficient
future::plan(future::sequential)

safe_bundles <- function(xs) {
  p <- progressor(along = xs)
  future_lapply(xs, function(x, ...) {
    bundle_me(x)
    p(sprintf("x=%s", x))
  },  future.seed = TRUE)
}


bench::bench_time({
  safe_bundles(model_paths)
})




# bundled count at end
count <- open_dataset(paste0("s3://", summaries_bundled_parquet_bucket),
                      s3_endpoint = config$endpoint,
                      anonymous = TRUE) |>
  count()

print(count)
