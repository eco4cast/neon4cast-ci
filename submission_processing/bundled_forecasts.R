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

forecast_parquet_bucket <- paste0(config$forecasts_bucket, "/parquet/project_id=", config$project_id, "/")
forecast_bundled_parquet_bucket <- paste0(config$forecasts_bucket, "/bundled-parquet/")
forecasts_bucket_base <- str_split(config$forecasts_bucket, "/", simplify = TRUE)[1]


install_mc()
mc_alias_set("osn", config$endpoint, Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))
# mc_alias_set("nrp", "s3-west.nrp-nautilus.io", Sys.getenv("EFI_NRP_KEY"), Sys.getenv("EFI_NRP_SECRET"))

duckdb_secrets(endpoint = config$endpoint , key = Sys.getenv("OSN_KEY"), secret = Sys.getenv("OSN_SECRET"), bucket = forecasts_bucket_base)



remote_path <- paste0("osn/", forecast_parquet_bucket)
contents <- mc_ls(remote_path, recursive = TRUE, details = TRUE)
data_paths <- contents |> filter(!is_folder) |> pull(path)

# model paths are paths with at least one reference_datetime containing data files
model_paths <-
  data_paths |>
  str_replace_all("reference_date=\\d{4}-\\d{2}-\\d{2}/.*", "") |>
  str_replace("^osn\\/", "s3://") |>
  unique()

print(model_paths)

# bundled count at start
count <- open_dataset(paste0("s3://", forecast_bundled_parquet_bucket),
                      s3_endpoint = config$endpoint,
                      anonymous = TRUE) |>
  count()
print(count)


bundle_me <- function(path) {

  print(path)
  con = duckdbfs::cached_connection(tempfile())
  duckdb_secrets(endpoint = config$endpoint, key = Sys.getenv("OSN_KEY"), secret = Sys.getenv("OSN_SECRET"), bucket = forecasts_bucket_base)
  bundled_path <- path |> str_replace(fixed("forecasts/parquet"), "forecasts/bundled-parquet")

  open_dataset(path, conn = con) |>
    filter( !is.na(model_id),
            !is.na(parameter),
            !is.na(prediction)) |>
    write_dataset("tmp_new.parquet")

  # special filters should not be needed on bundled copy
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

## We can just "append", we no longer face duplicates:
# by <- join_by(datetime, site_id, prediction, parameter, family, reference_datetime, pub_datetime, duration, model_id, project_id, variable)
#  filtered_n <- old |> anti_join(new, by = by) |> count() |> pull(n) # is this the bottleneck?
#  previous_n <- open_dataset("tmp_old.parquet") |> count() |> pull(n)
#  stopifnot(previous_n - filtered_n == 0)

  ## no partition levels left so we must write to an explicit .parquet
  if(!is.null(old)) {
    bundled_dir <- bundled_path |> str_replace(fixed("s3://"), "osn/") |> mc_ls(details = TRUE)
    mc_bundled_path <- bundled_dir |> filter(!is_folder) |> pull(path)
    stopifnot(length(mc_bundled_path) == 1)
    bundled_path <- mc_bundled_path |> str_replace(fixed("osn/"), fixed("s3://"))

    new <- union(old, new)
  } else {
    bundled_path <- paste0(bundled_path,'data_0.parquet') ## write explicit parquet for new submissions as well
  }

  ## once running consistently we can "append" with union_all instead of union
  # uses less RAM. since mc_rm / mc_mv removes anything we have already read
  new |>
    write_dataset(bundled_path,
                  options = list("PER_THREAD_OUTPUT false"))

  #We should now archive anything we have bundled:
  mc_path <- path |> str_replace(fixed("s3://"), "osn/")
  dest_path <- mc_path |>
    str_replace(fixed("forecasts/parquet"), "forecasts/archive-parquet")
  mc_mv(mc_path, dest_path, recursive = TRUE)

  # clears up empty folders (not necessary?)
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
    out <- bundle_me(x)
    p(sprintf("x=%s", x))
    out
  },  future.seed = TRUE)
}


bench::bench_time({
  out <- safe_bundles(model_paths)
})
# print(out)



# bundled count at end
count <- open_dataset(paste0("s3://", forecast_bundled_parquet_bucket),
                      s3_endpoint = config$endpoint,
                      anonymous = TRUE) |>
  count()
print(count)



most_recent <- open_dataset(paste0("s3://", forecast_bundled_parquet_bucket),
             s3_endpoint = config$endpoint,
             anonymous = TRUE) |>
  group_by(model_id, variable) |>
  summarise(most_recent = max(reference_datetime)) |>
  arrange(desc(most_recent))
print(most_recent)



# should we slice_max(pub_time) to ensure only most recent pub_time if duplicates submitted?
# grouping <- c("model_id", "reference_datetime", "site_id", "datetime", "family", "variable", "duration", "project_id")
