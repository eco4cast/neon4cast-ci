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

duckdb_secrets(endpoint = config$endpoint , key = Sys.getenv("OSN_KEY"), secret = Sys.getenv("OSN_SECRET"), bucket = forecasts_bucket_base, url_compatibility_mode = FALSE)



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


# mc path of a model's existing bundle file, or character(0) if it has none yet.
# mc_ls returns an empty listing for a missing prefix and errors on access or
# network failures. Those errors must propagate: reading them as "no bundle
# yet" would overwrite the bundle with only the new data and wipe its history.
existing_bundle <- function(bundled_path) {
  listing <- bundled_path |> str_replace(fixed("s3://"), "osn/") |> mc_ls(details = TRUE)
  if (nrow(listing) == 0) return(character(0))
  files <- listing |> filter(!is_folder) |> pull(path)
  if (length(files) > 1) {
    stop("Expected one bundle file, found ", length(files), ": ", toString(files))
  }
  files
}

bundle_me <- function(path) {

  print(path)
  local_bundle <- tempfile(fileext = ".parquet")
  on.exit(unlink(c(local_bundle, "tmp_new.parquet")), add = TRUE)
  con = duckdbfs::cached_connection(tempfile())
  duckdb_secrets(endpoint = config$endpoint, key = Sys.getenv("OSN_KEY"), secret = Sys.getenv("OSN_SECRET"), bucket = forecasts_bucket_base, url_compatibility_mode = FALSE)
  bundled_path <- path |> str_replace(fixed("forecasts/parquet"), "forecasts/bundled-parquet")

  open_dataset(path, conn = con) |>
    filter( !is.na(model_id),
            !is.na(parameter),
            !is.na(prediction)) |>
    write_dataset("tmp_new.parquet")
  new <- open_dataset("tmp_new.parquet", conn = con)

  # Only if model has bundled entries!
  mc_bundled_path <- existing_bundle(bundled_path)

  ## no partition levels left so we must write to an explicit .parquet
  if(length(mc_bundled_path) > 0) {
    # Stream the existing bundle straight from S3: we write locally and upload
    # afterwards, so there is no need for a local copy of it.
    # special filters should not be needed on bundled copy
    old <- open_dataset(bundled_path, conn = con)

    # Append with union_all. union() ran SELECT DISTINCT over every row, which
    # on large bundles (e.g. gcc_90/persistenceRW, ~264M rows) exhausted the
    # runner's disk/RAM. Instead, only skip submissions already in the bundle
    # (e.g. a previous run uploaded but failed before archiving the raw files).
    already_bundled <- old |> distinct(reference_datetime, pub_datetime)
    new <- new |>
      anti_join(already_bundled, by = c("reference_datetime", "pub_datetime"),
                na_matches = "na")
    new <- union_all(old, new)

    bundled_path <- mc_bundled_path |> str_replace(fixed("osn/"), fixed("s3://"))
  } else {
    bundled_path <- paste0(bundled_path,'data_0.parquet') ## write explicit parquet for new submissions as well
  }

  # duckdb >= 2.0 can't complete S3 multipart uploads to OSN, so write locally and upload with mc.
  new |>
    write_dataset(local_bundle,
                  options = list("PER_THREAD_OUTPUT false"))
  mc_cp(local_bundle, str_replace(bundled_path, fixed("s3://"), "osn/"))
  unlink(c(local_bundle, "tmp_new.parquet"))

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

# A failure is fatal for that model only: its raw files stay in place (they are
# archived only after a successful upload) and are retried next run. The job
# still fails at the end so the problem is visible.
safe_bundles <- function(xs) {
  p <- progressor(along = xs)
  future_lapply(xs, function(x, ...) {
    out <- tryCatch(bundle_me(x), error = function(e) {
      message("FAILED to bundle ", x, ": ", conditionMessage(e))
      e
    })
    p(sprintf("x=%s", x))
    out
  },  future.seed = TRUE)
}


bench::bench_time({
  out <- safe_bundles(model_paths)
})

failed <- model_paths[vapply(out, inherits, logical(1), "error")]
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

if (length(failed) > 0) {
  stop(length(failed), " model(s) failed to bundle:\n", paste(failed, collapse = "\n"))
}


# should we slice_max(pub_time) to ensure only most recent pub_time if duplicates submitted?
# grouping <- c("model_id", "reference_datetime", "site_id", "datetime", "family", "variable", "duration", "project_id")
