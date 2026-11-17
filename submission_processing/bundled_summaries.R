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
               bucket = forecasts_bucket_base,
               url_compatibility_mode = FALSE)


remote_path <- paste0("osn/", summaries_bundled_bucket)
contents <- mc_ls(remote_path, recursive = TRUE, details = TRUE)
data_paths <- contents |> filter(!is_folder) |> pull(path)

# model paths are paths with at least one reference_datetime containing data files
model_paths <-
  data_paths |>
  str_replace_all("reference_date=\\d{4}-\\d{2}-\\d{2}/.*", "") |>
  str_replace("^osn\\/", "s3://") |>
  unique()


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
  duckdb_secrets(endpoint = config$endpoint,
                 key = Sys.getenv("OSN_KEY"),
                 secret = Sys.getenv("OSN_SECRET"),
                 bucket = forecasts_bucket_base,
                 url_compatibility_mode = FALSE)


  bundled_path <- path |>
    str_replace(fixed("forecasts/summaries"),
                "forecasts/bundled-summaries")

  open_dataset(path, conn = con) |> write_dataset("tmp_new.parquet")
  new <- open_dataset("tmp_new.parquet", conn = con)

  # Only if model has bundled entries!
  if (length(existing_bundle(bundled_path)) > 0) {
    # Stream the existing bundle straight from S3: we write locally and upload
    # afterwards, so there is no need for a local copy of it.
    old <- open_dataset(bundled_path, conn = con)

    # Skip submissions already in the bundle, so a rerun (e.g. a previous run
    # uploaded but failed before archiving the raw files, or two bundlers
    # overlapping) doesn't append them twice.
    already_bundled <- old |> distinct(reference_datetime, pub_datetime)
    new <- new |>
      anti_join(already_bundled, by = c("reference_datetime", "pub_datetime"),
                na_matches = "na")
    new <- union_all(old, new)
  }

  # duckdb >= 2.0 can't complete S3 multipart uploads to OSN, so write locally and upload with mc.
  new |>
    write_dataset(local_bundle,
                  options = list("PER_THREAD_OUTPUT false"))
  mc_cp(local_bundle,
        paste0(str_replace(bundled_path, fixed("s3://"), "osn/"), "data_0.parquet"))
  unlink(c(local_bundle, "tmp_new.parquet"))

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




# bundled count at end
count <- open_dataset(paste0("s3://", summaries_bundled_parquet_bucket),
                      s3_endpoint = config$endpoint,
                      anonymous = TRUE) |>
  count()

print(count)

if (length(failed) > 0) {
  stop(length(failed), " model(s) failed to bundle:\n", paste(failed, collapse = "\n"))
}
