# Terrestrial (NEE, LE) daily targets for the NEON Ecological Forecasting Challenge.
#
# Data flow
#   1. NEON portal data (monthly releases) are downloaded with neonstore and
#      written to an archive on OSN (flux_staging/neon_portal), one parquet file
#      per site/year/month. Each run re-pulls the last few months so revised
#      provisional data replace what is in the archive.
#   2. NEON unpublished data (daily files, ~5-day latency) are kept in
#      flux_staging/current_month, only for days after each site's last portal day.
#   3. Targets are rebuilt from the archive + unpublished data, merged per site
#      with the existing targets file so no data are dropped, checked, and uploaded.
#
# Environment variables
#   NEONSTORE_HOME  neonstore directory (default /home/rstudio/data/neonstore_temp)
#   DRY_RUN=true    run everything but do not write to or delete from OSN

if (file.exists(".Renviron")) readRenviron(".Renviron")

print(paste0("Running Creating Terrestrial Targets at ", Sys.time()))
print(sessionInfo())

Sys.setenv(NEONSTORE_HOME = Sys.getenv("NEONSTORE_HOME", "/home/rstudio/data/neonstore_temp"))
Sys.setenv(AWS_EC2_METADATA_DISABLED = "TRUE")

dry_run <- tolower(Sys.getenv("DRY_RUN")) == "true"
use_5day_data <- TRUE
months_to_refresh <- 5

library(neonstore)
library(tidyverse)
library(lubridate)
library(arrow)
library(minioclient)

print(sessioninfo::package_info())
if (dry_run) message("DRY RUN: nothing will be written to or deleted from OSN")

options(timeout = max(300, getOption("timeout")))

install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

osn_root <- "osn/bio230014-bucket01"

# minioclient splits commands on spaces, so quote local paths (macOS puts the
# neonstore db under "Application Support")
mc_mirror(file.path(osn_root, "neonstore/db"), shQuote(neonstore::neon_db_dir()))
mc_mirror(file.path(osn_root, "flux_staging/neonstore_temp"), shQuote(neonstore::neon_dir()))

sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/main/neon4cast_field_site_metadata.csv", show_col_types = FALSE) |>
  dplyr::filter(terrestrial == 1)

site_names <- sites$field_site_id

# arrow is used to read and delete on OSN. All uploads go through mc, because
# arrow's S3 uploads fail on OSN with XAmzContentSHA256Mismatch.
osn_bucket <- function(path) {
  arrow::s3_bucket(file.path("bio230014-bucket01", path),
                   endpoint_override = "sdsc.osn.xsede.org",
                   access_key = Sys.getenv("OSN_KEY"),
                   secret_key = Sys.getenv("OSN_SECRET"))
}

s3 <- osn_bucket("challenges/targets/project_id=neon4cast/duration=P1D")
s3_neon_portal <- osn_bucket("flux_staging/neon_portal")
s3_current_month <- osn_bucket("flux_staging/current_month")

flux_cols <- c("timeBgn", "timeEnd", "data.fluxCo2.turb.flux", "siteID",
               "data.fluxH2o.turb.flux", "qfqm.fluxCo2.turb.qfFinl")

existing_targets_daily <- arrow::read_csv_arrow(s3$path("terrestrial_daily-targets.csv.gz"))

# Whole months only: archive partitions are replaced wholesale, so a partial
# month here would drop data from the archive.
start_month <- floor_date(as_date(max(existing_targets_daily$datetime)) %m-% months(months_to_refresh), "month")
message("Refreshing NEON portal data from ", start_month)

# ---------------------------------------------------------------------------
# 1. NEON portal data -> archive
# ---------------------------------------------------------------------------

neonstore::neon_download(product = "DP4.00200.001", type = "basic", start_date = start_month)
neonstore::neon_store(product = "DP4.00200.001")

# When NEON reprocesses a month, neonstore keeps both versions of the file. File
# names differ only by the processing timestamp, so the last file sorts newest.
portal_new <- neon_table(table = "nsae-basic", site = site_names, lazy = TRUE) |>
  mutate(timeBgn = as_datetime(timeBgn),
         timeEnd = as_datetime(timeEnd)) |>
  filter(timeBgn >= !!as_datetime(start_month)) |>
  select(all_of(flux_cols), file) |>
  collect() |>
  arrange(siteID, timeBgn, desc(file)) |>
  distinct(siteID, timeBgn, .keep_all = TRUE) |>
  select(-file) |>
  mutate(year = year(timeBgn),
         month = month(timeBgn)) |>
  arrange(siteID, year, month, timeBgn)  # sorted input gives one file per partition

message(nrow(portal_new), " portal rows since ", start_month, " for ",
        n_distinct(portal_new$siteID), " sites")

touched <- distinct(portal_new, siteID, year, month)

if (nrow(portal_new) > 0) {
  archive_local <- file.path(tempdir(), "neon_portal")
  unlink(archive_local, recursive = TRUE)
  arrow::write_dataset(portal_new, archive_local, partitioning = c("siteID", "year", "month"))

  new_files <- list.files(archive_local, recursive = TRUE)
  stopifnot("expected one parquet file per partition" = !anyDuplicated(dirname(new_files)))

  # Files in the partitions being replaced that this run did not write
  remote_files <- s3_neon_portal$ls(recursive = TRUE)
  remote_files <- remote_files[grepl("\\.parquet$", remote_files)]
  stale <- remote_files[dirname(remote_files) %in% dirname(new_files) & !remote_files %in% new_files]

  if (!dry_run) {
    mc_mirror(shQuote(archive_local), file.path(osn_root, "flux_staging/neon_portal"), overwrite = TRUE)
    for (f in stale) s3_neon_portal$DeleteFile(f)
  }
  message("Archive: wrote ", length(new_files), " partitions, removed ", length(stale), " stale files",
          if (dry_run) " (dry run: skipped)")
}

# The archive is the source of truth for portal data. Partitions refreshed this
# run come from portal_new, so the result is the same with or without DRY_RUN.
portal_data <- arrow::open_dataset(s3_neon_portal) |>
  filter(siteID %in% site_names) |>
  select(all_of(flux_cols), year, month) |>
  collect() |>
  anti_join(touched, by = c("siteID", "year", "month")) |>
  bind_rows(portal_new) |>
  select(all_of(flux_cols))

n_dup <- sum(duplicated(portal_data[c("siteID", "timeBgn")]))
if (n_dup > 0) {
  warning(n_dup, " duplicated half-hours in the neon_portal archive; keeping one of each. ",
          "Run targets/R/terrestrial_cleanup_neon_portal_archive.R to fix the archive.")
  portal_data <- distinct(portal_data, siteID, timeBgn, .keep_all = TRUE)
}

portal_end <- portal_data |>
  group_by(siteID) |>
  summarise(portal_end = as_date(max(timeBgn)), .groups = "drop")

# ---------------------------------------------------------------------------
# 2. NEON unpublished data (5-day latency) -> current_month
# ---------------------------------------------------------------------------

# File names look like NEON.D16.ABBY.DP4.00200.001.nsae.2026-10-01.expanded.20261009T040053Z.h5[.parquet]
nsae_site <- function(f) stringr::str_split_fixed(basename(f), "\\.", 12)[, 3]
nsae_date <- function(f) as_date(stringr::str_split_fixed(basename(f), "\\.", 12)[, 8])

current_data <- NULL

if (use_5day_data) {
  cm_prefix <- file.path(osn_root, "flux_staging/current_month")

  have <- s3_current_month$ls()
  have <- have[grepl("\\.parquet$", have)]

  files <- tryCatch(
    readr::read_csv("https://storage.googleapis.com/neon-sae-files/ods/sae_files_unpublished/sae_file_url_unpublished.csv",
                    show_col_types = FALSE),
    error = function(e) {
      warning("Could not read NEON's unpublished file list; using existing current_month files only: ",
              conditionMessage(e))
      NULL
    })

  if (is.null(files)) {
    # Without the list we can't tell what is stale: use what we have, delete nothing
    keep <- have
    to_get <- tibble(url = character(), file_name = character(), parquet = character())
    stale <- character()
  } else {
    # Convert old S3 links to GCS
    files$url <- base::gsub("https://s3.data.neonscience.org", "https://storage.googleapis.com", files$url)
    files <- files |>
      filter(site %in% site_names) |>
      mutate(file_name = basename(url),
             parquet = paste0(file_name, ".parquet")) |>
      left_join(portal_end, by = c("site" = "siteID")) |>
      filter(is.na(portal_end) | date > portal_end)   # per site, only days the portal doesn't have

    keep <- files$parquet
    to_get <- filter(files, !parquet %in% have)
  }

  message(length(intersect(have, keep)), " unpublished files already converted, ",
          nrow(to_get), " to download")

  tmp_dir <- file.path(tempdir(), "current_month")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  new_dfs <- list()
  failed <- character()

  for (i in seq_len(nrow(to_get))) {
    h5 <- file.path(tmp_dir, to_get$file_name[i])
    pq <- file.path(tmp_dir, to_get$parquet[i])
    ok <- tryCatch({
      download.file(to_get$url[i], destfile = h5, mode = "wb", quiet = TRUE)
      df <- neonstore::neon_read(files = h5) |>
        select(all_of(flux_cols))
      arrow::write_parquet(df, pq)
      if (!dry_run) mc_cp(shQuote(pq), file.path(cm_prefix, to_get$parquet[i]))
      new_dfs[[to_get$parquet[i]]] <- df
      TRUE
    }, error = function(e) {
      warning("Skipping ", to_get$file_name[i], " (will retry next run): ", conditionMessage(e))
      FALSE
    })
    unlink(c(h5, pq))
    if (!ok) failed <- c(failed, to_get$parquet[i])
  }

  if (!is.null(files)) {
    # Files no longer wanted: now covered by the portal, or replaced by a newer
    # version. Keep an old version if its replacement failed to download.
    failed_keys <- tibble(site = nsae_site(failed), date = nsae_date(failed))
    not_wanted <- tibble(parquet = setdiff(have, keep)) |>
      mutate(site = nsae_site(parquet), date = nsae_date(parquet))
    retained <- semi_join(not_wanted, failed_keys, by = c("site", "date"))$parquet
    stale <- setdiff(not_wanted$parquet, retained)
    keep <- c(keep, retained)
  }

  read_from_osn <- intersect(have, keep)
  remote_df <- NULL
  if (length(read_from_osn) > 0) {
    remote_df <- arrow::open_dataset(s3_current_month) |>
      mutate(file = add_filename()) |>
      collect() |>
      filter(basename(file) %in% read_from_osn) |>
      select(all_of(flux_cols))
  }

  current_data <- bind_rows(remote_df, bind_rows(new_dfs))

  if (!dry_run) for (f in stale) s3_current_month$DeleteFile(f)
  message("current_month: ", length(new_dfs), " added, ", length(failed), " failed, ",
          length(stale), " removed", if (dry_run) " (dry run: OSN not changed)")
}

# Combine published and unpublished; portal rows come first so they win any overlap
flux_data <- bind_rows(portal_data, current_data) |>
  distinct(siteID, timeBgn, .keep_all = TRUE)

flux_data |>
  group_by(siteID) |>
  summarize(min = min(timeBgn),
            max = max(timeBgn), .groups = "drop") |>
  arrange(max) |>
  print(n = 60)

# ---------------------------------------------------------------------------
# 3. Daily targets
# ---------------------------------------------------------------------------

#2025-07-15: Per Dave Durden - there was overflagging of QC on SRER in the latest relesae.
#He recommend using the Ameriflux release because it has the correct QC flags.
#This files contains the GC flags for SRER from AMF_US-xSR_FLUXNET_SUBSET_HH_2019-2024_4-7.csv
srer_qc_update <- read_csv("targets/supporting-data/AF_SRER_QC_CODES.csv", show_col_types = FALSE) %>%
  rename(time = datetime,
         siteID = site_id)

flux_data <- flux_data %>%
  mutate(time = as_datetime(timeBgn)) |>
  left_join(srer_qc_update, by = c("time", "siteID")) %>%
  mutate(qfqm.fluxCo2.turb.qfFinl = ifelse(!is.na(nee_AF_qc_flag), nee_AF_qc_flag, qfqm.fluxCo2.turb.qfFinl))


co2_data <- flux_data %>%
  filter(qfqm.fluxCo2.turb.qfFinl == 0 & data.fluxCo2.turb.flux > -50 & data.fluxCo2.turb.flux < 50) %>%
  select(time,data.fluxCo2.turb.flux, siteID, data.fluxH2o.turb.flux) %>%
  rename(nee = data.fluxCo2.turb.flux,
         le = data.fluxH2o.turb.flux,
         site_id = siteID) |>
  mutate(nee = ifelse(site_id == "OSBS" & year(time) < 2019, NA, nee),
         nee = ifelse(site_id == "SRER" & year(time) < 2019, NA, nee),
         nee = ifelse(site_id == "BARR" & year(time) < 2019, NA, nee),
         le = ifelse(site_id == "OSBS" & year(time) < 2019, NA, le),
         le = ifelse(site_id == "SRER" & year(time) < 2019, NA, le),
         le = ifelse(site_id == "BARR" & year(time) < 2019, NA, le)) |>
  pivot_longer(-c("time","site_id"), names_to = "variable", values_to = "observation")

flux_target_30m <- co2_data

valid_dates <- flux_target_30m %>%
  mutate(date = as_date(time)) %>%
  filter(!is.na(observation)) %>%
  group_by(date, site_id, variable) %>%
  summarise(count = n(), .groups = "drop")

flux_target_daily <- flux_target_30m %>%
  mutate(date = as_date(time)) %>%
  group_by(date, site_id, variable) %>%
  summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
  left_join(valid_dates, by = c("date","site_id", "variable")) |>
  mutate(observation = ifelse(count > 24, observation, NA),
         observation = ifelse(is.nan(observation), NA, observation)) %>%
  rename(time = date) %>%
  select(-count) |>
  mutate(observation = ifelse(variable == "nee", (observation * 12 / 1000000) * (60 * 60 * 24), observation))

flux_target_daily <- flux_target_daily |>
  select(time, site_id, variable, observation) |>
  rename(datetime = time) |>
  mutate(datetime = lubridate::as_datetime(datetime),
         duration = "P1D",
         project_id = "neon4cast") |>
  select(project_id, site_id, datetime, duration, variable, observation) |>
  arrange(variable, datetime) |>
  na.omit()

# Replace existing targets only within each site's recomputed date range. Sites
# with no data this run, and days outside the range, keep their existing values.
recomputed <- flux_data |>
  group_by(site_id = siteID) |>
  summarise(start = as_date(min(timeBgn)),
            end = as_date(max(timeBgn)), .groups = "drop")

old_fluxes <- existing_targets_daily |>
  mutate(date = as_date(datetime)) |>
  left_join(recomputed, by = "site_id") |>
  filter(is.na(start) | date < start | date > end) |>
  select(-date, -start, -end)

combined_daily <- bind_rows(flux_target_daily, old_fluxes) |>
  arrange(project_id, site_id, datetime, duration, variable) |>
  distinct()

# ---------------------------------------------------------------------------
# 4. Check and upload
# ---------------------------------------------------------------------------

n_old <- nrow(existing_targets_daily)
n_new <- nrow(combined_daily)
missing_sites <- setdiff(unique(existing_targets_daily$site_id), unique(combined_daily$site_id))
n_dup_keys <- sum(duplicated(combined_daily[c("site_id", "datetime", "variable")]))

message("Targets: ", n_old, " rows before, ", n_new, " rows now (",
        sprintf("%+.1f%%", 100 * (n_new - n_old) / n_old), "), latest ", max(combined_daily$datetime))

if (n_new < 0.95 * n_old) stop("New targets have ", n_new, " rows vs ", n_old, " before; not uploading")
if (length(missing_sites) > 0) stop("Sites missing from new targets: ", paste(missing_sites, collapse = ", "))
if (n_dup_keys > 0) stop(n_dup_keys, " duplicated site/datetime/variable rows in new targets")

targets_file <- if (dry_run) file.path(tempdir(), "terrestrial_daily-targets.csv.gz") else "terrestrial_daily-targets.csv.gz"
write_csv(combined_daily, targets_file)
message("Wrote ", targets_file)

if (!dry_run) {
  mc_cp(shQuote(targets_file), "osn/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/")

  if (fs::dir_exists(fs::path(neon_dir(), "DP4.00200.001"))) {
    fs::dir_delete(fs::path(neon_dir(), "DP4.00200.001"))
  }
  mc_mirror(shQuote(neonstore::neon_db_dir()), file.path(osn_root, "neonstore/db"), overwrite = TRUE, remove = TRUE)
  mc_mirror(shQuote(neonstore::neon_dir()), file.path(osn_root, "flux_staging/neonstore_temp"), overwrite = TRUE, remove = TRUE)
}

message(paste0("Completed Terrestrial Target at ", Sys.time()))
