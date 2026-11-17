# One-off repair for the flux archive on OSN (flux_staging/neon_portal).
#
# The archive has duplicated half-hours from two causes:
#   * earlier versions of terrestrial_targets.R wrote unsorted data with
#     write_dataset(), which split partitions into part-0/part-1 files once more
#     than 900 were open; later runs never removed the extra files
#   * when NEON reprocesses a month, neonstore keeps both versions of the file,
#     so neon_table() returned every half-hour twice and both went into the archive
#
# The second cause can't be fixed from the parquet files alone, because they
# don't record which file a row came from. So this script rebuilds each archive
# partition from the neonstore database on OSN, keeping the newest file's row
# for each half-hour. Partitions the database doesn't have are kept, with
# duplicates removed (newest parquet file wins). No partition is allowed to
# lose half-hours.
#
# Default is a dry run that only reports. Set APPLY=true to change OSN:
#   APPLY=true Rscript targets/R/terrestrial_cleanup_neon_portal_archive.R

if (file.exists(".Renviron")) readRenviron(".Renviron")
Sys.setenv(AWS_EC2_METADATA_DISABLED = "TRUE")

library(arrow)
library(dplyr)
library(lubridate)
library(minioclient)
library(neonstore)

apply_changes <- tolower(Sys.getenv("APPLY")) == "true"
message(if (apply_changes) "APPLY: OSN will be changed" else "DRY RUN: set APPLY=true to change OSN")

flux_cols <- c("timeBgn", "timeEnd", "data.fluxCo2.turb.flux", "siteID",
               "data.fluxH2o.turb.flux", "qfqm.fluxCo2.turb.qfFinl")

archive <- arrow::s3_bucket("bio230014-bucket01/flux_staging/neon_portal",
                            endpoint_override = "sdsc.osn.xsede.org",
                            access_key = Sys.getenv("OSN_KEY"),
                            secret_key = Sys.getenv("OSN_SECRET"))

install_mc()
mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

# Fresh copy of the OSN neonstore database in its own folder, so a local R
# session holding the default database open doesn't get in the way
db_dir <- file.path(tempdir(), "neonstore_db")
Sys.setenv(NEONSTORE_DB = db_dir)
mc_mirror("osn/bio230014-bucket01/neonstore/db", shQuote(db_dir))

# --- Current archive --------------------------------------------------------

info <- archive$GetFileInfo(FileSelector$create("", recursive = TRUE))
files <- tibble(path = vapply(info, \(x) x$path, ""),
                mtime = do.call(c, lapply(info, \(x) x$mtime))) |>
  filter(grepl("\\.parquet$", path)) |>
  mutate(partition = dirname(path))

old <- arrow::open_dataset(archive) |>
  mutate(path = add_filename()) |>
  collect() |>
  mutate(path = sub("^bio230014-bucket01/flux_staging/neon_portal/", "", path),
         partition = dirname(path)) |>
  left_join(select(files, path, mtime), by = "path")

message(nrow(files), " files, ", n_distinct(files$partition), " partitions, ", nrow(old), " rows in archive")

# --- Rebuild from neonstore -------------------------------------------------

from_db <- neon_table(table = "nsae-basic", lazy = TRUE) |>
  mutate(timeBgn = as_datetime(timeBgn),
         timeEnd = as_datetime(timeEnd)) |>
  select(all_of(flux_cols), file) |>
  collect() |>
  arrange(siteID, timeBgn, desc(file)) |>
  distinct(siteID, timeBgn, .keep_all = TRUE) |>
  select(-file) |>
  mutate(partition = paste0("siteID=", siteID, "/year=", year(timeBgn), "/month=", month(timeBgn)))

message(nrow(from_db), " distinct half-hours in neonstore")

# Partitions neonstore doesn't have: keep the archive's rows, newest file wins
archive_only <- old |>
  filter(!partition %in% from_db$partition) |>
  mutate(siteID = sub("^siteID=([^/]+)/.*", "\\1", partition)) |>
  arrange(siteID, timeBgn, desc(mtime)) |>
  distinct(siteID, timeBgn, .keep_all = TRUE) |>
  select(all_of(flux_cols), partition)

rebuilt <- bind_rows(from_db, archive_only)

# --- Compare ----------------------------------------------------------------

before <- old |>
  group_by(partition) |>
  summarise(files = n_distinct(path), rows_before = n(), halfhours_before = n_distinct(timeBgn), .groups = "drop")
after <- rebuilt |>
  group_by(partition) |>
  summarise(rows_after = n(), .groups = "drop")

cmp <- full_join(before, after, by = "partition") |>
  mutate(across(c(files, rows_before, halfhours_before, rows_after), \(x) coalesce(x, 0L)),
         source = ifelse(partition %in% from_db$partition, "neonstore", "archive"))

shrunk <- filter(cmp, rows_after < halfhours_before)
message("Rows before: ", sum(cmp$rows_before), ", after: ", sum(cmp$rows_after))
message("Partitions: ", sum(cmp$halfhours_before > 0), " before, ", sum(cmp$rows_after > 0), " after; ",
        sum(cmp$halfhours_before == 0), " new from neonstore, ",
        sum(cmp$source == "archive"), " kept from archive only")

if (nrow(shrunk) > 0) {
  print(shrunk, n = 50)
  stop(nrow(shrunk), " partitions would lose half-hours; not continuing")
}

# How many half-hours change value (old archive copy vs rebuilt), for the record
# Half-hours where an archive copy differs from the rebuilt (newest file) value
same <- function(a, b) (a == b & !is.na(a) & !is.na(b)) | (is.na(a) & is.na(b))
changed_values <- old |>
  mutate(siteID = sub("^siteID=([^/]+)/.*", "\\1", partition)) |>
  select(siteID, timeBgn, partition, all_of(setdiff(flux_cols, c("siteID", "timeBgn")))) |>
  inner_join(select(rebuilt, all_of(flux_cols)), by = c("siteID", "timeBgn"), suffix = c(".old", ".new")) |>
  filter(!(same(timeEnd.old, timeEnd.new) &
             same(data.fluxCo2.turb.flux.old, data.fluxCo2.turb.flux.new) &
             same(data.fluxH2o.turb.flux.old, data.fluxH2o.turb.flux.new) &
             same(as.numeric(qfqm.fluxCo2.turb.qfFinl.old), as.numeric(qfqm.fluxCo2.turb.qfFinl.new)))) |>
  distinct(siteID, timeBgn, partition)

to_write <- cmp |>
  filter(files > 1 | rows_before != rows_after | partition %in% changed_values$partition) |>
  pull(partition)

message(nrow(changed_values), " half-hours in ", n_distinct(changed_values$partition),
        " partitions have an archive copy that differs from the newest-file value")
message(length(to_write), " partitions will be rewritten")

if (!apply_changes) {
  message("Dry run complete")
  quit(save = "no")
}

# --- Write ------------------------------------------------------------------

local_root <- file.path(tempdir(), "neon_portal_rebuild")
unlink(local_root, recursive = TRUE)
rebuilt |>
  filter(partition %in% to_write) |>
  mutate(year = year(timeBgn), month = month(timeBgn)) |>
  select(-partition) |>
  arrange(siteID, year, month, timeBgn) |>
  arrow::write_dataset(local_root, partitioning = c("siteID", "year", "month"))

written <- list.files(local_root, recursive = TRUE)
stopifnot(!anyDuplicated(dirname(written)), setequal(dirname(written), to_write))

# Upload part-0 files first, then remove every other file in those partitions
mc_mirror(shQuote(local_root), "osn/bio230014-bucket01/flux_staging/neon_portal", overwrite = TRUE)
extra <- files$path[files$partition %in% to_write & !files$path %in% written]
for (f in extra) archive$DeleteFile(f)
message("Rewrote ", length(to_write), " partitions, removed ", length(extra), " extra files")

check <- arrow::open_dataset(archive) |> count(siteID, timeBgn) |> filter(n > 1) |> collect()
if (nrow(check) > 0) stop(nrow(check), " duplicated half-hours remain")
message("Archive has no duplicated half-hours")
