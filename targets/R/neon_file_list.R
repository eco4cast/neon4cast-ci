# Helpers for building a list of NEON portal files with *freshly signed* URLs.
#
# The NEON API returns Google Cloud Storage URLs that are signed and expire
# after 7 days (X-Goog-Expires=604800). The URLs must therefore never be reused
# from a cache across runs. The cache file is only used to remember which files
# are known to exist, so that files the API silently drops on a given call
# (rate limiting / transient HTTP errors return NULL in neon_data()) trigger a
# targeted retry rather than being lost.

# Strip the publication timestamp so re-published versions of the same file
# (e.g. ...2016-03.basic.20240905T232237Z.csv) share one key.
neon_file_key <- function(name) {
  sub("\\.[0-9]{8}T[0-9]{6}Z\\.", ".", name)
}

# NEON file names are NEON.<domain>.<site>.<level>.<product>.<revision>.
# <horizontal>.<vertical>.<interval>.<description>.<YYYY-MM>.<package>...,
# e.g. NEON.D03.BARC.DP1.20264.001.103.802.030.TSD_30_min.2025-01.basic....csv.
# neon_file_field() returns field `i` of each name (or URL: the query string
# and folders are dropped first).
neon_file_field <- function(name, i) {
  name <- basename(sub("\\?.*$", "", name))
  vapply(strsplit(name, ".", fixed = TRUE), function(x) x[i], character(1))
}
neon_file_site <- function(name) neon_file_field(name, 3)
neon_file_horizontal <- function(name) neon_file_field(name, 7)
neon_file_vertical <- function(name) neon_file_field(name, 8)

# Keep one row per logical file: the most recently published version
latest_per_key <- function(df) {
  df |>
    dplyr::mutate(key = neon_file_key(name)) |>
    dplyr::arrange(dplyr::desc(name)) |>
    dplyr::distinct(key, .keep_all = TRUE)
}

# Read NEON portal CSVs in site-month batches and apply `process` to each batch.
#
# Reading every URL in one vroom::vroom() call loads all files (and all ~100
# columns) into memory at once, which kills the GitHub runner (exit code 137)
# for the 1-minute water quality product. Every aggregation in
# aquatics_targets.R groups within a site and within a day/hour, so processing
# one site-month at a time gives identical results with bounded memory.
#
# Columns are read as character (startDateTime as datetime) so a column that
# happens to be empty at the start of a file is never guessed as logical and
# silently set to NA. Columns missing from a file are added as NA.
read_neon_csv_batches <- function(urls, cols, process, retries = 3) {
  fname <- basename(sub("\\?.*$", "", urls))
  batch <- paste(neon_file_site(fname),
                 regmatches(fname, regexpr("[0-9]{4}-[0-9]{2}", fname)))
  stopifnot(length(batch) == length(urls))
  groups <- split(urls, batch)
  col_types <- vroom::cols(.default = vroom::col_character(),
                           startDateTime = vroom::col_datetime())

  read_batch <- function(u) {
    for (attempt in seq_len(retries)) {
      out <- tryCatch(
        vroom::vroom(u, id = "filename", delim = ",",
                     col_select = tidyselect::any_of(c("filename", cols)),
                     col_types = col_types, progress = FALSE,
                     show_col_types = FALSE),
        error = function(e) e)
      if (!inherits(out, "error")) break
      message("  read failed (attempt ", attempt, "): ", conditionMessage(out))
      if (attempt == retries) stop(out)
      Sys.sleep(10 * attempt)
    }
    for (m in setdiff(cols, names(out))) out[[m]] <- NA_character_
    out
  }

  message("Reading ", length(urls), " files in ", length(groups), " site-month batches")
  res <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    if (i %% 100 == 0) message("  batch ", i, " / ", length(groups))
    res[[i]] <- process(read_batch(groups[[i]]))
  }
  dplyr::bind_rows(res)
}

neon_file_list <- function(product, cache_file, site = NA, max_iter = 4) {

  fresh <- neonstore:::neon_data(product = product, site = site, type = "basic") |>
    latest_per_key()

  known <- character(0)
  if (file.exists(cache_file)) {
    known <- readr::read_csv(cache_file, show_col_types = FALSE,
                             col_select = "name")$name |>
      neon_file_key() |>
      unique()
  }

  missing <- setdiff(known, fresh$key)
  iter <- 1
  while (length(missing) > 0 && iter < max_iter) {
    iter <- iter + 1
    retry_sites <- unique(neon_file_site(missing))
    message(product, ": ", length(missing), " known files missing from API response; ",
            "retrying sites ", paste(retry_sites, collapse = ", "))
    retry <- neonstore:::neon_data(product = product, site = retry_sites, type = "basic")
    fresh <- dplyr::bind_rows(fresh, retry) |> latest_per_key()
    missing <- setdiff(known, fresh$key)
  }

  if (length(missing) > 0) {
    warning(product, ": ", length(missing), " previously known files were not returned ",
            "by the NEON API after ", max_iter, " attempts and will be skipped this run")
  }

  # Record every known file name (fresh + still missing) so missing files are
  # retried next run. URLs are kept only for reference; they are never reused.
  fs::dir_create(dirname(cache_file))
  dplyr::bind_rows(fresh, tibble::tibble(name = missing)) |>
    dplyr::select(-key) |>
    readr::write_csv(cache_file)

  message(product, ": ", nrow(fresh), " files with freshly signed URLs")
  dplyr::select(fresh, -key)
}
