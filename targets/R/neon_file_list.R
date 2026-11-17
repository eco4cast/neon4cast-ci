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

# NEON file names look like NEON.D10.ARIK.DP1.20288.001....; the site is field 3
neon_file_site <- function(name) {
  vapply(strsplit(name, ".", fixed = TRUE), function(x) x[3], character(1))
}

# Keep one row per logical file: the most recently published version
latest_per_key <- function(df) {
  df |>
    dplyr::mutate(key = neon_file_key(name)) |>
    dplyr::arrange(dplyr::desc(name)) |>
    dplyr::distinct(key, .keep_all = TRUE)
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
