# Read one NEON observational (OS) table across all sites without DuckDB.
#
# neonUtilities::datasetQuery() reads the portal CSVs through duckdbfs/httpfs.
# In the eco4cast/rocker-neon4cast container httpfs fails on the
# storage.googleapis.com URLs (same SSL problem the aquatics job hit), and
# datasetQuery() hides the error inside try(silent = TRUE) and returns NULL,
# so the failure only surfaces later as
#   "no applicable method for 'select' applied to an object of class NULL".
#
# This uses neonUtilities::queryFiles() to get the file URLs, downloads them
# in parallel with curl (which uses the system CA bundle), and stacks them in
# R. Every column is read as character, so a column that is empty in one file
# can't be guessed as logical, and files with extra or missing columns stack
# cleanly. Column types are guessed once, on the combined table.
neon_os_table <- function(dpID, tabl, package = "basic", token = Sys.getenv("NEON_TOKEN"),
                          release = "current", include.provisional = TRUE,
                          dest = file.path(tempdir(), dpID, package, tabl), retries = 3) {

  urlset <- neonUtilities::queryFiles(dpID = dpID, tabl = tabl, package = package,
                                      release = release, metadata = FALSE,
                                      include.provisional = include.provisional,
                                      token = token)
  # neonUtilities >= 4.1.0 returns a data frame (url, urlvar, release) by
  # default (simplify.outputs = TRUE); earlier versions returned a list with
  # $files. Accept either.
  urls <- if (is.data.frame(urlset)) urlset$url else urlset$files
  if (length(urls) == 0) {
    stop("neonUtilities::queryFiles() returned no files for ", dpID, " ", tabl)
  }
  files <- file.path(dest, basename(sub("\\?.*$", "", urls)))
  stopifnot(!anyDuplicated(files))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  message("Downloading ", length(urls), " files for ", tabl)
  for (attempt in seq_len(retries)) {
    todo <- !file.exists(files)
    if (!any(todo)) break
    res <- curl::multi_download(urls[todo], files[todo], progress = FALSE)
    bad <- res$destfile[!(res$success %in% TRUE & res$status_code %in% 200)]
    unlink(bad)
    if (length(bad) > 0) {
      message("  ", length(bad), " downloads failed (attempt ", attempt, ")")
      Sys.sleep(10 * attempt)
    }
  }
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop(length(missing), " of ", length(files), " files for ", tabl,
         " could not be downloaded, e.g. ", basename(missing[1]))
  }

  out <- lapply(files, function(f) {
    vroom::vroom(f, delim = ",", col_types = vroom::cols(.default = vroom::col_character()),
                 progress = FALSE, show_col_types = FALSE)
  }) |>
    dplyr::bind_rows()

  suppressMessages(readr::type_convert(out, guess_integer = FALSE))
}
