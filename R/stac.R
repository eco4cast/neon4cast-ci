# Small builders for STAC (SpatioTemporal Asset Catalog) JSON.
#
# These replace the stac4cast package, whose functions read `config`,
# `catalog_config` and the loop variable `m` from the global environment, call
# dplyr and readr functions without attaching them, and re-download the site
# metadata for every bounding box. Here everything is passed in.

stac_extension_urls <- c(
  table = "https://stac-extensions.github.io/table/v1.2.0/schema.json",
  scientific = "https://stac-extensions.github.io/scientific/v1.0.0/schema.json",
  item_assets = "https://stac-extensions.github.io/item-assets/v1.0.0/schema.json"
)

#' Build parts of a STAC catalog
#'
#' * `stac_link()`: one link object. Returns `NULL` when `href` is `NULL`, so
#'   optional links can be dropped with [purrr::compact()].
#' * `stac_standard_links()`: the parent, root, self, cite-as, about and
#'   describedby links every collection in the catalog has.
#' * `stac_collection()`: a STAC Collection.
#' * `stac_datetime()`: dates as STAC datetimes (`YYYY-MM-DDT00:00:00Z`).
#' * `stac_bbox()`: the bounding box of some sites.
#' * `stac_columns()`: the `table:columns` entry.
#' * `write_stac()`: writes JSON, creating the folder.
#'
#' @param rel,href,type,title Link fields.
#' @param cfg Configuration from [read_config()].
#' @param parent,root Relative paths to the parent and root documents.
#' @param id,description Collection id and description.
#' @param links List of links.
#' @param bbox Numeric bounding box, from `stac_bbox()`.
#' @param start,end First and last dates covered.
#' @param columns `table:columns` entry, from `stac_columns()`.
#' @param assets Named list of assets.
#' @param extensions Names of STAC extensions used: `"table"`,
#'   `"scientific"`, `"item_assets"`.
#' @param ... Extra top-level fields placed before `links`, such as
#'   `"sci:doi"`.
#' @param x Dates or date-times.
#' @param sites Site metadata from [read_sites()].
#' @param site_ids Sites to include. `NULL` means all sites.
#' @param schema A tibble with columns `name` and `type` (a list column).
#' @param descriptions Named character vector of column descriptions.
#' @param path File to write.
#' @return A list ready for [jsonlite::write_json()]; `stac_datetime()` a
#'   character vector; `stac_bbox()` a numeric vector (min lon, min lat,
#'   max lon, max lat); `write_stac()` `path`, invisibly.
#' @export
stac_link <- function(rel, href, type = "application/json", title = NULL) {
  if (is.null(href)) return(NULL)
  purrr::compact(list(rel = rel, type = type, href = href, title = title))
}

#' @rdname stac_link
#' @export
stac_standard_links <- function(cfg, parent, root) {
  cc <- cfg$catalog_config
  purrr::compact(list(
    stac_link("parent", parent),
    stac_link("root", root),
    stac_link("self", "collection.json"),
    stac_link("cite-as", cc$citation_doi_link, type = NULL),
    stac_link("about", cc$about_string, "text/html", cc$about_title),
    stac_link("describedby", cc$documentation_url, "text/html", cc$catalog_title)
  ))
}

#' @rdname stac_link
#' @export
stac_collection <- function(id, title, description, links, bbox, start, end,
                            columns = NULL, assets = list(), extensions = "table", ...) {
  purrr::compact(list(
    id = id,
    description = description,
    stac_version = "1.0.0",
    license = "CC0-1.0",
    stac_extensions = as.list(unname(stac_extension_urls[extensions])),
    type = "Collection",
    ...,
    links = links,
    title = title,
    extent = list(
      spatial = list(bbox = list(bbox)),
      temporal = list(interval = list(as.list(stac_datetime(c(start, end)))))
    ),
    `table:columns` = columns,
    assets = assets
  ))
}

#' @rdname stac_link
#' @export
stac_datetime <- function(x) {
  paste0(format(as.Date(x)), "T00:00:00Z")
}

#' @rdname stac_link
#' @export
stac_bbox <- function(sites, site_ids = NULL) {
  if (!is.null(site_ids)) sites <- sites[sites$site_id %in% site_ids, ]
  if (nrow(sites) == 0) return(NULL)
  c(min(sites$longitude), min(sites$latitude), max(sites$longitude), max(sites$latitude))
}

#' @rdname stac_link
#' @export
stac_columns <- function(schema, descriptions = character()) {
  purrr::map2(schema$name, schema$type, function(name, type) {
    description <- if (name %in% names(descriptions)) descriptions[[name]]
    purrr::compact(list(name = name, type = type, description = description))
  })
}

#' @rdname stac_link
#' @export
write_stac <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}

#' Column descriptions shipped with the package
#'
#' @param kind One of `"forecasts"`, `"summaries"`, `"scores"`, `"noaa"` or
#'   `"targets"`.
#' @return A named character vector: column name = description.
#' @export
column_descriptions <- function(kind) {
  file <- system.file("extdata", "catalog_columns.csv", package = "challengeci", mustWork = TRUE)
  d <- readr::read_csv(file, show_col_types = FALSE, progress = FALSE)
  d <- d[d$kind == kind, ]
  if (nrow(d) == 0) cli::cli_abort("No column descriptions for {.val {kind}}.")
  rlang::set_names(d$description, d$column)
}

# The schema of an arrow dataset, with types as arrow prints them.
arrow_schema <- function(schema) {
  tibble::tibble(
    name = schema$names,
    type = purrr::map(schema$fields, function(f) f$type$ToString())
  )
}

# The schema of a data frame, with types as R classes.
r_schema <- function(df) {
  tibble::tibble(name = names(df), type = unname(lapply(df, class)))
}

# Public s3:// URI of a path in one of the challenge's buckets, in the form
# arrow and duckdbfs accept for anonymous reads.
public_s3_uri <- function(cfg, key, ...) {
  store <- bucket_key_store(key)
  endpoint <- store_credentials(cfg, store, anonymous = TRUE)$endpoint
  path <- sub("^s3://", "", bucket_uri(cfg, key, ..., scheme = "s3"))
  paste0("s3://anonymous@", path, "?endpoint_override=", endpoint)
}

# Asset pointing at parquet data, with R code to read it. `filter` is an
# optional line of dplyr code applied before collect().
parquet_asset <- function(uri, what, title = "Database Access", filter = NULL) {
  pipe <- if (is.null(filter)) "" else paste0(" |>\n  ", filter)
  list(
    href = uri,
    type = "application/x-parquet",
    title = title,
    roles = list("data"),
    description = paste0(
      "Use `arrow` for remote access to the database. This R code will return ", what,
      ".\n\n### R\n\n```{r}\n# Use code below\n\nall_results <- arrow::open_dataset(\"",
      uri, "\")\ndf <- all_results", pipe, " |>\n  dplyr::collect()\n```\n\n",
      "You can use dplyr operations before calling `dplyr::collect()` to `summarise`, ",
      "`select` columns, and/or `filter` rows prior to pulling the data into a local ",
      "`data.frame`. Reducing the data that is pulled locally will speed up the data ",
      "download speed and reduce your memory usage."
    )
  )
}

# Asset pointing at a CSV file, with R code to read it.
csv_asset <- function(url, title) {
  list(
    href = url,
    type = "text/csv",
    title = title,
    roles = list("data"),
    description = paste0(
      "This R code will return the data in this file.\n\n### R\n\n```{r}\n",
      "# Use code below\n\nurl <- \"", url, "\"\n",
      "df <- readr::read_csv(url, show_col_types = FALSE)\n```"
    )
  )
}

thumbnail_asset <- function(href, title) {
  if (is.null(href)) return(NULL)
  list(href = href, type = "image/JPEG", roles = list("thumbnail"), title = title)
}
