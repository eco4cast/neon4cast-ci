# The challenge's STAC catalog: a tree of JSON files describing the forecasts,
# summaries, scores, NOAA drivers, targets and sites, committed to the catalog
# repository (`catalog_config$catalog_repo`) by the catalog workflow.
#
#   catalog.json
#   forecasts|summaries|scores/collection.json            everything
#     <group>/collection.json                             one target group
#       <Duration>_<Variable>/collection.json             one variable
#         models/<model_id>.json                          one model (STAC item)
#   noaa_forecasts/collection.json, <product>/collection.json
#   targets/collection.json, sites/collection.json
#
# The dates and sites in the forecasts, summaries and scores collections come
# from catalog_inventory(): one grouped duckdb scan of the bundled summaries
# and one of the bundled scores, shared by all three collections.

catalog_parts <- c("metadata", "catalog", "forecasts", "summaries", "scores",
                   "noaa", "sites", "targets")

#' Build the challenge's STAC catalog
#'
#' Builds the requested parts and writes them under `out_dir`. Each part runs
#' even if an earlier one fails; failures are reported together at the end.
#'
#' Parts:
#' * `metadata`: model metadata JSON, uploaded to `model_metadata_bucket`
#'   (see [write_model_metadata()]).
#' * `catalog`: the root `catalog.json`.
#' * `forecasts`, `summaries`, `scores`: a collection per kind, target group
#'   and variable, and an item per registered model.
#' * `noaa`, `sites`, `targets`: one collection each. `noaa` is left out of
#'   the default parts when the config has no `noaa_forecasts`.
#'
#' @inheritParams config_get
#' @param parts Parts to build. Defaults to all of them.
#' @param out_dir Root of the catalog. Defaults to [catalog_dir()].
#' @return The parts built, invisibly.
#' @export
run_catalog <- function(cfg, parts = default_catalog_parts(cfg), out_dir = catalog_dir(cfg)) {
  parts <- rlang::arg_match(parts, catalog_parts, multiple = TRUE)
  log_step("Building catalog parts {.val {parts}} in {.path {out_dir}}")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  model_kinds <- intersect(parts, c("forecasts", "summaries", "scores"))

  # Inputs shared by several parts are read once, and only if a part needs
  # them. The two inventories are the slow step (a few minutes each).
  sites <- read_sites(cfg)
  registered <- variables <- inventory <- targets <- NULL
  if (length(model_kinds) > 0 || "metadata" %in% parts) registered <- read_registered_models(cfg)
  if (length(model_kinds) > 0) {
    variables <- catalog_variables(cfg)
    duckdb_connect(cfg, "main", anonymous = TRUE)
    inventory <- list(scores = catalog_inventory(cfg, "scores_bucket", "bundled-parquet"))
    if (any(c("forecasts", "summaries") %in% model_kinds)) {
      inventory$summaries <- catalog_inventory(cfg, "summaries_bucket", "bundled-summaries")
    }
  }
  if (any(c("sites", "targets") %in% parts)) targets <- targets_summary(cfg)

  build <- function(part) {
    switch(part,
      metadata = write_model_metadata(cfg, registered),
      catalog = build_root_catalog(cfg, out_dir),
      noaa = build_noaa_collection(cfg, sites, out_dir),
      sites = build_sites_collection(cfg, targets, out_dir),
      targets = build_targets_collection(cfg, targets, out_dir),
      build_model_collection(part, cfg, inventory, sites, registered, variables, out_dir)
    )
  }
  results <- purrr::map(rlang::set_names(parts), function(part) {
    log_step("Catalog: {part}")
    purrr::safely(build)(part)
  })
  failed <- purrr::keep(results, function(r) !is.null(r$error))
  if (length(failed) > 0) {
    cli::cli_abort(c(
      "{length(failed)} catalog part{?s} failed:",
      rlang::set_names(purrr::imap_chr(failed, function(r, part) {
        paste0(part, ": ", conditionMessage(r$error))
      }), "x")
    ))
  }
  invisible(parts)
}

# Every part, except NOAA drivers for a challenge without them.
default_catalog_parts <- function(cfg) {
  if (is.null(cfg$noaa_forecasts)) setdiff(catalog_parts, "noaa") else catalog_parts
}

#' Where the catalog is written
#'
#' The catalog workflow clones the catalog repository next to this one, so the
#' default is `../<catalog_config$catalog_path>`. In a dry run it is a
#' `catalog` folder under [dry_run_dir()].
#'
#' @inheritParams config_get
#' @return A folder path.
#' @export
catalog_dir <- function(cfg) {
  if (is_dry_run()) return(file.path(dry_run_dir(), "catalog"))
  file.path("..", config_get(cfg, "catalog_config", "catalog_path"))
}

# Folder of one part inside the catalog. The config's *_path settings start
# with the catalog folder name (e.g. neon4cast-catalog/forecasts).
catalog_subdir <- function(cfg, key, default) {
  path <- cfg$catalog_config[[key]] %||% default
  path <- sub("/+$", "", path)
  prefix <- paste0(cfg$catalog_config$catalog_path, "/")
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1) else path
}

#' Readable names for durations
#'
#' Used to name variables in the catalog, e.g. `Daily_Chlorophyll_a`.
#' Override or add labels in `catalog_config$duration_labels`.
#'
#' @param duration ISO 8601 durations.
#' @inheritParams config_get
#' @return Character vector of labels; durations without a label are
#'   returned unchanged.
#' @export
#' @examples
#' duration_label(c("P1D", "PT30M", "P1M"))
duration_label <- function(duration, cfg = list()) {
  labels <- c(P1D = "Daily", PT1H = "Hourly", PT30M = "30min", P1W = "Weekly")
  overrides <- unlist(cfg$catalog_config$duration_labels)
  labels[names(overrides)] <- overrides
  out <- unname(labels[duration])
  ifelse(is.na(out), duration, out)
}

#' The challenge's variables with their catalog names
#'
#' @inheritParams config_get
#' @param metadata Variable descriptions from [read_variable_metadata()].
#' @return [target_variables()] plus `long_name`, `description`, `label`
#'   (from [duration_label()]) and `catalog_name`, e.g. `Daily_Chlorophyll_a`.
#' @export
catalog_variables <- function(cfg, metadata = read_variable_metadata(cfg)) {
  vars <- target_variables(cfg) |>
    dplyr::left_join(metadata, by = c("variable", "duration"))
  missing <- vars[is.na(vars$long_name), ]
  missing_names <- paste(missing$variable, missing$duration)
  if (nrow(missing) > 0) {
    cli::cli_warn(c(
      "The target metadata sheet has no row for {.val {missing_names}}.",
      "i" = "Their catalog names use the variable name instead."
    ))
  }
  vars |>
    dplyr::mutate(
      long_name = dplyr::coalesce(.data$long_name, .data$variable),
      label = duration_label(.data$duration, cfg),
      catalog_name = paste0(.data$label, "_", .data$long_name)
    )
}

#' Dates and sites of every model's bundled forecasts or scores
#'
#' One grouped scan of a bundled dataset, restricted to this challenge's
#' project and durations.
#'
#' @inheritParams bucket_uri
#' @param folder Folder of the bundled dataset in the bucket.
#' @param conn duckdb connection with read access to the bucket.
#' @return A tibble with one row per model, variable, duration and site:
#'   `first` and `last` (forecasted datetimes), `last_reference`
#'   (latest reference datetime) and `last_pub` (latest submission time).
#' @export
catalog_inventory <- function(cfg, key, folder, conn = duckdbfs::cached_connection()) {
  durations <- unique(target_variables(cfg)$duration)
  uri <- bucket_uri(cfg, key, folder, paste0("project_id=", cfg$project_id))
  t0 <- Sys.time()
  inventory <- duckdbfs::open_dataset(uri, conn = conn) |>
    dplyr::filter(.data$duration %in% !!durations, !is.na(.data$model_id)) |>
    dplyr::summarise(
      first = min(.data$datetime, na.rm = TRUE),
      last = max(.data$datetime, na.rm = TRUE),
      last_reference = max(.data$reference_datetime, na.rm = TRUE),
      last_pub = max(.data$pub_datetime, na.rm = TRUE),
      .by = c("model_id", "variable", "duration", "site_id")
    ) |>
    dplyr::collect()
  log_step("Inventory of {.path {uri}}: {nrow(inventory)} model/variable/site rows ",
           "in {round(as.numeric(difftime(Sys.time(), t0, units = 'secs')))}s")
  inventory
}

# Settings that differ between the forecasts, summaries and scores collections.
catalog_kind <- function(kind, cfg) {
  challenge <- cfg$challenge_long_name %||% cfg$project_id
  access <- paste(
    "You can access the %s at the top level of the dataset where all models, variables,",
    "and dates that forecasts were produced (reference_datetime) are available. The code",
    "to access the entire dataset is provided as an asset. Given the size of the dataset,",
    "it can be time-consuming to access the data at the full dataset level. For quicker",
    "access to a particular model (model_id), we also provide the code to access the data",
    "at the model_id level as an asset for each model."
  )
  switch(kind,
    forecasts = list(
      dir = catalog_subdir(cfg, "forecast_path", "forecasts"),
      id = "daily-forecasts", title = "Forecasts", keyword = "Forecasts", suffix = "forecast",
      bucket = "forecasts_bucket", folder = "bundled-parquet", inventory = "summaries",
      thumbnail = "forecasts_thumbnail",
      description = paste(
        "Forecasts are the raw forecasts that include all ensemble members or distribution",
        "parameters. Due to the size of the raw forecasts, we recommend accessing the scores",
        "(summaries of the forecasts) to analyze forecasts (unless you need the individual",
        "ensemble members).", sprintf(access, "forecasts")
      ),
      model_note = paste(
        "Forecasts are the raw forecasts that include all ensemble members or distribution",
        "parameters. Due to the size of the raw forecasts, we recommend accessing the",
        "forecast summaries or scores to analyze forecasts (unless you need the individual",
        "ensemble members)."
      )
    ),
    summaries = list(
      dir = catalog_subdir(cfg, "summaries_path", "summaries"),
      id = "summaries", title = "Forecast Summaries", keyword = "Summaries",
      suffix = "summaries", bucket = "summaries_bucket", folder = "bundled-summaries",
      inventory = "summaries", thumbnail = "summaries_thumbnail",
      description = paste(
        "Summaries are the forecast statistics of the raw forecasts (i.e., mean, median,",
        "confidence intervals).", sprintf(access, "summaries")
      ),
      model_note = paste(
        "Summaries are the forecast statistics of the raw forecasts (i.e., mean, median,",
        "confidence intervals)."
      )
    ),
    scores = list(
      dir = catalog_subdir(cfg, "scores_path", "scores"),
      id = "daily-scores", title = "Scores", keyword = "Scores", suffix = "scores",
      bucket = "scores_bucket", folder = "bundled-parquet", inventory = "scores",
      thumbnail = "scores_thumbnail",
      description = paste(
        "The catalog contains scores for the", paste0(challenge, "."), "The scores are",
        "summaries of the forecasts (i.e., mean, median, confidence intervals), matched",
        "observations (if available), and scores (metrics of how well the model",
        "distribution compares to observations).", sprintf(access, "scores")
      ),
      model_note = paste(
        "Scores are metrics that describe how well forecasts compare to observations. The",
        "scores catalog includes summaries of the forecasts (i.e., mean, median, confidence",
        "intervals), matched observations (if available), and scores (metrics of how well",
        "the model distribution compares to observations)."
      )
    ),
    cli::cli_abort("Unknown catalog kind {.val {kind}}.")
  )
}

#' Build the forecasts, summaries or scores collections
#'
#' Replaces the previous build of `kind`, so variables and models that no
#' longer have data, or are no longer registered, disappear.
#'
#' @param kind `"forecasts"`, `"summaries"` or `"scores"`.
#' @inheritParams config_get
#' @param inventory A list with `scores` and (for forecasts and summaries)
#'   `summaries`, each from [catalog_inventory()].
#' @param sites Site metadata from [read_sites()].
#' @param registered Registered models from [read_registered_models()].
#' @param variables Variables from [catalog_variables()].
#' @param out_dir Root of the catalog.
#' @param schema Columns of the bundled dataset, as a tibble with `name` and
#'   `type`. Defaults to reading the dataset's schema from the bucket.
#' @return The target groups written, invisibly.
#' @export
build_model_collection <- function(kind, cfg, inventory, sites, registered, variables, out_dir,
                                   schema = NULL) {
  spec <- catalog_kind(kind, cfg)
  inv <- inventory[[spec$inventory]]
  # A model's first date and latest submission time come from either
  # inventory: summaries were bundled later than scores, but some reach back
  # further, and older scores lack pub_datetime.
  keys <- c("model_id", "variable", "duration")
  both <- dplyr::bind_rows(inv, inventory$scores)
  first <- dplyr::summarise(both, first = min(.data$first), .by = dplyr::all_of(keys))
  pub <- both |>
    dplyr::filter(!is.na(.data$last_pub)) |>
    dplyr::summarise(last_pub = max(.data$last_pub), .by = dplyr::all_of(keys))
  models <- inv |>
    dplyr::summarise(last = max(.data$last), last_reference = max(.data$last_reference),
                     .by = dplyr::all_of(keys)) |>
    dplyr::left_join(first, by = keys) |>
    dplyr::left_join(pub, by = keys)
  model_sites <- inv |>
    dplyr::filter(.data$site_id %in% sites$site_id) |>
    dplyr::distinct(.data$model_id, .data$variable, .data$duration, .data$site_id) |>
    dplyr::arrange(.data$site_id)

  data_path <- c(spec$bucket, spec$folder)
  if (is.null(schema)) {
    bucket <- arrow_bucket(cfg, spec$bucket, spec$folder, anonymous = TRUE)
    schema <- arrow_schema(arrow::open_dataset(bucket)$schema)
  }
  ctx <- list(
    kind = kind, spec = spec, cfg = cfg, models = models, model_sites = model_sites,
    sites = sites, registered = registered, data_path = data_path,
    columns = stac_columns(schema, column_descriptions(kind)),
    root = file.path(out_dir, spec$dir)
  )
  unlink(list.dirs(ctx$root, recursive = FALSE), recursive = TRUE)

  groups <- purrr::keep(names(cfg$target_groups), function(group) {
    build_group_collection(group, dplyr::filter(variables, .data$group == !!group), ctx)
  })

  all_sites <- unique(model_sites$site_id)
  collection <- stac_collection(
    id = spec$id, title = spec$title, description = spec$description,
    links = c(
      purrr::map(groups, function(g) stac_link("child", paste0(g, "/collection.json"), title = g)),
      stac_standard_links(cfg, parent = "../catalog.json", root = "../catalog.json")
    ),
    bbox = stac_bbox(sites, all_sites),
    start = min(models$first), end = max(models$last),
    columns = ctx$columns,
    assets = purrr::compact(list(
      data = parquet_asset(public_s3_uri(cfg, spec$bucket, spec$folder,
                                         paste0("project_id=", cfg$project_id)),
                           paste("all", kind, "for the challenge")),
      thumbnail = thumbnail_asset(cfg$catalog_config[[spec$thumbnail]],
                                  cfg$catalog_config[[paste0(spec$thumbnail, "_title")]])
    ))
  )
  write_stac(collection, file.path(ctx$root, "collection.json"))
  log_step("{spec$title}: {length(groups)} group{?s}, ",
           "{length(list.files(ctx$root, recursive = TRUE))} files")
  invisible(groups)
}

# One target group's collection, with its variables and models. Returns
# whether the group had any data (and so was written).
build_group_collection <- function(group, variables, ctx) {
  cfg <- ctx$cfg
  group_models <- dplyr::filter(ctx$models, .data$variable %in% variables$variable)
  if (nrow(group_models) == 0) {
    log_step("{ctx$spec$title}: no data for {group}")
    return(FALSE)
  }
  written <- purrr::map(seq_len(nrow(variables)), function(i) {
    build_variable_collection(variables[i, ], group, ctx)
  }) |>
    purrr::compact()
  written_vars <- dplyr::filter(variables, .data$catalog_name %in% unlist(written))

  settings <- group_config(cfg, group)
  sites <- ctx$model_sites |>
    dplyr::filter(.data$variable %in% variables$variable) |>
    dplyr::pull("site_id") |>
    unique()
  var_list <- paste0("\"", unique(variables$variable), "\"", collapse = ", ")
  collection <- stac_collection(
    id = group, title = group,
    description = paste0("All variables for the ", group, " group."),
    `sci:doi` = cfg$catalog_config$citation_doi,
    `sci:publications` = publications(cfg, written_vars),
    extensions = c("scientific", "table"),
    links = c(
      purrr::map(written_vars$catalog_name, function(v) {
        stac_link("child", paste0(v, "/collection.json"))
      }),
      stac_standard_links(cfg, parent = "../collection.json", root = "../../catalog.json")
    ),
    bbox = stac_bbox(ctx$sites, sites),
    start = min(group_models$first), end = max(group_models$last),
    columns = ctx$columns,
    assets = purrr::compact(list(
      data = parquet_asset(
        public_s3_uri(cfg, ctx$data_path[1], ctx$data_path[2],
                      paste0("project_id=", cfg$project_id)),
        paste(ctx$kind, "for the", group, "variables"),
        filter = paste0("dplyr::filter(variable %in% c(", var_list, "))")
      ),
      thumbnail = thumbnail_asset(settings$thumbnail_link, settings$thumbnail_title)
    ))
  )
  write_stac(collection, file.path(ctx$root, group, "collection.json"))
  TRUE
}

# The challenge's citation plus those of the variables, for sci:publications.
# The config marks variables without a paper with var_doi "None".
publications <- function(cfg, variables) {
  cc <- cfg$catalog_config
  pubs <- dplyr::distinct(
    tibble::tibble(doi = c(cc$citation_doi, variables$var_doi),
                   citation = c(cc$citation_text, variables$var_citation)),
    .data$doi, .keep_all = TRUE
  )
  pubs <- pubs[!is.na(pubs$doi) & !pubs$doi %in% c("", "None"), ]
  purrr::map2(pubs$doi, pubs$citation, function(doi, citation) list(doi = doi, citation = citation))
}

# One variable's collection and its model items. Returns the variable's
# catalog name, or NULL if it has no data.
build_variable_collection <- function(var, group, ctx) {
  cfg <- ctx$cfg
  models <- dplyr::filter(ctx$models, .data$variable == var$variable,
                          .data$duration == var$duration)
  if (nrow(models) == 0) return(NULL)
  registered <- sort(intersect(models$model_id, ctx$registered$model_id))
  # Dates cover the registered models, or all models if none is registered.
  dated <- models
  if (length(registered) > 0) dated <- dplyr::filter(models, .data$model_id %in% registered)
  sites <- ctx$model_sites |>
    dplyr::filter(.data$variable == var$variable, .data$duration == var$duration) |>
    dplyr::pull("site_id") |>
    unique()
  dir <- file.path(ctx$root, group, var$catalog_name)
  partition <- c(paste0("project_id=", cfg$project_id), paste0("duration=", var$duration),
                 paste0("variable=", var$variable))

  collection <- stac_collection(
    id = var$catalog_name, title = var$catalog_name,
    description = paste0("All models for the ", var$catalog_name, " variable. ",
                         "The variable description is as follows: ", var$description),
    links = c(
      purrr::map(registered, function(m) {
        list(rel = "item", type = "application/json", href = paste0("./models/", m, ".json"))
      }),
      stac_standard_links(cfg, parent = "../collection.json", root = "../../../catalog.json")
    ),
    bbox = stac_bbox(ctx$sites, sites),
    start = min(dated$first), end = max(dated$last),
    columns = ctx$columns,
    assets = purrr::compact(list(
      data = parquet_asset(public_s3_uri(cfg, ctx$data_path[1], ctx$data_path[2], partition),
                           paste(ctx$kind, "of this variable by every model")),
      thumbnail = thumbnail_asset(group_config(cfg, group)$thumbnail_link, "Thumbnail Image")
    ))
  )
  write_stac(collection, file.path(dir, "collection.json"))

  purrr::walk(registered, function(m) {
    item <- model_item(m, dplyr::filter(models, .data$model_id == m), var, group, partition, ctx)
    write_stac(item, file.path(dir, "models", paste0(m, ".json")))
  })
  var$catalog_name
}

# The STAC item for one model's forecasts (or summaries, or scores) of one
# variable.
model_item <- function(m, dates, var, group, partition, ctx) {
  cfg <- ctx$cfg
  cc <- cfg$catalog_config
  reg <- as.list(ctx$registered[ctx$registered$model_id == m, ])
  sites <- ctx$model_sites |>
    dplyr::filter(.data$model_id == m, .data$variable == var$variable,
                  .data$duration == var$duration) |>
    dplyr::pull("site_id")
  site_rows <- ctx$sites[match(sites, ctx$sites$site_id), ]
  code_url <- if (is.na(reg$code_url)) cfg$challenge_url else reg$code_url
  catalog_repo <- config_get(cfg, "catalog_config", "catalog_repo", default = cfg$github_repo)
  item_path <- paste(ctx$spec$dir, group, var$catalog_name, "models", paste0(m, ".json"), sep = "/")
  raw_url <- paste0("https://raw.githubusercontent.com/", catalog_repo, "/main/", item_path)
  metadata_url <- bucket_uri(cfg, "model_metadata_bucket", paste0(m, ".json"), scheme = "https")
  data_uri <- public_s3_uri(cfg, ctx$data_path[1], ctx$data_path[2], partition,
                            paste0("model_id=", m))
  s3_path <- sub("\\?.*$", "", sub("^s3://anonymous@", "s3://", data_uri))
  endpoint <- store_credentials(cfg, "main", anonymous = TRUE)$endpoint
  producer <- if (identical(reg$public_contact, "Yes")) {
    list(url = code_url, name = reg$contact_name)
  } else {
    list(url = cfg$challenge_url, name = cfg$challenge_long_name)
  }
  title <- paste(var$label, var$long_name)

  list(
    stac_version = "1.0.0",
    stac_extensions = list(stac_extension_urls[["table"]]),
    type = "Feature",
    id = paste(m, var$variable, var$duration, ctx$spec$suffix, sep = "_"),
    bbox = stac_bbox(site_rows),
    geometry = list(
      type = cc$site_type %||% "MultiPoint",
      coordinates = purrr::map2(site_rows$longitude, site_rows$latitude, c)
    ),
    properties = list(
      title = m,
      description = paste0(
        "All ", ctx$kind, " for the ", var$catalog_name, " variable for the ", m,
        " model. Information for the model is provided as follows: ", reg$description,
        ". The model predicts this variable at the following sites: ",
        paste(sites, collapse = ", "), ". ", ctx$spec$model_note
      ),
      datetime = stac_datetime(dates$last_reference),
      updated = stac_datetime(dplyr::coalesce(dates$last_pub, dates$last_reference)),
      start_datetime = stac_datetime(dates$first),
      end_datetime = stac_datetime(dates$last),
      providers = list(
        c(producer, list(roles = list("producer", "processor", "licensor"))),
        list(url = cc$host_url, name = cc$host_name, roles = list("host"))
      ),
      license = "CC0-1.0",
      keywords = as.list(c(ctx$spec$keyword, cfg$project_id, group, m, var$long_name,
                           var$variable, var$label, var$duration, sites)),
      `table:columns` = ctx$columns
    ),
    collection = ctx$kind,
    links = list(
      list(rel = "collection", href = "../collection.json", type = "application/json",
           title = var$catalog_name),
      list(rel = "root", href = "../../../../catalog.json", type = "application/json",
           title = "Forecast Catalog"),
      list(rel = "parent", href = "../collection.json", type = "application/json",
           title = var$catalog_name),
      list(rel = "self", href = paste0(m, ".json"), type = "application/json",
           title = "Model Forecast"),
      list(rel = "item", href = code_url, type = "text/html", title = "Link for Model Code"),
      list(rel = "item",
           href = paste0("https://radiantearth.github.io/stac-browser/#/external/",
                         sub("^https://", "", raw_url)),
           type = "text/html", title = "Link for rendered STAC item"),
      list(rel = "item", href = raw_url, type = "text/html", title = "Link for raw JSON file")
    ),
    assets = list(
      "1" = list(
        type = "application/json", title = "Model Metadata", href = metadata_url,
        description = paste0(
          "Use `jsonlite::fromJSON()` to download the model metadata JSON file. This R code ",
          "will return metadata provided during the model registration.\n\n### R\n\n```{r}\n",
          "# Use code below\n\nmodel_metadata <- jsonlite::fromJSON(\"", metadata_url, "\")\n```"
        )
      ),
      "2" = list(type = "text/html", title = "Link for Model Code", href = code_url,
                 description = "The link to the model code provided by the model submission team"),
      "3" = list(type = "text/plain", title = "Model Sites",
                 href = config_get(cfg, "sites", "metadata_url", default = cc$site_metadata_url),
                 description = paste(sites, collapse = ", ")),
      "4" = list(
        type = "application/x-parquet",
        title = paste("Database Access for", title),
        href = data_uri,
        description = paste0(
          "Use `R` or `Python` code for remote access to the database. This code will return ",
          "results for this variable and model combination.\n\n### R\n\n```{r}\n",
          "# Use code below\n\nall_results <- arrow::open_dataset(\"", data_uri, "\")\n",
          "df <- all_results |> dplyr::collect()\n```\n\n",
          "You can use dplyr operations before calling `dplyr::collect()` to `summarise`, ",
          "`select` columns, and/or `filter` rows prior to pulling the data into a local ",
          "`data.frame`. Reducing the data that is pulled locally will speed up the data ",
          "download speed and reduce your memory usage.\n\n### Python\n\n```{python}\n",
          "# Use code below\nimport ibis\ncon = ibis.duckdb.connect()\n",
          "con.raw_sql(\"CREATE OR REPLACE SECRET secret (TYPE S3, ENDPOINT '", endpoint,
          "', URL_STYLE 'path')\")\n",
          "df = con.read_parquet(\"", s3_path, "/**\")\n```"
        )
      )
    )
  )
}

#' Build the root catalog, NOAA, sites and targets collections
#'
#' @inheritParams config_get
#' @param out_dir Root of the catalog.
#' @param sites Site metadata from [read_sites()].
#' @param targets Output of [targets_summary()].
#' @return The path written, invisibly.
#' @export
build_root_catalog <- function(cfg, out_dir) {
  cc <- cfg$catalog_config
  child <- function(title, key, default) {
    stac_link("child", paste0(catalog_subdir(cfg, key, default), "/collection.json"), title = title)
  }
  catalog <- list(
    type = "Catalog",
    id = paste0(cfg$project_id, "-stac"),
    title = paste(cfg$challenge_long_name, "Catalog"),
    description = paste0("A STAC (Spatiotemporal Asset Catalog) describing forecasts and ",
                         "forecast scores for the ", cfg$project_id, " Forecasting Challenge"),
    stac_version = "1.0.0",
    links = purrr::compact(list(
      stac_link("self", "catalog.json"),
      stac_link("root", "catalog.json"),
      child("Forecasts", "forecast_path", "forecasts"),
      child("Scores", "scores_path", "scores"),
      if (!is.null(cfg$noaa_forecasts)) child("NOAA Forecasts", "noaa_path", "noaa_forecasts"),
      child("Targets", "targets_path", "targets"),
      child("Forecast Summaries", "summaries_path", "summaries"),
      child("Sites", "site_path", "sites"),
      stac_link("parent", cc$parent_catalog_url)
    ))
  )
  write_stac(catalog, file.path(out_dir, "catalog.json"))
}

#' @rdname build_root_catalog
#' @export
build_noaa_collection <- function(cfg, sites, out_dir) {
  cc <- cfg$catalog_config
  noaa <- config_get(cfg, "noaa_forecasts")
  products <- unlist(noaa$noaa_forecast_groups)
  paths <- unlist(noaa$noaa_forecast_group_paths)
  root <- file.path(out_dir, catalog_subdir(cfg, "noaa_path", "noaa_forecasts"))
  fs <- arrow_bucket(cfg, "noaa_forecast_bucket", anonymous = TRUE)
  schemas <- purrr::map(rlang::set_names(paths), function(p) {
    # Reads only the file's footer, not its data.
    reader <- arrow::ParquetFileReader$create(fs$OpenInputFile(first_parquet_file(fs, p)))
    stac_columns(arrow_schema(reader$GetSchema()), column_descriptions("noaa"))
  })
  start <- as.Date(noaa$start_date %||% "2020-09-24")
  end <- Sys.Date()
  description <- paste(
    "The catalog contains NOAA forecasts used for the", paste0(cfg$challenge_long_name, "."),
    "The forecasts are the raw forecasts that include all ensemble members (if a forecast",
    "represents uncertainty using an ensemble). You can access the forecasts at the top",
    "level of each product, where all dates that forecasts were produced",
    "(reference_datetime) and sites are available. The code to access the data is",
    "provided as an asset."
  )
  thumbnail <- thumbnail_asset(cc$noaa_thumbnail, cc$noaa_thumbnail_title)
  bbox <- stac_bbox(sites)

  purrr::walk2(products, paths, function(product, path) {
    collection <- stac_collection(
      id = product, title = product, description = description,
      extensions = c("scientific", "item_assets", "table"),
      links = stac_standard_links(cfg, parent = "../collection.json", root = "../../catalog.json"),
      bbox = bbox, start = start, end = end, columns = schemas[[path]],
      assets = purrr::compact(list(
        data = parquet_asset(public_s3_uri(cfg, "noaa_forecast_bucket", path),
                             paste("the", product, "NOAA forecasts")),
        thumbnail = thumbnail
      ))
    )
    write_stac(collection, file.path(root, product, "collection.json"))
  })

  # The top level shows the stage 2 columns: hourly forecasts, the product
  # most participants use.
  top_schema <- schemas[[if ("stage2" %in% paths) "stage2" else paths[1]]]
  collection <- stac_collection(
    id = "noaa-forecasts", title = "NOAA-Forecasts", description = description,
    links = c(
      purrr::map(products, function(p) {
        stac_link("child", paste0(p, "/collection.json"), title = p)
      }),
      stac_standard_links(cfg, parent = "../catalog.json", root = "../catalog.json")
    ),
    bbox = bbox, start = start, end = end, columns = top_schema,
    assets = purrr::compact(list(
      data = parquet_asset(public_s3_uri(cfg, "noaa_forecast_bucket", paths[1]),
                           "NOAA forecasts; change the folder to choose a product"),
      thumbnail = thumbnail
    ))
  )
  write_stac(collection, file.path(root, "collection.json"))
}

# The newest parquet file under `path`: descends through the last folder at
# each level, so only a few small listings are needed.
first_parquet_file <- function(fs, path) {
  for (depth in 1:10) {
    info <- fs$GetFileInfo(arrow::FileSelector$create(path, recursive = FALSE))
    listing <- tibble::tibble(
      path = purrr::map_chr(info, function(x) x$path),
      type = purrr::map_int(info, function(x) as.integer(x$type))
    ) |>
      dplyr::arrange(.data$path)
    files <- listing$path[listing$type == as.integer(arrow::FileType$File) &
                            grepl("\\.parquet$", listing$path)]
    if (length(files) > 0) return(files[length(files)])
    dirs <- listing$path[listing$type == as.integer(arrow::FileType$Directory)]
    if (length(dirs) == 0) break
    path <- dirs[length(dirs)]
  }
  cli::cli_abort("No parquet file found under {.path {path}}.")
}

#' Date range and columns of the published targets
#'
#' Reads only the `datetime` column of every group's targets file, and the
#' first rows of one file for its column types.
#'
#' @inheritParams config_get
#' @return A list with `start`, `end` and `schema` (from the first file).
#' @export
targets_summary <- function(cfg) {
  files <- unique(stats::na.omit(target_variables(cfg)$targets_file))
  datetimes <- readr::read_csv(files, col_select = "datetime", show_col_types = FALSE,
                               progress = FALSE)$datetime
  sample <- readr::read_csv(files[1], n_max = 1000, show_col_types = FALSE, progress = FALSE)
  list(start = min(datetimes, na.rm = TRUE), end = max(datetimes, na.rm = TRUE),
       schema = r_schema(sample))
}

#' @rdname build_root_catalog
#' @export
build_sites_collection <- function(cfg, targets, out_dir) {
  cc <- cfg$catalog_config
  url <- config_get(cfg, "sites", "metadata_url", default = cc$site_metadata_url)
  site_df <- readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
  descriptions <- character()
  if (!is.null(cc$site_columns_file) && file.exists(cc$site_columns_file)) {
    d <- readr::read_csv(cc$site_columns_file, show_col_types = FALSE, progress = FALSE)
    descriptions <- rlang::set_names(d$description, d$column)
  }
  collection <- stac_collection(
    id = "sites", title = "Site Metadata",
    description = paste("The catalog contains site metadata for the", cfg$challenge_long_name),
    extensions = c("scientific", "item_assets", "table"),
    links = stac_standard_links(cfg, parent = "../catalog.json", root = "../catalog.json"),
    bbox = config_bbox(cfg), start = targets$start, end = targets$end,
    columns = stac_columns(r_schema(site_df), descriptions),
    assets = purrr::compact(list(
      data = csv_asset(url, "Site Metadata Access"),
      thumbnail = thumbnail_asset(cc$site_thumbnail, cc$site_thumbnail_title)
    ))
  )
  write_stac(collection, file.path(out_dir, catalog_subdir(cfg, "site_path", "sites"),
                                   "collection.json"))
}

#' @rdname build_root_catalog
#' @export
build_targets_collection <- function(cfg, targets, out_dir) {
  cc <- cfg$catalog_config
  files <- purrr::compact(purrr::map(cfg$target_groups, "targets_file"))
  assets <- c(
    list(thumbnail = thumbnail_asset(cc$targets_thumbnail, cc$targets_thumbnail_title)),
    purrr::imap(files, function(url, group) csv_asset(url, paste(group, "Target Access")))
  )
  collection <- stac_collection(
    id = "targets", title = "Targets",
    description = paste("The targets are observations that can be used to evaluate and",
                        "build forecasts. We provide the code to access different targets",
                        "as an asset."),
    links = stac_standard_links(cfg, parent = "../catalog.json", root = "../catalog.json"),
    bbox = config_bbox(cfg), start = targets$start, end = targets$end,
    columns = stac_columns(targets$schema, column_descriptions("targets")),
    assets = purrr::compact(assets)
  )
  write_stac(collection, file.path(out_dir, catalog_subdir(cfg, "targets_path", "targets"),
                                   "collection.json"))
}

# The challenge's bounding box: catalog_config$bbox, or else that of all sites.
config_bbox <- function(cfg) {
  b <- cfg$catalog_config$bbox
  if (is.null(b)) return(stac_bbox(read_sites(cfg)))
  as.numeric(c(b$min_lon, b$min_lat, b$max_lon, b$max_lat))
}
