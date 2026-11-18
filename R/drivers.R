# NOAA GEFS weather driver forecasts.
#
#   stage1        GEFS 6-hourly forecasts at each site (gefs4cast)
#   stage1-stats  GEFS ensemble mean and spread (gefs4cast)
#   pseudo        GEFS 0-6 h forecasts from every cycle, a stand-in for
#                 observed weather (gefs4cast)
#   stage2        stage1 interpolated to hourly, in standard variable names
#   stage3        pseudo interpolated to hourly: one continuous series per site
#
# All of them live under the config's `noaa_forecast_bucket`, in the folders
# named by `noaa_forecasts$noaa_forecast_group_paths`.

#' Variables kept from GEFS for the hourly driver products
#' @noRd
gefs_variables <- c("PRES", "TMP", "RH", "UGRD", "VGRD", "APCP", "DSWRF", "DLWRF")

#' An arrow file system for one NOAA driver product
#'
#' For reading, this is the product's folder in the NOAA bucket, anonymous
#' when no credentials are set. For writing (`write = TRUE`), it is the same
#' folder with the main store's credentials, or in a dry run a local folder
#' under [dry_run_dir()]. That way a dry run still reads what exists remotely
#' but writes nothing there.
#'
#' @inheritParams config_get
#' @param product Product folder, e.g. `"stage2"`. One of the config's
#'   `noaa_forecasts$noaa_forecast_group_paths`.
#' @param write Return a destination to write to.
#' @return An arrow `SubTreeFileSystem`.
#' @export
noaa_store <- function(cfg, product, write = FALSE) {
  rlang::check_installed("arrow")
  products <- unlist(config_get(cfg, "noaa_forecasts", "noaa_forecast_group_paths"))
  if (!product %in% products) {
    cli::cli_abort(
      "{.val {product}} is not one of {.field noaa_forecast_group_paths}: {.val {products}}."
    )
  }
  if (write && is_dry_run()) {
    local <- output_uri(bucket_uri(cfg, "noaa_forecast_bucket", product))
    dir.create(local, recursive = TRUE, showWarnings = FALSE)
    return(arrow::SubTreeFileSystem$create(local, arrow::LocalFileSystem$create()))
  }
  arrow_bucket(cfg, "noaa_forecast_bucket", product,
               anonymous = !write && !has_credentials(cfg, "noaa"))
}

#' Sites that driver forecasts are made for
#'
#' Every site in [read_sites()], with numeric coordinates. The stage 1 GEFS
#' download can use a different site list (`noaa_forecasts$stage1_site_list_url`,
#' a CSV with `site_id`, `latitude` and `longitude`) so that its coordinates,
#' and so the GEFS grid cells it extracts, stay the same as the data already
#' stored.
#'
#' @inheritParams config_get
#' @param stage1 Return the stage 1 download's site list.
#' @return A tibble with `site_id`, `latitude` and `longitude`.
#' @export
driver_sites <- function(cfg, stage1 = FALSE) {
  url <- if (stage1) cfg$noaa_forecasts$stage1_site_list_url else NULL
  sites <- if (is.null(url)) {
    read_sites(cfg)
  } else {
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
  }
  sites |>
    dplyr::transmute(.data$site_id,
                     latitude = as.numeric(.data$latitude),
                     longitude = as.numeric(.data$longitude))
}

#' Interpolate 6-hourly GEFS forecasts to hourly driver data
#'
#' * States (pressure, temperature, humidity, wind) are interpolated linearly.
#'   Temperature is converted to kelvin by adding 273 and humidity to a
#'   fraction.
#' * Fluxes (precipitation and radiation) are 6-hour averages, so each hour
#'   takes the value of the 6-hour period it falls in. Precipitation is
#'   converted from kg m-2 per 6 hours to kg m-2 s-1. With `use_solar_geom`,
#'   shortwave radiation is spread over the day following the sun's angle.
#' * Variables are renamed to CF standard names.
#'
#' This is the code the driver scripts have always used, moved here unchanged.
#' Two things in it look wrong but are kept, because changing them would make
#' new data inconsistent with what is stored: 273 rather than 273.15, and UGRD
#' (the eastward wind component) being named `northward_wind`, with VGRD as
#' `eastward_wind`.
#'
#' @param df GEFS forecasts for one or more sites, in gefs4cast's long format.
#' @param sites Site coordinates (`site_id`, `latitude`, `longitude`), needed
#'   when `use_solar_geom = TRUE`.
#' @param use_solar_geom Spread shortwave radiation by solar geometry.
#' @param psuedo `TRUE` for pseudo-observations (from several cycles, with no
#'   single reference time).
#' @return Hourly driver data with the same columns as `df`.
#' @export
to_hourly <- function(df, sites = NULL, use_solar_geom = TRUE, psuedo = FALSE) {
  rlang::check_installed(c("imputeTS", "tidyr"))
  if (!psuedo) {
    reference_datetime <- lubridate::as_datetime(df$reference_datetime)[1]
  } else {
    reference_datetime <- NA
  }

  var_order <- names(df)

  ensemble_maxtime <- df |>
    dplyr::group_by(.data$site_id, .data$family, .data$ensemble) |>
    dplyr::summarise(max_time = max(.data$datetime), .groups = "drop")

  ensembles <- unique(df$ensemble)
  datetime <- seq(min(df$datetime), max(df$datetime), by = "1 hour")
  variables <- unique(df$variable)
  site_ids <- unique(df$site_id)

  full_time <- expand.grid(site_ids, ensembles, datetime, variables) |>
    dplyr::rename(site_id = "Var1", ensemble = "Var2", datetime = "Var3",
                  variable = "Var4") |>
    dplyr::mutate(datetime = lubridate::as_datetime(.data$datetime)) |>
    dplyr::arrange(.data$site_id, .data$ensemble, .data$variable, .data$datetime) |>
    dplyr::left_join(ensemble_maxtime, by = c("site_id", "ensemble")) |>
    dplyr::filter(.data$datetime <= .data$max_time) |>
    dplyr::select(-"max_time") |>
    dplyr::distinct()

  states <- df |>
    dplyr::select("site_id", "family", "horizon", "ensemble", "datetime", "variable",
                  "prediction") |>
    dplyr::filter(!psuedo | (psuedo & .data$horizon != "006") |
                    (psuedo & .data$datetime == max(df$datetime))) |>
    dplyr::select(-"horizon") |>
    dplyr::group_by(.data$site_id, .data$family, .data$ensemble, .data$variable) |>
    dplyr::right_join(full_time,
                      by = c("site_id", "ensemble", "datetime", "family", "variable")) |>
    dplyr::filter(.data$variable %in% c("PRES", "RH", "TMP", "UGRD", "VGRD")) |>
    dplyr::arrange(.data$site_id, .data$family, .data$ensemble, .data$datetime) |>
    dplyr::mutate(prediction = imputeTS::na_interpolation(.data$prediction,
                                                          option = "linear")) |>
    dplyr::mutate(prediction = ifelse(.data$variable == "TMP", .data$prediction + 273,
                                      .data$prediction)) |>
    dplyr::mutate(prediction = ifelse(.data$variable == "RH", .data$prediction / 100,
                                      .data$prediction)) |>
    dplyr::ungroup()

  fluxes <- df |>
    dplyr::select("site_id", "family", "horizon", "ensemble", "datetime", "variable",
                  "prediction") |>
    dplyr::filter(.data$horizon != "003") |>
    dplyr::select(-"horizon") |>
    dplyr::group_by(.data$site_id, .data$family, .data$ensemble, .data$variable) |>
    dplyr::right_join(full_time,
                      by = c("site_id", "ensemble", "datetime", "family", "variable")) |>
    dplyr::filter(.data$variable %in% c("APCP", "DSWRF", "DLWRF")) |>
    dplyr::arrange(.data$site_id, .data$family, .data$ensemble, .data$datetime) |>
    tidyr::fill("prediction", .direction = "up") |>
    dplyr::mutate(prediction = ifelse(.data$variable == "APCP",
                                      .data$prediction / (6 * 60 * 60), .data$prediction),
                  variable = ifelse(.data$variable == "APCP", "PRATE", .data$variable)) |>
    dplyr::ungroup()

  if (use_solar_geom) {
    if (is.null(sites)) cli::cli_abort("{.arg sites} is needed when {.code use_solar_geom = TRUE}.")
    fluxes <- fluxes |>
      dplyr::left_join(dplyr::select(sites, "site_id", "latitude", "longitude"),
                       by = "site_id") |>
      dplyr::mutate(hour = lubridate::hour(.data$datetime),
                    date = lubridate::as_date(.data$datetime),
                    doy = lubridate::yday(.data$datetime) + .data$hour / 24,
                    longitude = ifelse(.data$longitude < 0, 360 + .data$longitude,
                                       .data$longitude),
                    rpot = downscale_solar_geom(.data$doy, .data$longitude,
                                                .data$latitude)) |>
      dplyr::group_by(.data$site_id, .data$family, .data$ensemble, .data$date,
                      .data$variable) |>
      dplyr::mutate(avg.rpot = mean(.data$rpot, na.rm = TRUE),
                    avg.SW = mean(.data$prediction, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(prediction = ifelse(.data$variable == "DSWRF" & .data$avg.rpot > 0.0,
                                        .data$rpot * (.data$avg.SW / .data$avg.rpot),
                                        .data$prediction)) |>
      dplyr::select(dplyr::any_of(var_order))
  }

  standard_names <- c(
    TMP = "air_temperature",
    PRES = "air_pressure",
    RH = "relative_humidity",
    DLWRF = "surface_downwelling_longwave_flux_in_air",
    DSWRF = "surface_downwelling_shortwave_flux_in_air",
    PRATE = "precipitation_flux",
    VGRD = "eastward_wind",
    UGRD = "northward_wind",
    APCP = "precipitation_amount"
  )
  dplyr::bind_rows(states, fluxes) |>
    dplyr::arrange(.data$site_id, .data$family, .data$ensemble, .data$datetime) |>
    dplyr::mutate(variable = dplyr::coalesce(unname(standard_names[.data$variable]),
                                             .data$variable),
                  reference_datetime = reference_datetime) |>
    dplyr::select(dplyr::any_of(var_order))
}

# Cosine of the solar zenith angle, for spreading daily shortwave radiation
# over the hours of the day.
cos_solar_zenith_angle <- function(doy, lat, lon, dt, hr) {
  et <- equation_of_time(doy)
  merid <- floor(lon / 15) * 15
  merid[merid < 0] <- merid[merid < 0] + 15
  lc <- (lon - merid) * -4 / 60
  tz <- merid / 360 * 24
  midbin <- 0.5 * dt / 86400 * 24
  t0 <- 12 + lc - et - tz - midbin
  h <- pi / 12 * (hr - t0)
  dec <- -23.45 * pi / 180 * cos(2 * pi * (doy + 10) / 365)
  cosz <- sin(lat * pi / 180) * sin(dec) + cos(lat * pi / 180) * cos(dec) * cos(h)
  cosz[cosz < 0] <- 0
  cosz
}

equation_of_time <- function(doy) {
  stopifnot(doy <= 367)
  f <- pi / 180 * (279.5 + 0.9856 * doy)
  (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 * sin(4 * f) - 429.3 * cos(f) -
     2 * cos(2 * f) + 19.3 * cos(3 * f)) / 3600
}

downscale_solar_geom <- function(doy, lon, lat) {
  dt <- stats::median(diff(doy)) * 86400
  hr <- (doy - floor(doy)) * 24
  1366 * cos_solar_zenith_angle(doy, lat, lon, dt, hr)
}

#' Prepare raw GEFS rows for to_hourly()
#'
#' Keeps the driver variables and, for pseudo-observations, recomputes each
#' row's datetime from its reference date, cycle and horizon.
#'
#' @param df GEFS rows (collected).
#' @param psuedo Rows are pseudo-observations.
#' @return The prepared rows.
#' @noRd
prepare_gefs <- function(df, psuedo = FALSE) {
  df <- dplyr::filter(df, .data$variable %in% gefs_variables)
  if (!psuedo) return(df)
  df |>
    dplyr::mutate(
      date = lubridate::as_date(.data$reference_datetime),
      new_datetime = .data$date + lubridate::hours(as.numeric(.data$horizon)) +
        lubridate::hours(as.numeric(.data$cycle)),
      datetime = ifelse(.data$datetime != .data$new_datetime, .data$new_datetime,
                        .data$datetime),
      datetime = lubridate::as_datetime(.data$datetime)
    ) |>
    dplyr::select(-"date", -"new_datetime")
}

#' Number the ensemble members and rename them `parameter`
#'
#' GEFS members are named `gec00`, `gep01`, ...; the stored products use the
#' number.
#' @noRd
number_members <- function(df) {
  df |>
    dplyr::mutate(ensemble = as.numeric(substr(.data$ensemble, 4, 5))) |>
    dplyr::rename(parameter = "ensemble")
}

#' Download GEFS products that are missing
#'
#' Finds the days since `noaa_forecasts$start_date` that a gefs4cast product
#' does not have yet, and downloads them.
#'
#' @inheritParams config_get
#' @param product `"stage1"`, `"stage1-stats"` or `"pseudo"`.
#' @param today Today's date. Stage 1 products go up to yesterday, pseudo up to
#'   today.
#' @return The dates downloaded, invisibly.
#' @export
download_gefs <- function(cfg, product, today = Sys.Date()) {
  rlang::check_installed("gefs4cast")
  Sys.setenv(GEFS_VERSION = config_get(cfg, "noaa_forecasts", "gefs_version", default = "v12"))
  start <- as.Date(config_get(cfg, "noaa_forecasts", "start_date", default = "2020-09-24"))
  end <- if (product == "pseudo") today else today - 1
  have <- sub("reference_datetime=", "", noaa_store(cfg, product)$ls())
  missing <- seq(start, end, by = "day")
  missing <- missing[!as.character(missing) %in% have]
  log_step("{product}: {length(missing)} missing day{?s}")
  if (length(missing) == 0) return(invisible(missing))

  out <- noaa_store(cfg, product, write = TRUE)
  stage1_sites <- driver_sites(cfg, stage1 = TRUE)
  switch(product,
    "stage1-stats" = gefs4cast::gefs_to_parquet(missing, ensemble = c("geavg", "gespr"),
                                                path = out, sites = stage1_sites),
    "stage1" = gefs4cast::gefs_to_parquet(missing, path = out, sites = stage1_sites),
    "pseudo" = gefs4cast::gefs_pseudo_measures(missing, path = out, sites = driver_sites(cfg)),
    cli::cli_abort("Unknown GEFS product {.val {product}}.")
  )
  invisible(missing)
}

#' Build stage 2 for recent days that do not have it yet
#'
#' Stage 2 is stage 1 interpolated to hourly ([to_hourly()]), one
#' `reference_datetime=` folder per forecast. Only the last `days_back` days are
#' checked, so a day that stage 1 never got is not retried forever.
#'
#' @inheritParams config_get
#' @param days_back How many days before today to fill in.
#' @param today Today's date.
#' @return The reference dates built, invisibly.
#' @export
update_stage2 <- function(cfg, days_back = 14, today = Sys.Date()) {
  sites <- driver_sites(cfg)
  have <- sub("reference_datetime=", "", noaa_store(cfg, "stage2")$ls())
  wanted <- as.character(seq(today - days_back, today - 1, by = "day"))
  missing <- setdiff(wanted, have)
  log_step("Stage 2: {length(missing)} missing day{?s} in the last {days_back}")
  out <- noaa_store(cfg, "stage2", write = TRUE)

  for (date in missing) {
    log_step("Stage 2: {date}")
    stage1 <- noaa_store(cfg, "stage1")$path(paste0("reference_datetime=", date))
    hourly <- arrow::open_dataset(stage1) |>
      dplyr::filter(.data$variable %in% gefs_variables,
                    .data$site_id %in% sites$site_id) |>
      dplyr::collect() |>
      dplyr::mutate(reference_datetime = date) |>
      to_hourly(sites = sites, use_solar_geom = TRUE, psuedo = FALSE) |>
      number_members() |>
      dplyr::mutate(reference_datetime = lubridate::as_date(.data$reference_datetime))
    arrow::write_dataset(hourly, path = out, partitioning = c("reference_datetime", "site_id"))
  }
  invisible(missing)
}

#' Extend each site's stage 3 series with new pseudo-observations
#'
#' Stage 3 is one continuous hourly series per site, built from pseudo. For
#' each site this recomputes the last three days of stage 3 and everything
#' after them from pseudo, keeps the older stage 3 rows, and rewrites the
#' site's file. A site that fails is logged and the others carry on.
#'
#' With `overlap_days = Inf`, or for a site with no stage 3 yet, the site's
#' whole series is rebuilt from all of pseudo.
#'
#' @inheritParams config_get
#' @param overlap_days Days of existing stage 3 to recompute.
#' @return A tibble with one row per site: `site_id`, `rows` and `error`,
#'   invisibly.
#' @export
update_stage3 <- function(cfg, overlap_days = 3) {
  sites <- driver_sites(cfg)
  stage3 <- noaa_store(cfg, "stage3")
  out <- noaa_store(cfg, "stage3", write = TRUE)
  pseudo <- noaa_store(cfg, "pseudo")

  results <- purrr::map(sites$site_id, function(site) {
    log_step("Stage 3: {site}")
    tryCatch({
      existing <- arrow::open_dataset(stage3) |>
        dplyr::filter(.data$site_id == site) |>
        dplyr::collect()
      new <- arrow::open_dataset(pseudo) |>
        dplyr::filter(.data$variable %in% gefs_variables, .data$site_id == site)
      if (nrow(existing) > 0 && is.finite(overlap_days)) {
        cut_off <- as.character(lubridate::as_date(max(existing$datetime)) - overlap_days)
        new <- dplyr::filter(new, .data$reference_datetime >= cut_off)
      }
      new <- new |>
        dplyr::collect() |>
        prepare_gefs(psuedo = TRUE)
      if (nrow(new) == 0) {
        return(tibble::tibble(site_id = site, rows = 0L, error = NA_character_))
      }
      hourly <- new |>
        to_hourly(sites = sites, use_solar_geom = TRUE, psuedo = TRUE) |>
        number_members()
      hourly |>
        dplyr::bind_rows(dplyr::filter(existing, .data$datetime < min(hourly$datetime))) |>
        dplyr::arrange(.data$variable, .data$datetime, .data$parameter) |>
        arrow::write_dataset(path = out, partitioning = "site_id")
      tibble::tibble(site_id = site, rows = nrow(hourly), error = NA_character_)
    }, error = function(e) {
      cli::cli_inform(c("x" = "{site} failed: {conditionMessage(e)}"))
      tibble::tibble(site_id = site, rows = NA_integer_, error = conditionMessage(e))
    })
  }) |>
    purrr::list_rbind()

  failed <- results$site_id[!is.na(results$error)]
  if (length(failed) > 0) cli::cli_abort("Stage 3 failed for {.val {failed}}.")
  invisible(results)
}
