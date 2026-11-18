download.neon.avro <- function(months, data_product, path) {
  for (i in 1:length(months$site_id)) {
    # create a directory for each site (if one doesn't already exist)
    if (!paste0('site=', months$site_id[i]) %in% list.dirs(path, full.names = F)) {
      dir.create(path = paste0(path, '/site=', months$site_id[i]), recursive = TRUE, showWarnings = FALSE)
      message('creating new directory')
    }

    message(paste0('downloading ',months$site_id[i],": ", i, '/', length(months$site_id)))

    month_use <- format(seq.Date(sort(c(months$new_date[i], floor_date(Sys.Date() - days(2), 'month')))[1],
                                 sort(c(months$new_date[i], floor_date(Sys.Date() - days(2), 'month')))[2],
                                 by = 'month'),
                        "%Y-%m")
    # loop through each month (max 2) and each dp listed
    for (j in 1:length(month_use)) {

      for (dp in data_product) {
        # download the provisional files from Google Cloud Storage using the gsutils tool from cmd
        # -n argument skips files already downloaded
        dir <- path.expand('~/google-cloud-sdk/bin/gsutil')
        cmd <- paste0(dir,' -m cp -n gs://neon-is-transition-output/provisional/dpid=DP1.',
                      dp,
                      '.001/ms=',
                      # -n excludes already downloaded files
                      # -m cp runs the copy function in parallel to speed up download
                      month_use[j],
                      '/site=',
                      months$site_id[i],
                      "/* ",
                      path,
                      '/site=',
                      months$site_id[i])
        system(cmd, ignore.stderr = T)

      }

    }

  }
}
# no error is thrown if the data product doesn't exist for that site


# delete the superseded files
delete.neon.parquet <- function(months, path, data_product) {

  for (i in 1:nrow(months)) {
    site_parquet <- dir(path = path,
                        pattern = months$site_id[i])

    superseded <- site_parquet[which(lubridate::as_date(str_match(site_parquet,
                                                                  "__\\s*(.*?)\\s*.avro")[,2]) <=
                                       months$cur_wq_date[i])]

    if (length(superseded) != 0) {
      file.remove(paste0(path, '/',superseded))
    }
  }
}

# delete the superseded files
delete.neon.avro <- function(months, path, data_product) {

  for (i in 1:nrow(months)) {

    site_avro <- dir(path = paste0(path, '/site=', months$site_id[i]),
                     pattern = data_product)

    superseded <- site_avro[which(lubridate::as_date(str_match(site_avro,
                                                               "__\\s*(.*?)\\s*.avro")[,2]) <=
                                    months$cur_wq_date[i])]

    if (length(superseded) != 0) {
      file.remove(paste0(path,'/site=', months$site_id[i], '/',superseded))
    }
  }
}



# requires wq_vars - specify
read.avro.wq <- function(path, columns_keep, dir ) {
  message(paste0('reading file ', path))
  profiling_sites <- c('CRAM', 'LIRO', 'BARC', 'TOOK')

  wq_avro <- tibble::as_tibble(py_to_r(read_avro_file(path)))

  if (nrow(wq_avro) >= 1) {
    wq_tibble <- wq_avro |>
      dplyr::filter(termName %in% wq_vars) %>%
      dplyr::filter(horizontalIndex %in% c('101', '111', '103'))

    if(nrow(wq_tibble) > 0){
      # for streams want to omit the downstream measurement (102) and retain upstream (101)
      # rivers and lakes horizontal index is 103
      wq_tibble <- wq_tibble %>%

        dplyr::select(siteName, termName, startDate,
                      doubleValue, intValue) %>%
        # combine the value fields to one
        dplyr::mutate(Value = ifelse(is.na(doubleValue),
                                     intValue, doubleValue)) %>%
        dplyr::select(any_of(columns_keep)) %>%
        as.data.frame() %>%
        suppressWarnings()  %>%
        arrange(startDate, termName) %>%
        pivot_wider(names_from = termName, values_from = Value) |>
        filter_at(vars(ends_with('QF')), any_vars(. != 1)) # checks to see if any of the QF cols have a 1
    }
  } else {
    wq_tibble <- data.frame()
  }


  if (nrow(wq_tibble) >=1 ) {
    wq_tibble_wider <- wq_tibble %>%
      dplyr::mutate(sensorDepth = ifelse(siteName %in% profiling_sites,
                                         sensorDepth,
                                         0.5)) |>
      dplyr::filter(sensorDepth > 0 & sensorDepth < 1)

    # if the filtering has left rows then find the mean
    if (nrow(wq_tibble_wider) >= 1) {
      daily_wq <- wq_tibble_wider  %>%
        mutate(time = as.Date(startDate),
               # add missing columns (sites with no chlorophyll), need all otherwise function won't run
               chlorophyll = ifelse('chlorophyll' %in% colnames(wq_tibble_wider), chlorophyll, NA),
               chlorophyllExpUncert = ifelse('chlorophyll' %in% colnames(wq_tibble_wider),chlorophyllExpUncert,NA)) %>%
        group_by(siteName, time) %>%
        summarize(oxygen = mean(dissolvedOxygen, na.rm = TRUE),
                  chla = mean(chlorophyll, na.rm = TRUE),.groups = "drop") %>%
        rename(site_id = siteName) %>%
        # get in the same long format as the NEON portal data
        pivot_longer(cols = -c("time", "site_id"),
                     names_to = "variable",
                     values_to = "observation")
    }
  }

  if (exists('daily_wq')) {
    if (unique(daily_wq$site_id) %in% stream_sites) {
      daily_wq <- daily_wq %>% dplyr::filter(variable == "oxygen")
    }
  }

  fs::dir_create(dir)

  file_name <- file.path(file.path(dir, paste0(basename(path), ".parquet")))

  if (exists('daily_wq')) {
    arrow::write_parquet(x = daily_wq, sink = file_name)
    if(fs::file_exists(file_name)){
      #fs::file_delete(path)
    }
  } else {
    # create an empty df to return
    empty <- data.frame(site_id = NA,
                        time = NA,
                        variable = NA,
                        observation = NA)  %>%
      mutate(site_id = as.character(site_id),
             time = as.Date(time),
             variable = as.character(variable),
             observation = as.numeric(observation)) %>%
      dplyr::filter(rowSums(is.na(.)) != ncol(.)) # remove the empty row
    arrow::write_parquet(x = empty, sink = file_name)
    if(fs::file_exists(file_name)){
      #fs::file_delete(path)
    }
  }
}

read.avro.tsd <- function(path, thermistor_depths, dir, delete_files) {
  message(paste0('reading file ', path))

  tsd_avro <- py_to_r(read_avro_file(path)) |>
    dplyr::filter(termName %in% tsd_vars) %>%
    # for streams want to omit the downstream measurement (102) and retain upstream (101)
    # rivers and lakes horizontal index is 103
    dplyr::filter(horizontalIndex %in% c('101','111', '103'),
                  temporalIndex == "030") %>% # take the 30-minutely data only
    dplyr::select(siteName, termName, startDate,
                  doubleValue, intValue, verticalIndex) %>%
    collect() |>
    # combine the value fields to one
    dplyr::mutate(Value = ifelse(is.na(doubleValue),
                                 intValue, doubleValue)) %>%
    dplyr::select(any_of(columns_keep))


  tsd_tibble <- tsd_avro %>%
    as.data.frame() %>%
    suppressWarnings() %>%
    inner_join(., thermistor_depths, by = c('siteName', 'verticalIndex')) %>%
    # only want temperatures in the surface (< 1 m) of the water column
    dplyr::filter(thermistorDepth <= 1.0)

  if (nrow(tsd_tibble) >=1) {
    tsd_tibble_wider <- tsd_tibble %>%
      arrange(startDate, termName) %>%
      pivot_wider(names_from = termName, values_from = Value) %>%
      filter_at(vars(ends_with('QF')), any_vars(. != 1)) # checks to see if any of the QF cols have a 1

    # if the filtering has left rows then find the mean
    if (nrow(tsd_tibble_wider) >= 1) {
      daily_tsd <- tsd_tibble_wider  %>%
        mutate(time = as.Date(startDate)) %>%
        # add missing columns (sites with no tsdWaterTempMean), need all otherwise function won't run
        # tsdWaterTempMean = ifelse('tsdWaterTempMean' %in% colnames(tsd_tibble_wider),
        #                           tsdWaterTempMean, NA),
        # tsdWaterTempExpUncert = ifelse('tsdWaterTempExpUncert' %in% colnames(tsd_tibble_wider),
        #                                tsdWaterTempExpUncert, NA)) %>%
        group_by(siteName, time) %>%
        summarize(temperature = mean(tsdWaterTempMean, na.rm = TRUE),
                  .groups = "drop") %>%
        rename(site_id = siteName) %>%
        # get in the same long format as the NEON portal data
        pivot_longer(cols = -c("time", "site_id"),
                     names_to = "variable",
                     values_to = "observation")

    }
  }

  fs::dir_create(dir)
  file_name <- file.path(file.path(dir, paste0(basename(path), ".parquet")))

  if (exists('daily_tsd')) {
    daily_tsd <- daily_tsd |> QC.temp(range = c(-5, 40), spike = 5, by.depth = F)

    arrow::write_parquet(x = daily_tsd, sink = file_name)
    if(fs::file_exists(file_name) & delete_files){
      fs::file_delete(path)
    }
  } else {
    # create an empty df to return
    empty <- data.frame(site_id = NA,
                        time = NA,
                        variable = NA,
                        observation = NA)  %>%
      mutate(site_id = as.character(site_id),
             time = as.Date(time),
             variable = as.character(variable),
             observation = as.numeric(observation)) %>%
      dplyr::filter(rowSums(is.na(.)) != ncol(.)) # remove the empty row
    if(fs::file_exists(file_name) & delete_files){
      fs::file_delete(path)
    }
  }
}


read.avro.tsd.profile <- function(path, thermistor_depths, columns_keep, dir, delete_files) {
  message(paste0('reading file ', path))

  tsd_avro <- py_to_r(read_avro_file(path)) |>
    dplyr::filter(termName %in% tsd_vars) %>%
    # for streams want to omit the downstream measurement (102) and retain upstream (101)
    # rivers and lakes horizontal index is 103
    dplyr::filter(horizontalIndex %in% c('101', '111', '103'),
                  temporalIndex == "030") %>% # take the 30-minutely data only
    dplyr::select(siteName, termName, startDate,
                  doubleValue, intValue, verticalIndex) %>%
    collect() |>
    # combine the value fields to one
    dplyr::mutate(Value = ifelse(is.na(doubleValue),
                                 intValue, doubleValue)) %>%
    dplyr::select(any_of(columns_keep))


  tsd_tibble <- tsd_avro %>%
    as.data.frame() %>%
    suppressWarnings() %>%
    inner_join(., thermistor_depths, by = c('siteName', 'verticalIndex'))

  if (nrow(tsd_tibble) >=1) {
    tsd_tibble_wider <- tsd_tibble %>%
      arrange(startDate, termName) %>%
      pivot_wider(names_from = termName, values_from = Value) %>%
      filter_at(vars(ends_with('QF')), any_vars(. != 1)) # checks to see if any of the QF cols have a 1

    # if the filtering has left rows then find the mean
    if (nrow(tsd_tibble_wider) >= 1) {
      hourly_tsd <- tsd_tibble_wider  %>%
        mutate(time = lubridate::ymd_h(format(startDate, "%Y-%m-%d %H"))) %>%
        group_by(siteName, time, thermistorDepth) %>%
        summarize(temperature = mean(tsdWaterTempMean, na.rm = TRUE),
                  .groups = "drop") %>%
        rename(site_id = siteName,
               depth = thermistorDepth) %>%
        # get in the same long format as the NEON portal data
        pivot_longer(cols = -c("time", "site_id", "depth"),
                     names_to = "variable",
                     values_to = "observation")

    }
  }

  fs::dir_create(dir)
  file_name <- file.path(file.path(dir, paste0(basename(path), ".parquet")))


  if (exists('hourly_tsd')) {
    hourly_tsd <- hourly_tsd |>
      QC.temp(range = c(-5, 40), spike = 5, by.depth = T) |>
      mutate(data_source = 'NEON_pre_release')

    arrow::write_parquet(x = hourly_tsd, sink = file_name)
    if(fs::file_exists(file_name) & delete_files){
      fs::file_delete(path)
    }
  } else {
    # create an empty df to return
    empty <- data.frame(site_id = NA,
                        time = NA,
                        depth = NA,
                        variable = NA,
                        observation = NA)  %>%
      mutate(site_id = as.character(site_id),
             time = as.Date(time),
             depth = as.numeric(depth),
             variable = as.character(variable),
             observation = as.numeric(observation)) %>%
      dplyr::filter(rowSums(is.na(.)) != ncol(.)) # remove the empty row
    arrow::write_parquet(x = empty, sink = file_name)
    if(fs::file_exists(file_name) & delete_files){
      fs::file_delete(path)
    }
  }
}


read.avro.prt <- function(path, columns_keep, dir) {
  message(paste0('reading file ', path))

  df <- py_to_r(read_avro_file(path))

  if(nrow(df) > 0){

  prt_avro <- df |>
    dplyr::filter(termName %in% prt_vars) %>%
    # for streams want to omit the downstream measurement (102) and retain upstream (101)
    # rivers and lakes horizontal index is 103
    dplyr::filter(horizontalIndex %in% c('101', '111', '103'),
                  temporalIndex == "030") %>% # take the 30-minutely data only
    dplyr::select(siteName, termName, startDate,
                  doubleValue, intValue) %>%
    collect() |>
    # combine the value fields to one
    dplyr::mutate(Value = ifelse(is.na(doubleValue),
                                 intValue, doubleValue)) %>%
    dplyr::select(any_of(columns_keep))

  }else{
    prt_avro <- data.frame()
  }


  prt_tibble <- prt_avro %>%
    as.data.frame() %>%
    suppressWarnings()

  if (nrow(prt_tibble) >=1) {
    prt_tibble_wider <- prt_tibble %>%
      arrange(startDate, termName) %>%
      pivot_wider(names_from = termName, values_from = Value) %>%
      filter_at(vars(ends_with('QF')), any_vars(. != 1)) # checks to see if any of the QF cols have a 1

    # if the filtering has left rows then find the mean
    if (nrow(prt_tibble_wider) >= 1) {
      daily_prt <- prt_tibble_wider  %>%
        mutate(time = as.Date(startDate)) %>%
        group_by(siteName, time) %>%
        summarize(temperature = mean(surfWaterTempMean, na.rm = TRUE),
                  .groups = "drop") %>%
        rename(site_id = siteName) %>%
        rename(observation = temperature) |>
        mutate(variable = "temperature")

    }
  }

  fs::dir_create(dir)
  file_name <- file.path(file.path(dir, paste0(basename(path), ".parquet")))


  if (exists('daily_prt')) {

    daily_prt <- daily_prt |> QC.temp(range = c(-5, 40), spike = 5, by.depth = F)

    arrow::write_parquet(x = daily_prt, sink = file_name)
    if(fs::file_exists(file_name)){
      #fs::file_delete(path)
    }
  } else {
    # create an empty df to return
    empty <- data.frame(site_id = NA,
                        time = NA,
                        variable = NA,
                        observation = NA)  %>%
      mutate(site_id = as.character(site_id),
             time = as.Date(time),
             variable = as.character(variable),
             observation = as.numeric(observation)) %>%
      dplyr::filter(rowSums(is.na(.)) != ncol(.)) # remove the empty row
    arrow::write_parquet(x = empty, sink = file_name)
    if(fs::file_exists(file_name)){
      #fs::file_delete(path)
    }
  }
}
