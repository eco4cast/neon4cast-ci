print(paste0("Running Creating 30-minute Terrestrial Historical Means Forecasts at ", Sys.time()))

library(ncdf4)
library(tidyverse)
library(lubridate)
library(modelr)

set.seed(42)

ne = 200 ## number of ensemble members

team_list <- list(list(individualName = list(givenName = "Mike",  surName ="Dietze"),
                       id = "https://orcid.org/0000-0002-2324-2518"),
                  list(individualName = list(givenName = "Quinn", surName = "Thomas"),
                       id = "https://orcid.org/0000-0003-1282-7825")
)

team_name <- "climatology"
forecast_project_id <- "efi_hist"

download.file("https://data.ecoforecast.org/neon4cast-targets/terrestrial_30min/terrestrial_30min-targets.csv.gz",
              "terrestrial_30min-targets.csv.gz")

terrestrial_targets <- read_csv("terrestrial_30min-targets.csv.gz", guess_max = 100000, show_col_types = FALSE)

site_names <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |>
  dplyr::filter(terrestrial == 1) |>
  dplyr::pull(field_site_id)

start_forecast <- as.POSIXct(Sys.Date(),tz="UTC")
fx_datetime <- seq(start_forecast, start_forecast + days(10), by = "30 min")

reference_datetime <- strftime(lubridate::as_datetime(start_forecast),format= "%Y-%m-%d %H:%M:%S")

nee_fx = le_fx = array(NA, dim=c(length(fx_datetime),length(site_names),ne)) ## dimensions: datetime, space, ensemble

## Forecast by site
for(s in 1:length(site_names)){
  print(site_names[s])

  site_data_var <- terrestrial_targets %>%
    filter(site_id == site_names[s]) |>
    arrange(variable, datetime)

  max_datetime <- max(site_data_var$datetime) + days(1)


  min_datetime <- min(which(!is.na(site_data_var$observation)))
  # This is key here - I added 16 days on the end of the data for the forecast period

  full_datetime1 <- tibble(datetime = seq(site_data_var$datetime[min_datetime], max(site_data_var$datetime), by = "30 min"),
                           variable = "nee")

  full_datetime2 = tibble(datetime = seq(site_data_var$datetime[min_datetime], max(site_data_var$datetime), by = "30 min"),
                          variable = "le")

  full_datetime <- bind_rows(full_datetime1, full_datetime2)

  site_data_var <- left_join(full_datetime, site_data_var, by = c("datetime", "variable")) %>%
    filter(lubridate::month(datetime) == lubridate::month(max_datetime))  ## grab all historical data for this month

  non_na <- site_data_var |>
    na.omit()
  ############ NEE  #############

  if(nrow(non_na) > 0){

    #Full datetime series with gaps
    y_wgaps <- site_data_var$observation[which(site_data_var$variable == "nee")]
    datetime <- c(site_data_var$datetime[which(site_data_var$variable == "nee")])
    #Remove gaps
    t_nogaps <- datetime[!is.na(y_wgaps)]
    y_nogaps <- y_wgaps[!is.na(y_wgaps)]

    ## check that we can calculate mean diurnal cycle
    htod = hour(t_nogaps) + minute(t_nogaps)/60  ## historical datetime of day
    nee_diurnal = tapply(y_nogaps,htod,mean)
    n_diurnal = tapply(y_nogaps,htod,length)
    #  plot(nee_diurnal,main=site_names[s])

    ## forecast
    ftod = hour(fx_datetime) + minute(fx_datetime)/60
    for(i in 1:48){
      #print(i)
      tod = (i-1)/2
      window = 0.25
      hindex = which(abs(htod - tod) < window) ## indices in historical data to resample
      while(length(hindex) < 10){
        window = window + 0.5
        hindex = which(abs(htod - tod) < window) ## if no data at that datetime, look at adjacent datetimes
        #print(window)
      }
      findex = which(ftod == tod) ## columns in fx to fill in
      for(j in findex){
        nee_fx[j,s,] = y_nogaps[sample(hindex,ne,replace=TRUE)]
      }
    }

    ############ Latent heat ############

    #Full datetime series with gaps
    y_wgaps <- site_data_var$observation[which(site_data_var$variable == "le")]
    datetime <- c(site_data_var$datetime[which(site_data_var$variable == "le")])
    #Remove gaps
    t_nogaps <- datetime[!is.na(y_wgaps)]
    y_nogaps <- y_wgaps[!is.na(y_wgaps)]

    htod = hour(t_nogaps) + minute(t_nogaps)/60  ## historical datetime of day
    for(i in 1:48){
      tod = (i-1)/2
      window = 0.25
      hindex = which(abs(htod - tod) < window) ## indices in historical data to resample
      while(length(hindex) < 10){
        window = window + 0.5
        hindex = which(abs(htod - tod) < window) ## if no data at that datetime, look at adjacent datetimes
        print(window)
      }
      findex = which(ftod == tod) ## columns in fx to fill in
      for(j in findex){
        le_fx[j,s,] = y_nogaps[sample(hindex,ne,replace=TRUE)]
      }
    }
  }
} ## end loop over sites

## Set dimensions
datetimedim <- ncdim_def("datetime",   ## dimension name
                         units = paste('seconds since',fx_datetime[1]),
                         ## size of datetimestep, with units and start date
                         vals = as.numeric(fx_datetime - fx_datetime[1]),
                         ## sequence of values that defines the dimension.
                         longname = 'datetime')
## descriptive name

sitedim <- ncdim_def("site",
                     units="",
                     vals=1:length(site_names),
                     longname = "NEON siteID")

ensdim <- ncdim_def("parameter",
                    units = "",
                    vals = seq_len(ne),
                    longname = 'ensemble member')

nchardim <- ncdim_def("nchar",units="",1:4,create_dimvar=FALSE)

## quick check that units are valid
udunits2::ud.is.parseable(datetimedim$units)
udunits2::ud.is.parseable(sitedim$units)
udunits2::ud.is.parseable(ensdim$units)

fillvalue <- 1e32  ## missing data value

#Define variables
def_list <- list()
def_list[[1]] <- ncvar_def(name =  "nee",
                           units = "umol CO2 m-2 s-1",
                           dim = list(datetimedim, sitedim, ensdim),
                           missval = fillvalue,
                           longname = 'net ecosystem exchange of CO2',
                           prec="double")
def_list[[2]] <- ncvar_def(name =  "le",
                           units = "W m-2",
                           dim = list(datetimedim, sitedim, ensdim),
                           missval = fillvalue,
                           longname = 'latent heat flux',
                           prec="double")
def_list[[3]] <- ncvar_def(name = "site_id",
                           units="",
                           dim = list(nchardim,sitedim),
                           longname = "NEON site codes",
                           prec="char")

ncfname <- paste0("terrestrial_30min-",lubridate::as_date(reference_datetime),"-",team_name,".nc")
ncout <- nc_create(ncfname,def_list,force_v4=T)
ncvar_put(ncout,def_list[[1]] , nee_fx)
ncvar_put(ncout,def_list[[2]] , le_fx)
ncvar_put(ncout,def_list[[3]] , site_names)

## Global attributes (metadata)
curr_datetime <- with_tz(Sys.time(), "UTC")
ncatt_put(ncout,0,"reference_datetime", reference_datetime,
          prec =  "text")
ncatt_put(ncout,0,"model_id",team_name, prec =  "text")


nc_close(ncout)   ## make sure to close the file

# Generate metadata
forecast_issue_datetime <- as_date(curr_datetime)
forecast_iteration_id <- start_forecast
forecast_model_id <- team_name

#source("generate_metadata.R")
#meta_data_filename <- generate_metadata(forecast_file =  ncfname,
#                                        metadata_yaml = "metadata_30min.yml",
#                                        forecast_issue_datetime = as_date(with_tz(Sys.time(), "UTC")),
#                                        forecast_iteration_id = start_forecast,
#                                        forecast_file_name_base = strsplit(ncfname,".",fixed=TRUE)[[1]][1],
#                                        start_time = fx_time[1],
#                                        reference_datetime = last(fx_datetime))

neon4cast::submit(forecast_file = ncfname,
                  #metadata = meta_data_filename,
                  ask = FALSE)

unlink(ncfname)
#unlink(meta_data_filename)



