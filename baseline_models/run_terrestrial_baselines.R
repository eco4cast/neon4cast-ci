#renv::restore()

print(paste0("Running Creating Terrestrial baselines at ", Sys.time()))

generate_null_daily <- TRUE
generate_null_30min <- TRUE

if(generate_null_daily){
  print(paste0("Running daily climatology at ", Sys.time()))
  source("baseline_models/models/terrestrial_daily_climatology.R")
  print(paste0("Completed daily climatology at ", Sys.time()))
  print(paste0("Running daily persistence at ", Sys.time()))
  source("baseline_models/models/terrestrial_daily_persistenceRW.R")
  print(paste0("Completed daily persistence at ", Sys.time()))

}

if(generate_null_30min){
  print(paste0("Running 30 min persistence at ", Sys.time()))
  source("baseline_models/models/terrestrial_30min_climatology.R")
  print(paste0("Completed 30 min Null at ", Sys.time()))
}

#RCurl::url.exists("https://hc-ping.com/bbe3ddc7-4020-4c53-bb13-08580d765e32")
