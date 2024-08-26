
# remotes::install_github("cboettig/duckdbfs", upgrade=TRUE)
#install.packages(c("bench", "minioclient"))

library(dplyr)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)


mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))


# Sync local scores, fastest way to access all the bytes.
fs::dir_create("forecasts")
fs::dir_create("new-forecasts/bundled-parquet")
bench::bench_time({ # 11.4 min from scratch, 114 GB
  # mirror everything(!) crazy
  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/parquet/", "forecasts/parquet/", overwrite = TRUE)
  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/bundled-parquet/", "forecasts/bundled-parquet/", overwrite = TRUE)

#  mc_mirror("efi/osn-backup/challenges/forecasts/parquet/", "forecasts/parquet/", overwrite = TRUE, remove = TRUE)

})


##OOOF, still fragile!
durations <- mc_ls("forecasts/parquet/project_id=neon4cast/")
con = duckdbfs::cached_connection(tempfile())

by <- join_by(datetime, site_id, prediction, parameter, family,
              reference_datetime, pub_datetime, duration, model_id,
              project_id, variable)
bench::bench_time({ # 18m w/ union, ~ 50 GB used at times
  for (dur in durations) {
    variables <- mc_ls(glue("forecasts/parquet/project_id=neon4cast/{dur}"))
    for (var in variables) {
      models <- mc_ls(glue("forecasts/parquet/project_id=neon4cast/{dur}{var}"))
      for (model_id in models) {
        path = glue("./forecasts/parquet/project_id=neon4cast/{dur}{var}{model_id}")
        print(path)
        readr::write_lines(path, "bundled.log", append=TRUE)
        if(length(fs::dir_ls(path)) > 0) {
          new <- open_dataset(path, conn = con) |> select(-any_of(c("date", "reference_date", "...1")))  # (date is a short version of datetime from partitioning, drop it)

          bundles <- glue("forecasts/bundled-parquet/project_id=neon4cast/{dur}{var}{model_id}")
          if (fs::dir_exists(bundles)) {
             old <- open_dataset(bundles, conn = con) |>
                    select(-any_of(c("date", "reference_date", "...1"))) |>
                    anti_join(new, by = by) # old not duplicated in new
             new <- union_all(old, new)

             # anti_join |> union_all() may be more efficient than union()
          }
          new |>
            write_dataset("new-forecasts/bundled-parquet/project_id=neon4cast",
                          partitioning = c("duration", 'variable', "model_id"))

          duckdbfs::close_connection(con); gc()
          con = duckdbfs::cached_connection(tempfile())
        }
      }
    }
  }
})


  # checks that we have no corruption
open_dataset("new-forecasts/bundled-parquet/") |> count()
open_dataset(fs::path("new-forecasts/", "bundled-parquet/")) |>
  distinct(duration, variable, model_id)
open_dataset(fs::path("new-forecasts/", "bundled-parquet/")) |>
  summarise(first_fc = min(reference_datetime), last_fc = max(reference_datetime),
            first_prediction = min(datetime), last_prediction = max(datetime))


## Now, new-bundled overwrites bundled
fs::dir_copy("new-forecasts/bundled-parquet/", "forecasts/bundled-parquet/", overwrite =TRUE)

## More checks
date_range <- open_dataset("forecasts/bundled-parquet/") |>
  summarise(first_fc = min(reference_datetime), last_fc = max(reference_datetime),
            first_prediction = min(datetime), last_prediction = max(datetime))
message(date_range)



## Drop old forecasts so we don't keep rebundling them.  (keep last month for safety?)
all_fc_files <- fs::dir_ls("forecasts/parquet/project_id=neon4cast", type="file", recurse = TRUE)
dates <- all_fc_files |> stringr::str_extract("reference_date=(\\d{4}-\\d{2}-\\d{2})/", 1)  |> as.Date()
drop <- dates < Sys.Date() - lubridate::dmonths(1)
all_fc_files[drop] |> fs::file_delete()



bench::bench_time({ # 12.1m
  mc_mirror("forecasts/bundled-parquet", "osn/bio230014-bucket01/challenges/forecasts/bundled-parquet",
            # remove = TRUE,
            overwrite = TRUE)
#  mc_mirror("forecasts/parquet",  "osn/bio230014-bucket01/challenges/forecasts/parquet", remove = TRUE)
})

## We are done.

# df = duckdbfs::open_dataset("forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=oxygen/")


## online tests

online <- open_dataset("s3://bio230014-bucket01/challenges/forecasts/bundled-parquet",
                       s3_endpoint = "sdsc.osn.xsede.org",
                       anonymous = TRUE)
online |> count()
online |>  count(duration, variable, model_id)


