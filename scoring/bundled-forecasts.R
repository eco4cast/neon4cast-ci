
  # remotes::install_github("cboettig/duckdbfs", upgrade=TRUE); install.packages(c("bench", "minioclient"))

library(dplyr)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)


mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

fs::dir_delete("forecasts/")


fs::dir_create("forecasts/bundled-parquet")
fs::dir_delete("new-forecasts")
fs::dir_create("new-forecasts/bundled-parquet")

# Sync local scores, fastest way to access all the bytes.
bench::bench_time({ # 11.4 min from scratch, 114 GB
  # mirror everything(!) crazy
  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/parquet/", "forecasts/parquet/", overwrite = TRUE, remove = TRUE)
#  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/bundled-parquet/", "forecasts/bundled-parquet/", overwrite = TRUE, remove = TRUE)

})



##OOOF, still fragile!
durations <- mc_ls("forecasts/parquet/project_id=neon4cast/")
con = duckdbfs::cached_connection(tempfile())
bench::bench_time({ # 14 min... 27m w/ distinct
  for (dur in durations) {
    variables <- mc_ls(glue("forecasts/parquet/project_id=neon4cast/{dur}"))
    for (var in variables) {
      models <- mc_ls(glue("forecasts/parquet/project_id=neon4cast/{dur}{var}"))
      for (model_id in models) {
        path = glue("./forecasts/parquet/project_id=neon4cast/{dur}{var}{model_id}")
        print(path)
        readr::write_lines(path, "bundled.log", append=TRUE)
        if(length(fs::dir_ls(path)) > 0) {
          fc <- open_dataset(path, conn = con) |> select(-any_of(c("date", "reference_date", "...1")))  # (date is a short version of datetime from partitioning, drop it)

          bundles <- glue("forecasts/bundled-parquet/project_id=neon4cast/{dur}{var}{model_id}")
          if (fs::dir_exists(bundles)) {
             fc <- open_dataset(bundles, conn = con) |> select(-any_of(c("date", "reference_date", "...1"))) |> union(fc)


          }
          fc |>
            write_dataset("new-forecasts/bundled-parquet/project_id=neon4cast",
                          partitioning = c("duration", 'variable', "model_id"))

          duckdbfs::close_connection(con); gc()
          con = duckdbfs::cached_connection(tempfile())
        }
      }
    }
  }
})


  # check that we have no corruption
n <- open_dataset("new-forecasts/bundled-parquet/") |> count() |> collect()
groups <- open_dataset(fs::path("new-forecasts/", "bundled-parquet/")) |>
  distinct(duration, variable, model_id) |> collect()

## earliest date
open_dataset(fs::path("new-forecasts/", "bundled-parquet/")) |>
  summarise(first = min(reference_datetime),
            first_date = min(datetime)
            ) |> collect()


open_dataset("forecasts/bundled-parquet/**") |> count() |> collect()
open_dataset("forecasts/bundled-parquet/**") |> count(duration, variable, model_id, sort=TRUE) |> collect()



## Now, new-bundled overwrites bundled
fs::dir_delete("forecasts/bundled-parquet/")
fs::dir_copy("new-forecasts/bundled-parquet/", "forecasts/bundled-parquet/", overwrite =TRUE)

fs::dir_copy("forecasts/bundled-parquet/", "testit/", overwrite=TRUE)



# PURGE all but last 2 months from un-bundled. also yr bundles.
#all_fc_files <- fs::dir_ls("forecasts/parquet/project_id=neon4cast", type="file", recurse = TRUE)
#yrs <- all_fc_files |> stringr::str_extract("reference_date=(\\d{4})/", 1)
#all_fc_files[!is.na(yrs)] |> fs::file_delete()

all_fc_files <- fs::dir_ls("forecasts/parquet/project_id=neon4cast", type="file", recurse = TRUE)
dates <- all_fc_files |> stringr::str_extract("reference_date=(\\d{4}-\\d{2}-\\d{2})/", 1)  |> as.Date()
drop <- dates < Sys.Date() - lubridate::dmonths(2)
all_fc_files[drop] |> fs::file_delete()


bench::bench_time({ # 12.1m
  mc_mirror("forecasts/bundled-parquet", "osn/bio230014-bucket01/challenges/forecasts/bundled-parquet", overwrite = TRUE, remove=TRUE)
#  mc_mirror("forecasts/parquet",  "osn/bio230014-bucket01/challenges/forecasts/parquet", remove = TRUE)
})

## We are done.

# df = duckdbfs::open_dataset("forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=oxygen/")


## online tests

online <- open_dataset("s3://bio230014-bucket01/challenges/forecasts/bundled-parquet",
                       s3_endpoint = "sdsc.osn.xsede.org",
                       anonymous = TRUE)
online |> count() |> collect()
online |>  distinct(duration, variable, model_id) |> collect()
online |> summarise(first = min(reference_datetime),  first_date = min(datetime)) |> collect()

local <- open_dataset("forecasts/bundled-parquet")
local |> count() |> collect()
local |>  distinct(duration, variable, model_id) |> collect()

local |>
  summarise(first = min(reference_datetime),
           first_date = min(datetime)
  ) |> collect()

#o_groups <- online |>
#  distinct(duration, variable, model_id)  |> collect()
