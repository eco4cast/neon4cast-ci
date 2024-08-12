library(dplyr)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)

mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

# Sync local scores, fastest way to access all the bytes.
bench::bench_time({ # 17.5 min from scratch, 114 GB
  # mirror everything(!) crazy
  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/parquet/", "forecasts/parquet/", overwrite = TRUE)

})




fs::dir_create("forecasts/bundled-parquet")
## archive


##OOOF, still fragile!
durations <- mc_ls("forecasts/parquet/project_id=neon4cast/")
bench::bench_time({ # 14 min
  for (dur in durations) {
    variables <- mc_ls(glue("forecasts/parquet/project_id=neon4cast/{dur}"))
    for (var in variables) {
      path = glue("./forecasts/parquet/project_id=neon4cast/{dur}{var}")
      print(path)
      open_dataset(path) |>
        select(-any_of("date")) |> # (date is a short version of datetime from partitioning, drop it)
        write_dataset("forecasts/bundled-parquet/project_id=neon4cast",
                      partitioning = c("duration", 'variable', "model_id"))

      gc()
    }
    gc()
  }
})

duckdbfs::close_connection()

# check that we have no corruption
#n_raw <- open_dataset("forecasts/parquet/") |> count() |> collect()
n <- open_dataset("forecasts/bundled-parquet/") |> count() |> collect()
n_groups <- open_dataset(fs::path("forecasts", "bundled-parquet/")) |>
  distinct(duration, variable, model_id) |> count() |> collect()

## earliest date
open_dataset(fs::path("forecasts", "bundled-parquet/")) |>
  summarise(first = min(reference_datetime)) |> collect()

duckdbfs::close_connection()



# PURGE all but last 2 months from un-bundled. also yr bundles.
#all_fc_files <- fs::dir_ls("forecasts/parquet/project_id=neon4cast", type="file", recurse = TRUE)
#yrs <- all_fc_files |> stringr::str_extract("reference_date=(\\d{4})/", 1)
#all_fc_files[!is.na(yrs)] |> fs::file_delete()

all_fc_files <- fs::dir_ls("forecasts/parquet/project_id=neon4cast", type="file", recurse = TRUE)
dates <- all_fc_files |> stringr::str_extract("reference_date=(\\d{4}-\\d{2}-\\d{2})/", 1)  |> as.Date()
drop <- dates < Sys.Date() - lubridate::dmonths(2)
all_fc_files[drop] |> fs::file_delete()



bench::bench_time({ # 12.1m
  mc_mirror("forecasts",
            "osn/bio230014-bucket01/challenges/forecasts", overwrite=TRUE, remove = TRUE)
})

## We are done.

# df = duckdbfs::open_dataset("forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=oxygen/")

