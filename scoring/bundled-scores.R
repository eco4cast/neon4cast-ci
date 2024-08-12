library(dplyr)
library(duckdbfs)
library(minioclient)
library(bench)

mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

local <- fs::dir_create("osn-scores")

# Sync local scores, fastest way to access all the bytes.
# not strictly necessary
bench::bench_time({ # 13.7s

  mc_mirror("osn/bio230014-bucket01/challenges/scores",
            local)
})

# Merely write out locally with new partition via duckdb, fast!
# Sync bytes in bulk again, faster.
fs::dir_create(fs::path(local, "bundled-parquet"))

bench::bench_time({ # 34.38s
  open_dataset(fs::path(local, "parquet", "/**")) |>
  select(-date) |> # (date is a short version of datetime from partitioning, drop it)
  write_dataset("bundled-parquet",
                partitioning = c("project_id", "duration", 'variable', "model_id"))
})



# check that we have no corruption
n_bundled <- open_dataset(fs::path(local, "bundled-parquet/")) |> count() |> collect()
n_groups <- open_dataset(fs::path(local, "bundled-parquet/")) |>
  distinct(duration, variable, model_id) |> count() |> collect()
open_dataset(fs::path(local, "bundled-parquet/")) |>
  summarise(first = min(reference_datetime),
            date = min(datetime)) |> collect()


# PURGE
all_files <- fs::dir_ls(fs::path(local, "/parquet/project_id=neon4cast"), type="file", recurse = TRUE)
dates <- all_files |> stringr::str_extract("date=(\\d{4}-\\d{2}-\\d{2})/", 1)  |> as.Date()
stopifnot(all(!is.na(dates)))
drop <- dates < Sys.Date() - lubridate::dmonths(2)
all_files[drop] |> fs::file_delete()


bench::bench_time({
  mc_mirror(local,
            "osn/bio230014-bucket01/challenges/scores", overwrite = TRUE, remove=TRUE)
})

## We are done.


## direct write, much slower...
#bench::bench_time({
#  scores |> write_dataset("s3://bio230014-bucket01/challenges/scores/bundled-parquet/project_id=neon4cast",
#                          partitioning = c("duration", 'variable', "model_id"),
#                          s3_endpoint = "sdsc.osn.xsede.org",
#                          s3_access_key_id = Sys.getenv("OSN_KEY"),
#                          s3_secret_access_key=Sys.getenv("OSN_SECRET"))
#})



# TESTING: single URL is fast
url <- paste0("https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/",
              "scores/bundled-parquet/project_id=neon4cast/duration=P1D/",
              "variable=temperature/model_id=climatology/data_0.parquet")
bench::bench_time({ # 1.69s
  duckdbfs::open_dataset(url) |> collect()
})


## Testing, inventory computation is fast
s3 <- paste0("s3://anonymous@bio230014-bucket01/challenges/scores/bundled-parquet/",
             "project_id=neon4cast/duration=P1D/variable=temperature")

bench::bench_time({ # 3.43s
    open_dataset(s3, s3_endpoint = "sdsc.osn.xsede.org") |>
    count(model_id, datetime) |>
    collect()
})
