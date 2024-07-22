library(dplyr)
library(duckdbfs)
library(minioclient)
library(bench)

mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

# Sync local scores, fastest way to access all the bytes.
bench::bench_time({ # 13.7s

  mc_mirror("osn/bio230014-bucket01/challenges/scores/parquet/project_id=neon4cast/",
          "project_id=neon4cast")
})

# Merely write out locally with new partition via duckdb, fast!
# Sync bytes in bulk again, faster.
fs::dir_create("bundled-parquet")
bench::bench_time({ # 34.38s

  open_dataset("project_id=neon4cast/**") |>
  select(-date) |> # (date is a short version of datetime from partitioning, drop it)
  write_dataset("bundled-parquet/project_id=neon4cast",
                partitioning = c("duration", 'variable', "model_id"))

  mc_mirror("bundled-parquet/",
            "osn/bio230014-bucket01/challenges/scores/bundled-parquet")
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
