library(dplyr)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)


fs::dir_create("forecasts/parquet/")
fs::dir_create("forecasts/summaries/")

mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))

# mirror all!
#mc_mirror("osn/bio230014-bucket01/challenges/forecasts/", "forecasts/", overwrite = TRUE)

# Sync local scores, fastest way to access all the bytes.
bench::bench_time({ # 13.7s

  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/parquet/project_id=neon4cast/",
            "forecasts/parquet/project_id=neon4cast")

  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/summaries/project_id=neon4cast/",
            "forecasts/summaries/project_id=neon4cast")

})

durations <- mc_ls("forecasts/parquet/project_id=neon4cast/")

# Merely write out locally with new partition via duckdb, fast!
# Sync bytes in bulk again, faster.
fs::dir_create("forecasts/bundled-parquet")
fs::dir_create("forecasts/bundled-summaries")

## Summaries are quick and easy:
bench::bench_time({
  open_dataset("./forecasts/summaries/") |>
    write_dataset("forecasts/bundled-summaries/project_id=neon4cast",
                  partitioning = c("duration", 'variable', "model_id"))
})


##OOOF
bench::bench_time({
  for (dur in durations) {
      variables <- mc_ls(glue("forecasts/parquet/project_id=neon4cast/{dur}"))
      for (var in variables) {
        path = glue("./forecasts/parquet/project_id=neon4cast/{dur}{var}")
        print(path)
        open_dataset(path) |>
        select(-any_of("date")) |> # (date is a short version of datetime from partitioning, drop it)
        write_dataset("forecasts/bundled-parquet/project_id=neon4cast",
                      partitioning = c("duration", 'variable', "model_id"))
      }
  }
})



bench::bench_time({
  mc_mirror("forecasts",
            "osn/bio230014-bucket01/challenges/forecasts", overwrite=TRUE)
})

## We are done.

# df = duckdbfs::open_dataset("forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=oxygen/")

