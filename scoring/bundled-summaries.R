library(dplyr)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)

mc_alias_set("osn", "sdsc.osn.xsede.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))


fs::dir_create("forecasts/")
fs::dir_create("forecasts/bundled-summaries")

# Sync local scores, fastest way to access all the bytes.
bench::bench_time({ # 17.5 min from scratch, 114 GB

  # mirror everything(!) crazy
  # Could focus on summaries here
  mc_mirror("osn/bio230014-bucket01/challenges/forecasts/summaries/", "forecasts/summaries/", overwrite = TRUE)

})

grouping <- c("model_id", "reference_datetime", "site_id",
              "datetime", "family", "variable", "duration", "project_id")

bench::bench_time({
  bundled_summaries <- open_dataset("./forecasts/bundled-summaries/project_id=neon4cast")
  new_summaries <- open_dataset("./forecasts/summaries/project_id=neon4cast/")
  union_all(bundled_summaries, new_summaries) |>
    group_by(across(any_of(grouping))) |>
    slice_max(pub_datetime) |>
      write_dataset("forecasts/bundled-summaries/project_id=neon4cast",
                    partitioning = c("duration", 'variable', "model_id"))

})


# check that we have no corruption
test <- open_dataset("forecasts/bundled-summaries/") |> count() |> collect()



bench::bench_time({ # 12.1m
  mc_mirror("forecasts/bundled-summaries",
            "osn/bio230014-bucket01/challenges/forecasts/bundled-summaries", overwrite=TRUE)
})

## We are done.

# df = duckdbfs::open_dataset("forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=oxygen/")

