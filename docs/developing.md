# Developing the CI

Shared code lives in a small R package, `challengeci`, inside this repository:

```
DESCRIPTION, NAMESPACE   package metadata (NAMESPACE is generated, do not edit)
R/                       package functions, documented with roxygen comments
man/                     help pages (generated, do not edit)
tests/testthat/          unit tests and small fixtures
inst/scripts/            entry points the workflows run (below)
inst/extdata/            data shipped with the package (catalog column descriptions)
challenge_configuration.yaml   every challenge-specific setting (docs/configuration.md)
targets/                 challenge-specific targets scripts and their helpers (targets/R/)
dashboard/               the Quarto dashboard; data staged by inst/scripts/dashboard.R
catalog/                 challenge-specific catalog files (site column descriptions, images)
.github/                 workflows and the composite actions they share (docs/operations.md)
docs/                    configuration, development, operations and launch guides
```

Each workflow step runs a short entry script from the repository root. The
script loads the package, reads the config and calls the package functions
that do the work:

| Script | Workflow | Function | Does |
|---|---|---|---|
| `inst/scripts/process_submissions.R` | submissions | `run_submissions()` | validate new submissions, write parquet + summaries |
| `inst/scripts/bundle.R [forecasts\|summaries]` | bundling | `run_bundling()` | append new parquet to each model's bundle, archive it |
| `inst/scripts/score.R [since]` | scoring | `run_scoring()` | score forecasts that have new observations |
| `inst/scripts/baselines.R <schedule> [group]` | baseline-forecasts | `run_baselines()` | run each group's baseline models and submit them |
| `inst/scripts/drivers_stage1_2.R` | drivers-stage1-2 | `download_gefs()`, `update_stage2()` | download GEFS stage 1, build hourly stage 2 |
| `inst/scripts/drivers_stage3.R [rebuild]` | drivers-stage3 | `download_gefs()`, `update_stage3()` | download GEFS pseudo, extend hourly stage 3 |
| `inst/scripts/catalog.R [part ...]` | catalog | `run_catalog()` | build the STAC catalog JSON and upload model metadata |
| `inst/scripts/dashboard.R` | dashboard | `stage_dashboard_data()` | stage the data the dashboard plots; then `quarto render dashboard` |
| `targets/<group>_targets.R` | target-generation | `format_targets()`, `check_targets()`, `write_targets()` | download and clean one group's observations (challenge-specific) |

Each one documents its arguments and environment variables in its header.
All that write to S3 take `DRY_RUN=true` (see below).

The targets scripts stay plain scripts because downloading and cleaning
observations is specific to each challenge's data source. Their helpers
(NEON file lists, avro readers, taxonomy) are in `targets/R/`. A new challenge
writes its own `targets/<group>_targets.R` that ends with
`format_targets() |> check_targets() |> write_targets()`, so its targets are
checked and uploaded the same way.

The dashboard pages (`dashboard/*.qmd`) only read the small files
`stage_dashboard_data()` writes to `dashboard/data/`, so every S3 query is in
the package and tested. Pages start with `source("R/setup.R")`, which loads
the package and config; plots and the one-tab-per-variable helper are in
`dashboard/R/plot-utils.R`. To preview locally:
`Rscript inst/scripts/dashboard.R && quarto preview dashboard`.

The catalog is plain STAC JSON written by `R/catalog.R` and `R/stac.R`.
Generic column descriptions are in `inst/extdata/catalog_columns.csv`; the
challenge's site column descriptions are in the file named by
`catalog_config$site_columns_file`. A dry run
(`DRY_RUN=true Rscript inst/scripts/catalog.R`) writes the catalog under
`DRY_RUN_DIR/catalog`, so it can be diffed against the catalog repository.

## Using the package from a script

The workflows check out the repository and run scripts from its root. Load the
package from the checkout instead of installing it, so a script always runs the
code from the same commit:

```r
pkgload::load_all(quiet = TRUE)   # makes challengeci's functions available

cfg <- read_config()              # challenge_configuration.yaml, or $CHALLENGE_CONFIG
s3_connect(cfg, "main")           # mc alias "main" + duckdb secret, from OSN_KEY/OSN_SECRET
bundled <- bucket_uri(cfg, "forecasts_bucket", "bundled-parquet")
sites <- read_sites(cfg, "Aquatics")
```

`pkgload` comes with `devtools`, which the Docker image already includes.

## The edit, document, test loop

From the repository root, in R:

```r
devtools::load_all()   # reload after editing R/
devtools::document()   # after changing roxygen comments (#') or adding a function
devtools::test()       # run tests/testthat
devtools::check()      # full R CMD check before opening a PR
lintr::lint_dir("R")   # style (rules in .lintr)
```

Commit the regenerated `NAMESPACE` and `man/` files with your change. The
`package-tests` workflow runs the tests and `R CMD check` on every pull request
that touches the package.

## Conventions

- **The config is the source of truth.** Never write a bucket, endpoint,
  project id, variable, horizon or site list into code. Read it from `cfg`. If
  a setting does not exist yet, add a new key to the config, document it in
  [configuration.md](configuration.md), and give it a default in
  `config_defaults()` when a sensible one exists.
- **Functions take `cfg` and data as arguments.** No globals.
- **Tidyverse style:** native pipe `|>`, snake_case, `pkg::fun()` calls inside
  `R/` (no `library()`), and explicit `by =` in joins. Use `purrr::map()`/`walk()`
  rather than loops that grow objects.
- **Fail loudly.** Use `cli::cli_abort()` with a message that says what to fix.
  When one item fails in a loop over many (submissions, groups, models),
  record the failure, carry on, and exit non-zero at the end.
- **Do as much as possible in duckdb.** Filter and aggregate before `collect()`.
- **Writes respect dry runs.** Send every change to S3 through
  `upload_file()`, `upload_dir()`, `move_remote()` or `remove_remote()`, or
  write to a path from `output_uri()`. Then `DRY_RUN=true` sends it to a
  local folder or only logs it.
- **Upload with mc.** duckdb (>= 2.0) cannot finish multipart uploads to OSN,
  and arrow's uploads fail there too. Write locally, then `upload_file()` or
  `upload_dir()`.
- **Pass `recursive = FALSE` when giving `duckdbfs::open_dataset()` a file.**
  Given a single remote path, it otherwise treats it as a folder and adds
  `/**`.
- **Comments explain why.** Every exported function has roxygen docs with
  `@param` and `@return`.

## Testing without touching production

- `DRY_RUN=true` sends all package writes to `DRY_RUN_DIR` (default: a
  temporary folder) and only logs moves and deletes. Reads still use the real
  buckets, anonymously if no credentials are set, so the bundling and scoring
  stages can be dry-run with no secrets at all. Submissions need the
  submissions key, because only it can list that bucket.
- `CHALLENGE_CONFIG=path/to/test.yaml` points everything at a different
  config, for example one whose buckets are a scratch prefix.
- Public buckets can be read without credentials:
  `s3_connect(cfg, "main", anonymous = TRUE)`.
