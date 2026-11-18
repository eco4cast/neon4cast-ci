# Launching a new challenge

This repository runs the NEON Ecological Forecasting Challenge, but nothing in
the shared code is specific to NEON. To run another challenge, copy the
repository and change three kinds of things:

- **settings**, all in `challenge_configuration.yaml`
  ([configuration.md](configuration.md));
- **challenge-specific code and text**: one script per target group that
  downloads and cleans the observations, and the dashboard's prose;
- **infrastructure**: buckets, secrets, a catalog repository, and cron times.

The steps below go in that order. Work through them with `DRY_RUN=true` (see
[developing.md](developing.md)) so nothing is written to S3 until you are ready.

## 1. Copy the repository

Fork it, or create a new repository from a copy. Keep `R/`, `inst/`, `tests/`,
`.github/`, `docs/`, `dashboard/`, `Dockerfile` and `DESCRIPTION`. Remove the
NEON-specific pieces you will replace: `targets/*_targets.R` and
`targets/R/` (NEON download helpers), `targets/supporting-data/`,
`neon4cast_field_site_metadata.csv`, `catalog/site_columns.csv`,
`catalog/thumbnail_plots/`, `dashboard/_content/targets/*.md` and
`manuscript/`.

## 2. Describe the sites

Make a CSV with one row per site. It needs a site id, name, latitude and
longitude (any column names; map them in `sites$columns`), plus one column per
target group holding `1` for sites in that group. Name each such column after
the group in lower case, or set the group's `sites_column`. Publish it where
the workflows can read it, for example in the repository, and set
`sites$metadata_url` to its raw URL.

## 3. Write the configuration

Edit `challenge_configuration.yaml`. The keys that make up a challenge:

| Section | Set |
|---|---|
| Identity | `project_id` (short, used in every path and row), `challenge_long_name`, `github_repo`, `challenge_url` (where the dashboard is published) |
| Storage | `endpoint` and the `*_bucket` keys for the main store; `submissions_endpoint` and `submissions_bucket`; `scratch` for scoring; `credentials` if your secrets have other names |
| Sites | `sites$metadata_url`, `sites$columns` |
| Target groups | for each group: `variable`, `duration`, `max_horizon` (days), `targets_file`, `targets_script`, `theme` (file-name prefix participants use), `focal_sites`, `baselines`, and the catalog's `thumbnail_link`/`thumbnail_title` and `group_vars` citations |
| Submissions | `submissions$min_reference_date`, `submissions$exclude_patterns` |
| Drivers | `noaa_forecast_bucket`, `noaa_endpoint`, `noaa_forecasts` (only if you provide NOAA weather forecasts) |
| Metadata | `model_metadata_gsheet`, `target_metadata_gsheet` (step 6) |
| Dashboard | `dashboard` block: title, contact email, registration form, excluded models |
| Catalog | `catalog_config`: catalog repository and folder, titles, thumbnails, citation, bounding box |
| Workflows | `ci`: container image, large runner (or `ubuntu-latest`), healthcheck URLs |

Then check it from the repository root:

```r
pkgload::load_all()
challengeci::config_problems(read_config(validate = FALSE))  # every problem at once
devtools::test()                                             # includes config and docs checks
```

## 4. Create the storage

Create the buckets named in the config. The main store's forecasts, scores,
summaries, targets and metadata buckets must allow **anonymous read**: the
dashboard, catalog and participants read them without credentials. The
submissions bucket must let participants upload. The scratch bucket is private
and holds temporary files while scoring.

## 5. Add the secrets

Add the GitHub repository secrets listed in [operations.md](operations.md#secrets):
keys for each store, `CATALOG_TOKEN` (a token that can push to the catalog
repository), and, if you build your own image, Docker Hub credentials and
`PAT`. If your secrets have different names, either rename them in the
workflows' `env:` blocks or change `credentials` in the config.

## 6. Set up registration and variable metadata

- **Registration form.** Participants register models with a Google Form whose
  responses go to a sheet readable by anyone with the link; set
  `model_metadata_gsheet` to it. The EFI form is shared by several challenges
  and has a question naming the challenge; only rows whose answer equals your
  `project_id` are used. If your form words a question differently, map it in
  `catalog_config$registration_columns` (`?challengeci::registration_columns`).
- **Variable sheet.** One row per variable and duration with columns
  `Variable name` (long name), `"official" targets name` (the variable),
  `Description` and `duration`; set `target_metadata_gsheet`. Different column
  names go in `catalog_config$variable_metadata_columns`.

## 7. Write the targets scripts

For each target group, write `targets/<group>_targets.R` and name it in the
group's `targets_script`. A script downloads and cleans the observations any
way it needs to, and ends the same way as every other:

```r
# <Group> targets from <source>.
#
# Run from the repository root:  Rscript targets/<group>_targets.R
#
# Environment variables
#   OSN_KEY, OSN_SECRET  main store, for the upload
#   DRY_RUN=true         write the targets to a local folder instead

pkgload::load_all(quiet = TRUE)
cfg <- read_config()
minioclient::install_mc()
sites <- read_sites(cfg, "<Group>")

observations <- ...  # site_id, datetime, variable, observation

observations |>
  format_targets(cfg, duration = "P1D") |>  # standard columns and types
  check_targets(cfg, "<Group>") |>          # stops if rows, sites or variables went missing
  write_targets(cfg, "<Group>")             # uploads to the group's targets_file
```

`check_targets()` compares the new targets with the published file and refuses
to upload if they lost more than 5% of the rows, a site or a variable. Put
helpers in `targets/R/`. Set `targets_large_runner: true` for a group that
needs more than a GitHub runner's 16 GB of memory or 6 hours.

## 8. Write the dashboard text

- `dashboard/_content/targets/<Group>.md`: an image and a few paragraphs about
  each group, shown on the "What to forecast" page.
- `dashboard/index.qmd`, `learn-more.qmd` and `use-in-course.qmd`: the home
  page text, reading list and teaching notes are prose about the NEON
  challenge; rewrite them.
- `dashboard/img/`: the banner image.

The rest of the dashboard (plots, tables, links, site map) is built from the
config.

## 9. Set up publishing

- **Catalog.** Create an empty repository for the STAC catalog and set
  `catalog_config$catalog_repo` and `catalog_config$catalog_path`.
- **Dashboard.** Create an empty `gh-pages` branch and turn on GitHub Pages
  for it. `challenge_url` should be its address.
- **Container image.** Keep `ci$docker_image` as the EFI image, or set your own
  name and let `docker.yaml` build and push it (needs the Docker Hub secrets).

## 10. Set the schedules

Cron times cannot come from the config: edit the `cron:` lines in
`.github/workflows/*.yaml` ([operations.md](operations.md#workflows) lists them).
If you change the baselines workflow's weekly cron, change the same string in
its `SCHEDULE` expression and matrix too. If you have no NOAA drivers, delete
`drivers_stage1_2.yaml` and `drivers_stage3.yaml` and leave `noaa_forecasts`
out of the config; the catalog then leaves out its NOAA collection.

## 11. Dry-run every stage

From the repository root, with the secrets set as environment variables (or
none, for the stages that only read public data):

```sh
export DRY_RUN=true DRY_RUN_DIR=/tmp/dry-run
Rscript targets/<group>_targets.R           # one per group
Rscript inst/scripts/baselines.R daily      # and weekly
Rscript inst/scripts/process_submissions.R
Rscript inst/scripts/bundle.R forecasts
Rscript inst/scripts/score.R
Rscript inst/scripts/catalog.R
Rscript inst/scripts/dashboard.R && quarto render dashboard
```

Until the first targets and forecasts exist, the later stages have little to
do; run them again once the first real data are in.

## 12. Turn it on

Push to the default branch, then run each workflow once by hand from the
Actions tab, in pipeline order: target-generation, baseline-forecasts,
process-submissions, bundling, scoring, catalog, dashboard. Once they pass,
the schedules take over. Add healthcheck URLs to `ci$healthchecks` for the
jobs you want to be alerted about.
