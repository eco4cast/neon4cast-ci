# Challenge configuration reference

Every challenge-specific setting lives in
[`challenge_configuration.yaml`](../challenge_configuration.yaml) at the repository
root. Code reads it with `read_config()` from the `challengeci` package (see
[developing.md](developing.md)). To use a different file, for example in tests or
when trying out a new challenge, set the environment variable `CHALLENGE_CONFIG`
to its path. [launching-a-new-challenge.md](launching-a-new-challenge.md)
walks through setting up a new challenge from scratch.

`read_config()` checks the file with `validate_config()`:

- **Errors** stop the run: a required key is missing, a duration is malformed, or
  `variable`/`duration`/`max_horizon` have different lengths.
- **Warnings** flag copies of a setting that disagree, such as a `group_vars`
  entry with a different `max_horizon` from its group.

Run `challengeci::config_problems(challengeci::read_config(validate = FALSE))` to
list every problem at once.

> **Keep changes additive.** Tools outside this repository read this file
> straight from GitHub `main`. For example, `neon4cast::forecast_output_validator()`
> and `neon4cast::submit()` read it. Add new keys freely, but do not rename or
> change the meaning of existing ones without checking those tools first.

Column key: **Req.** means `validate_config()` requires the key. **Used by** names
the stages that read it today. A stage is one of the workflows (`submissions`,
`bundling`, `scoring`, `targets`, `baselines`, `catalog`, `dashboard`, `drivers`),
`workflows` for the workflow files themselves (read with `yq`), or `pkg` for the
`challengeci` package in general.

## Identity

| Key | Req. | Description | Used by |
|---|---|---|---|
| `project_id` | yes | Short challenge id, e.g. `neon4cast`. Forms the `project_id=` partition in buckets and appears in every forecast and score row. | submissions, scoring, catalog, dashboard |
| `challenge_long_name` | | Full name, shown in the catalog and on the dashboard (and its title, unless `dashboard$title` is set). | catalog, dashboard |
| `github_repo` | | `owner/repo` of this repository. The dashboard links to it; the catalog uses it when `catalog_config$catalog_repo` is not set. | catalog, dashboard |
| `challenge_url` | | Public dashboard URL. Quarto builds the sitemap from it; the catalog links to it for models registered without a code link. | catalog, dashboard |
| `example_model_id` | | Text that marks test models, default `example`. Registered models whose id contains it get no catalog entry or metadata record. | catalog |

## Storage

Bucket keys hold a bucket name followed by an optional prefix, such as
`bio230014-bucket01/challenges/forecasts`. In package code, build paths inside a
bucket with `bucket_uri(cfg, "<key>", ...)`. Do not paste paths together by hand.

There are four stores. A store is an S3 endpoint plus its credentials:

| Store | Endpoint key | Holds |
|---|---|---|
| `main` | `endpoint` | forecasts, scores, summaries, targets, archive, metadata |
| `submissions` | `submissions_endpoint` | raw participant uploads |
| `noaa` | `noaa_endpoint` | NOAA driver forecasts (uses the `main` credentials) |
| `scratch` | `scratch$endpoint` | temporary files written while scoring |

| Key | Req. | Description | Used by |
|---|---|---|---|
| `endpoint` | yes | Host of the `main` store, without `https://`. | all |
| `submissions_endpoint` | | Host of the `submissions` store. | submissions |
| `noaa_endpoint` | | Host of the `noaa` store. Defaults to `endpoint`. | catalog, drivers |
| `forecasts_bucket` | yes | Forecasts: `parquet/` (new), `bundled-parquet/`, `archive-parquet/` (bundled), `raw/` (every submitted file). | submissions, bundling, scoring, catalog, dashboard |
| `summaries_bucket` | | Forecast summaries: `summaries/` (new) and `bundled-summaries/`. Defaults to `forecasts_bucket`. | submissions, bundling, catalog, dashboard |
| `scores_bucket` | yes | Scores: `parquet/` and `bundled-parquet/`. | scoring, catalog, dashboard |
| `targets_bucket` | yes | Target (observation) files. | targets, scoring, dashboard |
| `submissions_bucket` | | Bucket participants upload to. | submissions |
| `archive_bucket` | | Raw copies of every processed submission. | submissions |
| `noaa_forecast_bucket` | | NOAA GEFS driver forecasts, one folder per stage. | drivers, catalog |
| `model_metadata_bucket` | | One JSON file per registered model, named `<model_id>.json`. Challenges may share it, so a model registered without answers never replaces an existing file. | catalog |
| `supporting_data_bucket` | | Observations that are not targets, such as hourly lake temperature profiles. | targets |
| `processed_submissions` | | CSV recording which submissions have been processed. It stands in for deleting them; see the comment in the config file. | submissions |
| `scratch$endpoint` | | Host of the `scratch` store. | scoring |
| `scratch$bucket` | | Scratch bucket and prefix. The `score_me`, `forecasts`, `scores` and `targets` folders under it are deleted at the start of each scoring run. | scoring |
| `credentials$<store>$key_env` | | **Name** of the environment variable holding the store's access key. Defaults: `OSN_KEY`, `AWS_ACCESS_KEY_SUBMISSIONS`, `EFI_NRP_KEY`. | pkg |
| `credentials$<store>$secret_env` | | **Name** of the environment variable holding the store's secret key. Defaults: `OSN_SECRET`, `AWS_SECRET_ACCESS_KEY_SUBMISSIONS`, `EFI_NRP_SECRET`. | pkg |

Never put the keys themselves in the config. They come from GitHub repository
secrets, which the workflows expose as the environment variables named above.

## Submissions

| Key | Req. | Description | Used by |
|---|---|---|---|
| `submissions$exclude_patterns` | | Regular expressions. Keys in the submissions bucket that match any of them are never downloaded or recorded. NEON uses `2023` and `usgsrc4cast`. | submissions |
| `submissions$min_reference_date` | | Files whose name dates them before this (`YYYY-MM-DD`) are archived without being processed. | submissions |

A submission file must be named `<theme>-<YYYY-MM-DD>-<model_id>.csv` or `.csv.gz`.
Files that are not are archived to `raw/` and recorded without being processed.
`?challengeci::validate_forecast` lists the format rules.

## Sites

| Key | Req. | Description | Used by |
|---|---|---|---|
| `sites$metadata_url` | | URL or path of the site metadata CSV. Falls back to `catalog_config$site_metadata_url`; one of the two is required. | pkg |
| `sites$columns` | | Maps standard column names (`site_id`, `site_name`, `latitude`, `longitude`) to the file's column names. Defaults to the NEON file's names. | pkg |

`read_sites(cfg, group)` returns only the sites in a target group. Group
membership is a column of 1s in the metadata file. By default that column is
named after the group in lower case; set the group's `sites_column` to use a
different column.

## Target groups

`target_groups` maps each group name, e.g. `Aquatics`, to its settings. In package
code, use `target_variables(cfg)` to get one tidy row per variable rather than
reading the parallel arrays directly.

| Key | Req. | Description | Used by |
|---|---|---|---|
| `variable` | yes | Variables forecast in this group. | all |
| `duration` | yes | ISO 8601 time step for each variable, parallel to `variable`: `P1D`, `P1W`, `PT30M`, ... | all |
| `max_horizon` | yes | Longest accepted horizon for each variable, **in days**, parallel to `variable`. Submitted rows further ahead are dropped. | submissions |
| `targets_script` | | Script the target-generation workflow runs for this group, e.g. `targets/aquatics_targets.R`. A group without one gets no targets job. | workflows |
| `targets_large_runner` | | `true` to run the group's targets job on `ci$large_runner`. Default `false`. | workflows |
| `targets_file` | | Public URL of the group's targets CSV, on the main store's `endpoint`. The targets scripts upload to this location. | targets, scoring, baselines, catalog |
| `thumbnail_link` | | Image for the group's catalog page. | catalog |
| `thumbnail_title` | | Alt text for `thumbnail_link`. | catalog |
| `focal_sites` | | Suggested starting sites, listed on the dashboard. The dashboard plots forecasts at the first `dashboard$n_plot_sites` of them. | dashboard |
| `sites_column` | | Site metadata column marking this group's sites. Defaults to the lower-case group name. | pkg |
| `theme` | | Prefix of the group's submission file names, e.g. `terrestrial_daily`. Defaults to the lower-case group name. | baselines |
| `baselines` | | The group's baseline models, a list. See [Baselines](#baselines). | baselines |
| `group_vars` | | Per-variable catalog details, keyed by variable name. | catalog |
| `group_vars$<variable>$duration` | | Must agree with the group's `duration` entry for the variable. | catalog |
| `group_vars$<variable>$max_horizon` | | Must agree with the group's `max_horizon` entry for the variable. | catalog |
| `group_vars$<variable>$var_doi` | | DOI of the paper describing this variable's forecasts, or `"None"`. | catalog |
| `group_vars$<variable>$var_citation` | | Citation for that paper, or `"None"`. | catalog |

Each variable and duration pair must appear only once across all groups.
Otherwise the horizon for that pair would be ambiguous.

## Baselines

Each target group lists the baseline (null) models run for it. Each entry has
these fields:

| Field | Description |
|---|---|
| `model_id` | Model id the forecasts are submitted under. |
| `method` | `climatology` (day-of-year mean and sd, normal), `weekly_climatology` (ISO-week mean and sd, log-normal ensemble), `persistence_rw` (random walk, bootstrapped ensemble) or `mean` (historical mean of log(x + 1), bootstrapped ensemble). |
| `schedule` | When the baseline-forecasts workflow runs it: `daily` (its daily cron) or `weekly` (its Sunday cron). |
| `args` | Optional. `variables` (default: all of the group's), `horizon` in days (default: their `max_horizon`), and any argument of the method's function, e.g. `transform: {chla: log}` for `persistence_rw` or `exclude_current_year: true` for `climatology`. See `?challengeci::baseline_climatology` and the related help pages. |

Daily baselines are issued for today. Weekly baselines are issued for the Monday
that starts the current ISO week. Forecasts are uploaded to the submissions
bucket and processed like any participant's.

```yaml
Aquatics:
  baselines:
    - {model_id: climatology, method: climatology, schedule: daily}
    - {model_id: persistenceRW, method: persistence_rw, schedule: daily, args: {transform: {chla: log}}}
```

## NOAA forecasts

| Key | Description | Used by |
|---|---|---|
| `noaa_forecasts$noaa_forecast_groups` | Display names of the driver stages, e.g. `Stage1`. | catalog |
| `noaa_forecasts$noaa_forecast_group_paths` | Folder for each stage under `noaa_forecast_bucket`, parallel to the names. | catalog, drivers |
| `noaa_forecasts$gefs_version` | GEFS version gefs4cast downloads, e.g. `v12`. Default `v12`. | drivers |
| `noaa_forecasts$start_date` | First reference date downloaded (`YYYY-MM-DD`). Missing days after it are filled in. Default `2020-09-24`. | drivers |
| `noaa_forecasts$stage1_site_list_url` | CSV of `site_id`, `latitude`, `longitude` for the stage 1 download. Defaults to the site metadata. NEON keeps a separate list because its coordinates differ slightly and would move one site to another GEFS grid cell. | drivers |

## Model and variable metadata

| Key | Description | Used by |
|---|---|---|
| `model_metadata_gsheet` | Google Sheet of model registrations (form responses). Must be readable by anyone with the link; it is read as a CSV export. Only rows for this `project_id` are used, one per `model_id`. | catalog |
| `target_metadata_gsheet` | Google Sheet of variable long names and descriptions, one row per variable and duration. Read the same way. | catalog |

## GitHub Actions (`ci`)

The workflows read these with `yq` in `.github/workflows/_config.yaml`. See
[operations.md](operations.md) for the workflows and secrets.

| Key | Description |
|---|---|
| `ci$docker_image` | Container image every job runs in, and the image `docker.yaml` builds (also tagged with the build date). Default `eco4cast/rocker-neon4cast:latest`. |
| `ci$large_runner` | Self-hosted runner label for heavy jobs: scoring, stage 3 drivers, and targets groups with `targets_large_runner`. Default `ubuntu-latest`. |
| `ci$healthchecks` | healthchecks.io ping URLs keyed by job name (`targets`, `targets-<group>`, `baselines-daily`, `baselines-weekly`, `submissions`, `bundling`, `scoring`, `drivers-stage1-2`, `drivers-stage3`, `catalog`, `dashboard`, `docker`). A job without an entry is not monitored. |

## Dashboard (`dashboard`)

These settings drive the Quarto site in `dashboard/`. All are optional.

| Key | Description |
|---|---|
| `dashboard$title` | Site title. Defaults to `challenge_long_name`. |
| `dashboard$contact_email` | Contact address shown on the home and instructions pages. |
| `dashboard$registration_form_url` | Model registration form linked from the instructions. |
| `dashboard$n_plot_sites` | Sites per target group in forecast plots, taken from the group's `focal_sites`. Default 2. |
| `dashboard$leaderboard_max_horizon` | Longest horizon, in days, in the leaderboards. Default 35. |
| `dashboard$excluded_models` | Model ids left out of the plots, keyed by variable, or `all` for every variable. |

The site title, URL (`challenge_url`, which also sets the sitemap) and
repository link (`github_repo`) reach Quarto through `dashboard/_challenge.yml`,
written by `Rscript inst/scripts/dashboard.R`. Each target group's description
on the "What to forecast" page is `dashboard/_content/targets/<group>.md`.

## Catalog (`catalog_config`)

These settings drive the STAC catalog built by `inst/scripts/catalog.R`
(`run_catalog()`).

| Key | Description |
|---|---|
| `catalog_title` | Title of the root catalog. |
| `catalog_path` | Folder the catalog is written to, next to this repository (`../<catalog_path>`). The catalog workflow clones `catalog_repo` there. |
| `catalog_repo` | `owner/repo` of the catalog repository. The workflow commits the catalog there, and each model item links to its raw JSON in it. Defaults to `github_repo`. |
| `parent_catalog_url` | Catalog that lists this one as a child. Optional; without it the root catalog has no parent link. |
| `site_columns_file` | CSV (`column`, `description`) describing the site metadata columns, for the sites collection. Optional. |
| `registration_columns` | Optional. Overrides the registration form questions the catalog reads, as `short_name: "Question text"`; see `?registration_columns` for the short names and defaults. |
| `variable_metadata_columns` | Optional. Overrides the column names read from `target_metadata_gsheet` (`long_name`, `variable`, `description`, `duration`). |
| `duration_labels` | Optional. Readable names for durations in catalog folder names, e.g. `P1M: Monthly`. `P1D`, `PT1H`, `PT30M` and `P1W` have defaults. |
| `forecast_path`, `scores_path`, `summaries_path`, `targets_path`, `noaa_path`, `site_path` | Folders for each collection inside `catalog_path`. |
| `about_string`, `about_title` | Link to the challenge documentation, and its title. |
| `documentation_url` | Challenge documentation site. |
| `forecasts_thumbnail`, `scores_thumbnail`, `summaries_thumbnail`, `targets_thumbnail`, `noaa_thumbnail`, `site_thumbnail` | Image for each collection. |
| `forecasts_thumbnail_title`, `scores_thumbnail_title`, `summaries_thumbnail_title`, `targets_thumbnail_title`, `noaa_thumbnail_title`, `site_thumbnail_title` | Alt text for each image. |
| `host_url`, `host_name` | Organisation hosting the challenge. |
| `bbox` | Bounding box of all sites: `max_lat`, `min_lat`, `max_lon`, `min_lon`. |
| `site_type` | GeoJSON geometry type for the sites, e.g. `MultiPoint`. |
| `site_metadata_url` | Site metadata CSV. Kept for the catalog and external tools; new code uses `sites$metadata_url`. |
| `citation_doi`, `citation_doi_link`, `citation_text` | How to cite the challenge. |

## Not in the config

GitHub reads cron schedules from the workflow files before any code runs, so they
cannot come from the config. Set them in `.github/workflows/*.yaml`; the
baselines workflow also uses its weekly cron string to tell the two schedules
apart. Secrets are GitHub repository secrets, listed in
[operations.md](operations.md).
