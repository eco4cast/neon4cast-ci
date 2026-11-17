# Challenge configuration reference

Every challenge-specific setting lives in
[`challenge_configuration.yaml`](../challenge_configuration.yaml) at the repository
root. Code reads it with `read_config()` from the `challengeci` package (see
[developing.md](developing.md)). To use a different file, for example in tests or
when trying out a new challenge, set the environment variable `CHALLENGE_CONFIG`
to its path.

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
or `pkg` for the `challengeci` package in general.

## Identity

| Key | Req. | Description | Used by |
|---|---|---|---|
| `project_id` | yes | Short challenge id, e.g. `neon4cast`. Forms the `project_id=` partition in buckets and appears in every forecast and score row. | submissions, scoring, catalog, dashboard |
| `challenge_long_name` | | Full name shown in the catalog. | catalog |
| `github_repo` | | `owner/repo` of this repository; used to build links to files. | catalog, dashboard |
| `challenge_url` | | Public dashboard URL. | catalog, dashboard |
| `example_model_id` | | Model id participants use for test submissions. | (documentation only) |

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
| `model_metadata_bucket` | | One JSON file per registered model. | catalog |
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
| `targets_file` | | Public URL of the group's targets CSV. | targets, scoring, catalog |
| `thumbnail_link` | | Image for the group's catalog page. | catalog |
| `thumbnail_title` | | Alt text for `thumbnail_link`. | catalog |
| `sites_column` | | Site metadata column marking this group's sites. Defaults to the lower-case group name. | pkg |
| `group_vars` | | Per-variable catalog details, keyed by variable name. | catalog |
| `group_vars$<variable>$duration` | | Must agree with the group's `duration` entry for the variable. | catalog |
| `group_vars$<variable>$max_horizon` | | Must agree with the group's `max_horizon` entry for the variable. | catalog |
| `group_vars$<variable>$var_doi` | | DOI of the paper describing this variable's forecasts, or `"None"`. | catalog |
| `group_vars$<variable>$var_citation` | | Citation for that paper, or `"None"`. | catalog |

Each variable and duration pair must appear only once across all groups.
Otherwise the horizon for that pair would be ambiguous.

## NOAA forecasts

| Key | Description | Used by |
|---|---|---|
| `noaa_forecasts$noaa_forecast_groups` | Display names of the driver stages, e.g. `Stage1`. | catalog |
| `noaa_forecasts$noaa_forecast_group_paths` | Folder for each stage under `noaa_forecast_bucket`, parallel to the names. | catalog |

## Model and variable metadata

| Key | Description | Used by |
|---|---|---|
| `model_metadata_gsheet` | Google Sheet of model registrations (form responses). | catalog |
| `target_metadata_gsheet` | Google Sheet of variable descriptions and units. | catalog |

## Catalog (`catalog_config`)

These settings drive the STAC catalog built by `catalog/`.

| Key | Description |
|---|---|
| `catalog_title` | Title of the root catalog. |
| `catalog_path` | Folder the catalog is written to. |
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
cannot come from the config. Set them in `.github/workflows/*.yaml`.
