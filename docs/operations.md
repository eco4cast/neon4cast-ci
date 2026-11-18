# Running the challenge

Every stage is a GitHub Actions workflow in `.github/workflows/`. Each one
first calls `_config.yaml`, which reads `challenge_configuration.yaml` with
`yq` and passes on the container image (`ci$docker_image`), the large runner
(`ci$large_runner`) and the job matrices. Jobs then run an entry script in
`inst/scripts/` (or a targets script) inside that image.

## Workflows

Times are UTC. "Large" means the self-hosted runner named by `ci$large_runner`.

| Workflow (file) | Schedule | Runner | Runs | Healthcheck |
|---|---|---|---|---|
| process-submissions (`submissions.yaml`) | every 2 h | GitHub | `process_submissions.R` | `submissions` |
| bundling (`bundling.yaml`) | every 6 h | GitHub | `bundle.R forecasts`, `bundle.R summaries` | `bundling` |
| scoring (`scoring.yaml`) | 10:00 | large | `score.R` | `scoring` |
| target-generation (`targets.yaml`) | 07:00 | GitHub, or large for groups with `targets_large_runner` | each group's `targets_script` | `targets-<group>`, then `targets` when all succeed |
| baseline-forecasts (`baselines.yaml`) | 20:00 daily; 01:00 Sunday weekly | GitHub | `baselines.R <schedule> <group>` per group with a baseline on that schedule | `baselines-daily` / `baselines-weekly` when all succeed |
| drivers-stage1-2 (`drivers_stage1_2.yaml`) | 05:00 | GitHub | `drivers_stage1_2.R` | `drivers-stage1-2` |
| drivers-stage3 (`drivers_stage3.yaml`) | 23:00 | large | `drivers_stage3.R` (manual run can rebuild) | `drivers-stage3` |
| catalog (`catalog.yaml`) | 23:00 | GitHub | `catalog.R`, then commits to `catalog_config$catalog_repo` | `catalog` |
| dashboard (`dashboard.yaml`) | 08:00 | GitHub | `dashboard.R`, `quarto render dashboard`, publishes to `gh-pages` | `dashboard` |
| docker container (`docker.yaml`) | 00:00, and on Dockerfile changes | GitHub | builds and pushes `ci$docker_image` | `docker` |
| package-tests (`package-tests.yaml`) | pull requests | GitHub | unit tests, docs check, R CMD check | none |

Each workflow that writes shared data has a `concurrency` group, so a run
waits for the previous one instead of overlapping it. Each job has a
`timeout-minutes` about three times its longest recent run.

### Healthchecks

The last step of a job pings the URL under its name in `ci$healthchecks`
(through `ping_healthcheck()`), so a ping means the job succeeded. A job with
no URL in the config is simply not monitored; add one to start monitoring it.
`submissions`, `bundling`, `scoring` and `dashboard` have no URL yet.

## Secrets

Set these as GitHub repository secrets. The config names the environment
variables the code reads (`credentials`); the workflows map secrets onto them.

| Secret | Read as | Used by | For |
|---|---|---|---|
| `OSN_KEY`, `OSN_SECRET` | same | most workflows | writing to the main store (`endpoint`) |
| `AWS_ACCESS_KEY_SUBMISSIONS`, `AWS_SECRET_ACCESS_KEY_SUBMISSIONS` | same | process-submissions | listing and moving files in the submissions bucket |
| `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` | `AWS_ACCESS_KEY_SUBMISSIONS`, `AWS_SECRET_ACCESS_KEY_SUBMISSIONS` | baseline-forecasts | uploading baseline forecasts to the submissions bucket |
| `EFI_NRP_KEY`, `EFI_NRP_SECRET` | same | scoring, target-generation | the scratch store (`scratch`) |
| `NEON_TOKEN` | same | target-generation | NEON API downloads |
| `CATALOG_TOKEN` | `EXTERNAL_GH_PAT` | catalog | pushing to the catalog repository |
| `DOCKERHUB_USERNAME`, `DOCKERHUB_TOKEN` | — | docker container | pushing the image to Docker Hub |
| `PAT` | build secret `github_pat` | docker container | GitHub installs during the build, without rate limits |

The baselines and submissions workflows upload to the same bucket with
different secrets. If both are the same key, the baselines workflow can use
the `AWS_ACCESS_KEY_SUBMISSIONS` secrets too, and `AWS_ACCESS_KEY_ID` can be
deleted.

## Common tasks

- **Rerun a stage:** open the workflow in the Actions tab and choose *Run
  workflow*. Concurrency groups queue it behind any run in progress.
- **Try a change without writing data:** run the entry script locally with
  `DRY_RUN=true` (see [developing.md](developing.md)); writes go to
  `DRY_RUN_DIR`.
- **Rebuild stage 3 drivers from scratch:** run *drivers-stage3* manually with
  *rebuild* ticked. It can take many hours, so its timeout is 24 h.
- **Run baselines by hand:** run *baseline-forecasts* manually and choose daily
  or weekly.
- **Roll back the image:** each build is also tagged with its date. Set
  `ci$docker_image` to e.g. `eco4cast/rocker-neon4cast:2026-11-18` to pin every
  workflow to that build, and back to `:latest` to follow new builds.
- **Upgrade a package in the image:** change its pinned commit (or the rocker
  base tag) in the `Dockerfile`; pushing the change to `main` rebuilds it.

## Scoring older forecasts and rescoring

Each scoring run scores forecast rows from the last six months that have an
observation but no score yet. Rows already scored are never scored again.

- **Score older forecasts** (after a long outage, or when observations arrive
  late): run *scoring* manually with *since* set to a date, e.g. `2025-01-01`.
  Locally: `Rscript inst/scripts/score.R 2025-01-01`. A long window takes
  longer and uses more scratch space.
- **Rescore a model or variable** (after its observations were corrected):
  its existing scores must go first, because scored rows are skipped. Copy the
  model's folder under `<scores_bucket>/bundled-parquet/` aside (e.g. with
  `mc cp --recursive`), delete it, then run *scoring* with *since* early enough
  to cover the model's forecasts.

## Recovering from failed bundling

Bundling moves each processed file through three folders of the forecasts
bucket (summaries use `summaries/`, `bundled-summaries/`, `archive-summaries/`):

```
parquet/<model path>/...   new, not yet bundled
bundled-parquet/<model path>/data_0.parquet   one file per model
archive-parquet/<model path>/...   bundled, kept as the raw record
```

- **A run fails:** usually nothing to do. A model that fails keeps its files
  in `parquet/`, the others still finish, and the next run retries it. If a
  run uploaded a bundle but failed before archiving the files, the next run
  recognises the rows (same `reference_datetime` and `pub_datetime`) and does
  not add them twice. The log names each model that failed and why.
- **A bundle is damaged:** rebuild it from the archive. Copy the bundle aside,
  move the model's files from `archive-parquet/<model path>/` back to
  `parquet/<model path>/`, delete `bundled-parquet/<model path>/`, and run
  *bundling*. It rebuilds the bundle from every archived file.
- **Check before changing anything:** `DRY_RUN=true VERBOSE=true Rscript
  inst/scripts/bundle.R forecasts` reads the real buckets, writes the bundles
  locally, and counts the rows in every bundle before and after.

## The submissions bucket workaround

Since 2026-09-08 the NRP submissions key can only list the submissions bucket:
reading and deleting with it fail with "Insufficient permissions". The
bucket's own policy lets anyone read and delete, so processing lists the
bucket with the key and reads and deletes anonymously. Deleting may still
fail, so processing also keeps its own record of every submission handled
(`processed_submissions`, on the main store) and skips those files. The two
minio client aliases are at the top of `R/submissions.R`; once NRP restores
the key's permissions, drop the anonymous one there.
