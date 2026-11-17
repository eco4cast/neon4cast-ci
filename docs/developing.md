# Developing the CI

Shared code lives in a small R package, `challengeci`, inside this repository:

```
DESCRIPTION, NAMESPACE   package metadata (NAMESPACE is generated, do not edit)
R/                       package functions, documented with roxygen comments
man/                     help pages (generated, do not edit)
tests/testthat/          unit tests and small fixtures
challenge_configuration.yaml   every challenge-specific setting (docs/configuration.md)
```

The stage folders (`submission_processing/`, `scoring/`, `catalog/`, ...) hold
the scripts the workflows run. They are being moved onto the package one stage
at a time. Challenge-specific data downloads (`targets/`) stay as plain scripts.

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
- **Writes respect dry runs.** Send every write to S3 through `upload_file()`,
  or through a path from `output_uri()`, so that `DRY_RUN=true` sends it to a
  local folder.
- **Comments explain why.** Every exported function has roxygen docs with
  `@param` and `@return`.

## Testing without touching production

- `DRY_RUN=true` sends all package writes to `DRY_RUN_DIR` (default: a
  temporary folder). Reads still use the real buckets.
- `CHALLENGE_CONFIG=path/to/test.yaml` points everything at a different
  config, for example one whose buckets are a scratch prefix.
- Public buckets can be read without credentials:
  `s3_connect(cfg, "main", anonymous = TRUE)`.
