# docs/configuration.md must describe every key the config uses. These tests
# fail when a key is added to the config without documenting it.

config_doc <- function() {
  path <- test_path("..", "..", "docs", "configuration.md")
  skip_if_not(file.exists(path), "not running inside the repository")
  paste(readLines(path), collapse = "\n")
}

repo_config <- function() {
  path <- test_path("..", "..", "challenge_configuration.yaml")
  skip_if_not(file.exists(path), "not running inside the repository")
  yaml::read_yaml(path)
}

# Keys missing from the doc. A key counts as documented when it appears in
# backticks on its own (`sites`) or as the start of a path (`sites$columns`).
documented <- function(doc, keys) {
  found <- vapply(keys, function(k) grepl(paste0("`", k, "[`$]"), doc), logical(1))
  keys[!found]
}

test_that("every top-level config key is documented", {
  doc <- config_doc()
  keys <- names(repo_config())
  expect_equal(documented(doc, keys), character(0))
})

test_that("every target group and catalog key is documented", {
  doc <- config_doc()
  cfg <- repo_config()
  group_keys <- unique(unlist(lapply(cfg$target_groups, names)))
  # Keys may appear in a list, e.g. `forecasts_thumbnail`, `scores_thumbnail`.
  keys <- c(group_keys, names(cfg$catalog_config))
  expect_equal(documented(doc, keys), character(0))
})
