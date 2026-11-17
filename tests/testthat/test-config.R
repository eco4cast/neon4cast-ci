test_that("the repository's own config is valid", {
  path <- test_path("..", "..", "challenge_configuration.yaml")
  skip_if_not(file.exists(path), "not running inside the repository")
  cfg <- read_config(path)
  expect_equal(nrow(config_problems(cfg)), 0)
  expect_equal(cfg$project_id, "neon4cast")
})

test_that("read_config() honours CHALLENGE_CONFIG", {
  withr::local_envvar(CHALLENGE_CONFIG = test_path("fixtures", "minimal_config.yaml"))
  expect_equal(read_config()$project_id, "testcast")
})

test_that("read_config() explains a missing file", {
  expect_error(read_config("no-such-file.yaml"), "does not exist")
})

test_that("defaults fill gaps but never override the file", {
  cfg <- test_config()
  # Not in the fixture: comes from config_defaults().
  expect_equal(cfg$credentials$main$key_env, "OSN_KEY")
  # In the fixture: the file wins.
  expect_equal(cfg$scratch$bucket, "scratch-bucket/tmp")
})

test_that("a missing required key is an error", {
  cfg <- yaml::read_yaml(test_path("fixtures", "minimal_config.yaml"))
  cfg$forecasts_bucket <- NULL
  expect_error(read_config(write_config(cfg)), "forecasts_bucket")
})

test_that("arrays of different lengths are an error", {
  cfg <- yaml::read_yaml(test_path("fixtures", "minimal_config.yaml"))
  cfg$target_groups$Lakes$max_horizon <- list(35)
  problems <- config_problems(cfg)
  expect_equal(problems$severity, "error")
  expect_match(problems$message, "same length")
})

test_that("bad durations and horizons are errors", {
  cfg <- yaml::read_yaml(test_path("fixtures", "minimal_config.yaml"))
  cfg$target_groups$Lakes$duration <- list("P1D", "daily")
  cfg$target_groups$Lakes$max_horizon <- list(35, -1)
  problems <- config_problems(cfg)
  problems <- problems[problems$severity == "error", ]
  expect_setequal(problems$key, c("target_groups$Lakes$duration",
                                  "target_groups$Lakes$max_horizon"))
  expect_match(problems$message[1], "daily")
})

test_that("a variable/duration pair listed twice is an error", {
  cfg <- yaml::read_yaml(test_path("fixtures", "minimal_config.yaml"))
  cfg$target_groups$Birds$variable <- list("temperature")
  cfg$target_groups$Birds$duration <- list("P1D")
  cfg$target_groups$Birds$group_vars <- list(
    temperature = list(duration = "P1D", max_horizon = 730)
  )
  problems <- config_problems(cfg)
  expect_true(any(grepl("temperature with duration P1D appears more than once",
                        problems$message)))
})

test_that("group_vars that disagree with the arrays give warnings", {
  cfg <- yaml::read_yaml(test_path("fixtures", "minimal_config.yaml"))
  cfg$target_groups$Lakes$group_vars$oxygen$max_horizon <- 30
  cfg$target_groups$Lakes$group_vars$temperature$max_horizon <- NULL
  cfg$target_groups$Lakes$group_vars$chla <- list(duration = "P1D")
  problems <- config_problems(cfg)
  expect_true(all(problems$severity == "warning"))
  expect_equal(nrow(problems), 3)
  # Warnings do not stop read_config().
  expect_warning(read_config(write_config(cfg)), "3 inconsistencies")
})

test_that("length-one lists and scalars in group_vars are equivalent", {
  cfg <- yaml::read_yaml(test_path("fixtures", "minimal_config.yaml"))
  cfg$target_groups$Lakes$group_vars$oxygen$duration <- list("P1D")
  cfg$target_groups$Lakes$group_vars$oxygen$max_horizon <- list(35)
  expect_equal(nrow(config_problems(cfg)), 0)
})

test_that("target_variables() gives one tidy row per variable", {
  vars <- target_variables(test_config())
  expect_named(vars, c("group", "variable", "duration", "max_horizon",
                       "targets_file", "var_doi", "var_citation"))
  expect_equal(vars$variable, c("temperature", "oxygen", "richness"))
  expect_equal(vars$group, c("Lakes", "Lakes", "Birds"))
  expect_equal(vars$max_horizon, c(35, 35, 730))
  expect_equal(vars$var_doi, c("10.1/x", NA, NA))
  expect_true(is.na(vars$targets_file[3]))
})

test_that("config_get() returns values, defaults, or a named error", {
  cfg <- test_config()
  expect_equal(config_get(cfg, "scratch", "endpoint"), "scratch.example.org")
  expect_null(config_get(cfg, "ci", "docker_image", default = NULL))
  expect_error(config_get(cfg, "ci", "docker_image"), "ci\\$docker_image")
})

test_that("group_config() lists the valid groups when given a bad one", {
  cfg <- test_config()
  expect_equal(group_config(cfg, "Birds")$sites_column, "has_birds")
  expect_error(group_config(cfg, "Fish"), "Lakes")
})
