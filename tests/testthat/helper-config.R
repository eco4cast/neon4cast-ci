# Read the minimal test config, with the site metadata path made absolute so
# tests do not depend on the working directory.
test_config <- function() {
  cfg <- read_config(test_path("fixtures", "minimal_config.yaml"))
  cfg$sites$metadata_url <- test_path("fixtures", "sites.csv")
  cfg
}

# Write a modified copy of the test config and return its path.
write_config <- function(cfg) {
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
  yaml::write_yaml(cfg, path)
  path
}
