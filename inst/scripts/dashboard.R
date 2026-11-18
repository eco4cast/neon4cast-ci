# Stage the data the dashboard plots. Run before `quarto render dashboard`.
#
# Queries the public buckets and writes dashboard/data/ and
# dashboard/_challenge.yml (site title and links for Quarto). See
# ?challengeci::stage_dashboard_data. Reads only public data, so it needs no
# credentials. Quarto writes the sitemap itself, from the config's
# challenge_url.
#
# Run from the repository root:  Rscript inst/scripts/dashboard.R
#
# Environment variables
#   CHALLENGE_CONFIG   config file (default challenge_configuration.yaml)

pkgload::load_all(quiet = TRUE)
stage_dashboard_data(read_config())
