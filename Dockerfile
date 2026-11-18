# syntax=docker/dockerfile:1
# A versioned rocker tag also fixes the CRAN snapshot that install2.r uses, so a
# rebuild installs the same package versions. To upgrade, change the tag (see
# https://hub.docker.com/r/rocker/geospatial/tags) and the commits below.
FROM rocker/geospatial:4.6.2

ENV AWS_REQUEST_CHECKSUM_CALCULATION=when_required \
    AWS_RESPONSE_CHECKSUM_VALIDATION=when_required

#USER root
RUN apt-get update && apt-get -y install cron jags libgd-dev libnetcdf-dev default-jdk-headless libuv1-dev

RUN sudo update-ca-certificates

RUN apt-get -y install python3 python3-venv python3-pip python3-dev

RUN install2.r devtools remotes reticulate neonstore RCurl neonUtilities contentid minioclient fs glue

RUN install2.r renv rjags ISOweek RNetCDF fable fabletools forecast imputeTS duckdbfs gsheet patchwork pak ggiraph

RUN install2.r ncdf4 scoringRules tidybayes tidync udunits2 bench yaml here feasts future furrr jsonlite bsicons bslib future.apply progressr

# GitHub installs authenticate with the build secret `github_pat` (passed by
# .github/workflows/docker.yaml) so they are not rate-limited; that is what the
# old `sleep 180` steps between installs were working around. A secret mount,
# unlike a build-arg, is never stored in the image or its history. Without the
# secret (e.g. a local `docker build`), installs run unauthenticated.
# Each package is pinned to a commit so a rebuild cannot pick up an untested
# change; bump a commit deliberately to upgrade. stac4cast, neon4cast and
# EFIstandards are no longer used by this repository's code, but participants
# use this image to build and submit forecasts.
RUN --mount=type=secret,id=github_pat \
    export GITHUB_PAT="$(cat /run/secrets/github_pat 2>/dev/null)" && \
    R -e "remotes::install_github('eco4cast/stac4cast@7d493c61fcc5c5e0025b60910a0686d4b423b9c0')" && \
    R -e "remotes::install_github('eco4cast/score4cast@f33c9e1352ad40bd743c072eb27c770bd18b0abe')" && \
    R -e "remotes::install_github('eco4cast/neon4cast@0705c8c50db648db168d3a1e836b1e68e54e2dd2')" && \
    R -e "remotes::install_github('eco4cast/EFIstandards@7045b4cc8003a3c9d82a2bda31e44ccbbbeb55af')" && \
    R -e "remotes::install_github('appelmar/gdalcubes@1193cbaead912277a8d7473ee7e538584431f5b6')" && \
    R -e "remotes::install_github('eco4cast/gefs4cast@6eb7675a95a3d8b1e604470f16dcbc182b1b4cd9', upgrade = FALSE)" && \
    R -e "remotes::install_github('mitchelloharawild/distributional', ref = 'bb0427e')"
RUN R -e "remotes::install_version('arrow', version = '20.0.0')"
# dev duckdbfs (bundling scripts previously installed this live at runtime)
RUN --mount=type=secret,id=github_pat \
    export GITHUB_PAT="$(cat /run/secrets/github_pat 2>/dev/null)" && \
    R -e "remotes::install_github('cboettig/duckdbfs@0b48916e7b4085d951318e2bd42d14c9433fa182', upgrade = FALSE)"
