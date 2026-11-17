# syntax=docker/dockerfile:1
FROM rocker/geospatial:latest

ENV AWS_REQUEST_CHECKSUM_CALCULATION=when_required \
    AWS_RESPONSE_CHECKSUM_VALIDATION=when_required

#USER root
RUN apt-get update && apt-get -y install cron jags libgd-dev libnetcdf-dev default-jdk-headless libuv1-dev

RUN sudo update-ca-certificates

RUN apt-get -y install python3 python3-venv python3-pip python3-dev

RUN install2.r devtools remotes reticulate neonstore RCurl neonUtilities contentid minioclient fs glue

RUN install2.r renv rjags ISOweek RNetCDF fable fabletools forecast imputeTS duckdbfs gsheet patchwork pak ggiraph

RUN install2.r ncdf4 scoringRules tidybayes tidync udunits2 bench yaml here feasts future furrr jsonlite bsicons bslib neonUtilities future.apply progressr

# GitHub installs authenticate with the build secret `github_pat` (passed by
# .github/workflows/docker.yaml) so they are not rate-limited; that is what the
# old `sleep 180` steps between installs were working around. A secret mount,
# unlike a build-arg, is never stored in the image or its history. Without the
# secret (e.g. a local `docker build`), installs run unauthenticated.
RUN --mount=type=secret,id=github_pat \
    export GITHUB_PAT="$(cat /run/secrets/github_pat 2>/dev/null)" && \
    R -e "remotes::install_github('eco4cast/stac4cast')" && \
    R -e "remotes::install_github('eco4cast/score4cast')" && \
    R -e "remotes::install_github('eco4cast/neon4cast')" && \
    R -e "remotes::install_github('eco4cast/EFIstandards')" && \
    R -e "remotes::install_github('eco4cast/gefs4cast', upgrade = FALSE)" && \
    R -e "remotes::install_github('mitchelloharawild/distributional', ref = 'bb0427e')"
RUN R -e "remotes::install_version('arrow', version = '20.0.0')"
# dev duckdbfs (bundling scripts previously installed this live at runtime)
RUN --mount=type=secret,id=github_pat \
    export GITHUB_PAT="$(cat /run/secrets/github_pat 2>/dev/null)" && \
    R -e "remotes::install_github('cboettig/duckdbfs', upgrade = FALSE)"
