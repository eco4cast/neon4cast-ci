FROM rocker/geospatial:latest

#USER root
RUN apt-get update && apt-get -y install cron jags libgd-dev libnetcdf-dev default-jdk-headless libuv1-dev

RUN sudo update-ca-certificates

RUN apt-get -y install python3 python3-venv python3-pip python3-dev

RUN install2.r devtools remotes reticulate neonstore RCurl neonUtilities contentid minioclient fs glue

RUN install2.r renv rjags ISOweek RNetCDF fable fabletools forecast imputeTS duckdbfs gsheet patchwork pak ggiraph

RUN install2.r ncdf4 scoringRules tidybayes tidync udunits2 bench yaml here feasts future furrr jsonlite bsicons bslib neonUtilities

RUN R -e "remotes::install_github('eco4cast/stac4cast')"
RUN sleep 180
RUN R -e "remotes::install_github('eco4cast/score4cast')"
RUN sleep 180
RUN R -e "remotes::install_github('eco4cast/neon4cast')"
RUN sleep 180
RUN R -e "remotes::install_github('eco4cast/gefs4cast', upgrade = FALSE)"
RUN sleep 180
RUN R -e "remotes::install_github('mitchelloharawild/distributional', ref = 'bb0427e')"
RUN R -e "remotes::install_version('arrow', version = '20.0.0')"
