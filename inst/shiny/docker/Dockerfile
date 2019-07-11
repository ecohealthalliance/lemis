FROM rocker/shiny-verse:3.5.2
MAINTAINER "Noam Ross" ross@ecohealthalliance.org

RUN install2.r --error --skipinstalled \
  lubridate \
  countrycode \
  htmlwidgets

RUN installGithub.r ecohealthalliance/lemis@devs mattflor/chorddiag

COPY . /srv/shiny-server/
