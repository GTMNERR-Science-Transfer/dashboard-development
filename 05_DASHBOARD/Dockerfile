FROM rocker/shiny:4.3.2
RUN install2.r rsconnect shiny tidyverse bslib leaflet sf
WORKDIR /usr/src/dashboard-development
# Copy the 'app' directory contents into the container at /usr/src/app
COPY ./dashboard-development /usr/src/dashboard-development
CMD Rscript /usr/src/dashboard-development/05_DASHBOARD/deploy.R
