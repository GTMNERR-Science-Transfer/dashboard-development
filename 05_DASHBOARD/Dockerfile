FROM rocker/shiny:4.3.2
# Define BRANCH_NAME as an argument
ARG BRANCH_NAME
# You can use the ARG value as an environment variable if needed
ENV BRANCH_NAME=${BRANCH_NAME}
RUN install2.r rsconnect shiny tidyverse bslib leaflet sf
WORKDIR /usr/src/dashboard-development
# Copy the 'app' directory contents into the container at /usr/src/app
COPY ./ /usr/src/dashboard-development
CMD Rscript /usr/src/dashboard-development/05_DASHBOARD/deploy.R
