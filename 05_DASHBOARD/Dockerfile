FROM rocker/shiny:4.3.2
# Define BRANCH_NAME as an argument
ARG BRANCH_NAME
# You can use the ARG value as an environment variable if needed
ENV BRANCH_NAME=${BRANCH_NAME}
RUN install2.r rsconnect shiny tidyverse bslib leaflet sf shinydashboard plotly shinyWidgets vroom formattable DT
WORKDIR /usr/src/dashboard-development
# Copy the 'app' directory contents into the container
COPY ./app.R /usr/src/dashboard-development/
COPY ./global.R /usr/src/dashboard-development/
COPY ./functions.R /usr/src/dashboard-development/
COPY ./03_Data_for_app /usr/src/dashboard-development/03_Data_for_app/
COPY ./04_Tests /usr/src/dashboard-development/04_Tests/
COPY ./05_DASHBOARD /usr/src/dashboard-development/05_DASHBOARD/
COPY ./modules /usr/src/dashboard-development/modules/
CMD Rscript /usr/src/dashboard-development/05_DASHBOARD/deploy.R
