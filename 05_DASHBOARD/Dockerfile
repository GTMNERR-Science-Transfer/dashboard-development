FROM rocker/shiny:4.3.2
RUN install2.r rsconnect shiny tidyverse bslib leaflet sf
WORKDIR /home/dashboard
COPY app.R 
COPY server.R server.R 
COPY likes.rds likes.rds
COPY deploy.R deploy.R
CMD Rscript deploy.R
