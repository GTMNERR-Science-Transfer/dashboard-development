# Global settings, e.g. reading in of packages, data

#### Install / load packages ---------------------------------
if(!require(shiny)){ install.packages("shiny") } ;  library(shiny)
if(!require(shinydashboard)){ install.packages("shinydashboard") } ;  library(shinydashboard)
if(!require(tidyverse)){ install.packages("tidyverse") } ;  library(tidyverse)
if(!require(bslib)){ install.packages("bslib") } ;  library(bslib)
if(!require(leaflet)){ install.packages("leaflet") } ;  library(leaflet)
if(!require(sf)){ install.packages("sf") } ;  library(sf)
if(!require(fs)){ install.packages("fs") } ;  library(fs)
if(!require(plotly)){ install.packages("plotly") } ;  library(plotly)
if(!require(shinyWidgets)){ install.packages("shinyWidgets") } ;  library(shinyWidgets)

#### Get app.R file dir and set work dir ---------------------
# find file_name from current working directory
# before trying from a shallower directory
file_name <- "*/app.R" # The file you are searching for

try({
  found_dir <- find_directory_of_file(file_name)
  # Check if found_dir is NULL or empty, indicating the file was not found
  if (is.null(found_dir) || length(found_dir) == 0) {
    # print error
    print("/app.R not found from current working directory!")
    print("trying again from shallower directory")
    # trying again from great grandparent directory of working directory
    setwd("../../..")
    found_dir <- find_directory_of_file(file_name)
    if (is.null(found_dir) || length(found_dir) == 0) {
      print("app.R likely does not exist in filesystem!")
    }
    # Set working directory to parent directory of found dir
    setwd(fs::path_dir(found_dir[1]))
    # Print working directory
    print(paste0("Working dir: ", getwd()))
  }
  # Set working directory to parent directory of found dir
  setwd(found_dir[1])
  # Print working directory
  print(paste0("Working dir: ", getwd()))
}, silent = FALSE) # Setting silent = FALSE will print the error message to the console

### Load shapefiles ------------------------------------------------

##### GTMNERR shapefile ------------------------------------------------
GTMNERR <- st_read("./03_Data_for_app/shapefiles_new/GTMNERR_small_nosea.shp")

##### county shapefiles ------------------------------------------------
counties_select <- st_read("./03_Data_for_app/shapefiles_new/counties_GTMNERR.shp")

##### HUC shapefiles ------------------------------------------------
HUC10 <- st_read("./03_Data_for_app/shapefiles_new/nhdwbd_huc10_dec17_GTMNERR.shp")
HUC12 <- st_read("./03_Data_for_app/shapefiles_new/nhdwbd_huc12_dec17_GTMNERR.shp")

##### Outstanding Florida Waters (OFW) shapefiles ------------------------------
ofw <- st_read("./03_Data_for_app/shapefiles_new/ofw_GTMNERR.shp")

##### mangrove shapefiles -----------------------------------------------------
mangroves <- st_read("./03_Data_for_app/shapefiles_new/mangrove_GTMNERR.shp")

##### salt marsh shapefiles -----------------------------------------------------
saltmarsh <- st_read("./03_Data_for_app/shapefiles_new/salt_marsh_GTMNERR.shp")
