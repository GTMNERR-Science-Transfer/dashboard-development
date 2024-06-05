# Global settings, e.g. reading in of packages, data

#### Install / load packages ---------------------------------
if(!require(shiny)){ install.packages("shiny") } ;  library(shiny)
if(!require(shinydashboard)){ install.packages("shinydashboard") } ;  library(shinydashboard)
if(!require(tidyverse)){ install.packages("tidyverse") } ;  library(tidyverse)
if(!require(bslib)){ install.packages("bslib") } ;  library(bslib)
if(!require(leaflet)){ install.packages("leaflet") } ;  library(leaflet)
if(!require(sf)){ install.packages("sf") } ;  library(sf)
if(!require(fs)){ install.packages("fs") } ;  library(fs)


#### Get app.R file dir and set work dir ---------------------
# Function to find the directory of a file named app.R
find_directory_of_file <- function(file_name, start_dir=getwd()) {
  # Recursively list all files starting from the start_dir
  app_dir <- fs::dir_ls(start_dir, recurse = TRUE, glob=file_name)
  
  # Check if any file named app.R is found
  if (length(app_dir) > 0) {
    # Assuming you want the directory of the first matching file
    file_dir <- fs::path_dir(app_dir[1])
    return(file_dir)
  } else {
    return(NULL) # Return NULL if the file is not found
  }
}

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

#### WQ locations data ------------------------------------------------
load("../03_Data_for_app/WQ_locations.RData")
WQ_data_locations <- WQ_locations %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#### WQ data ------------------------------------------------
load("../03_Data_for_app/WQ.RData")

#### GTMNERR shapefile ------------------------------------------------
GTMNERR <- st_read("../03_Data_for_app/shapefiles_new/GTMNERR.shp")
GTMNERR <- st_transform(GTMNERR, crs = 4326)

#### county shapefiles ------------------------------------------------
counties_select <- st_read("../03_Data_for_app/shapefiles_new/counties_GTMNERR.shp")
counties_select <- st_transform(counties_select, crs = 4326)
