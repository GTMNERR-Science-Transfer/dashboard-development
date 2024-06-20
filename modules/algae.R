HABPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Harmful Algal Bloom Data"),
    fluidRow(
      #User inputs in 1st column
      column(width = 3, selectInput(ns("genus"), "What genus of Algae do you want data for?", c("All", unique(GeneraData$genus))),
             uiOutput("selectSpecies"),
             uiOutput("selectDate")), #Reserve space for species dropdown input
      # Map occupies 2nd column
      column(width = 5, leafletOutput(ns("map"), height=750)),
      # plot occupies rows in the 3rd column
      column(width = 4, plotOutput(ns("timePlot")),
             sliderInput("bins", "Number of bins:", 
                         min = 1, max = 50, value = 30))
      ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page") #All input IDs need to be inside ns()
  )
}

HABPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
      })
  })
}


# ALL OF THIS CODE MIGHT BE HELPFUL FOR THIS PAGE
# 
# # NOTE: .RData restores the object to the name it had when you saved it as .RData
# #load("03_Data_for_app/HAB.RData")
# 
# #### Get app.R file dir and set work dir ---------------------
# # Function to find the directory of a file named app.R
# find_directory_of_file <- function(file_name, start_dir=getwd()) {
#   # Recursively list all files starting from the start_dir
#   app_dir <- fs::dir_ls(start_dir, recurse = TRUE, glob=file_name)
#   
#   # Check if any file named app.R is found
#   if (length(app_dir) > 0) {
#     # Assuming you want the directory of the first matching file
#     file_dir <- fs::path_dir(app_dir[1])
#     return(file_dir)
#   } else {
#     return(NULL) # Return NULL if the file is not found
#   }
# }
# 
# # find file_name from current working directory
# # before trying from a shallower directory
# file_name <- "*/app.R" # The file you are searching for
# 
# try({
#   found_dir <- find_directory_of_file(file_name)
#   # Check if found_dir is NULL or empty, indicating the file was not found
#   if (is.null(found_dir) || length(found_dir) == 0) {
#     # print error
#     print("/app.R not found from current working directory!")
#     print("trying again from shallower directory")
#     # trying again from great grandparent directory of working directory
#     setwd("../../..")
#     found_dir <- find_directory_of_file(file_name)
#     if (is.null(found_dir) || length(found_dir) == 0) {
#       print("app.R likely does not exist in filesystem!")
#     }
#     # Set working directory to parent directory of found dir
#     setwd(fs::path_dir(found_dir[1]))
#     # Print working directory
#     print(paste0("Working dir: ", getwd()))
#   }
#   # Set working directory to parent directory of found dir
#   setwd(found_dir[1])
#   # Print working directory
#   print(paste0("Working dir: ", getwd()))
# }, silent = FALSE) # Setting silent = FALSE will print the error message to the console
# 
# 
# #### HAB data ------------------------------------------------
# load("./03_Data_for_app/HAB.RData")
# HAB_data_locations <- HAB %>% 
#   select(Latitude, Longitude, Site, `Collection Agency`, County) %>% # took out `HAB ID` (otherwise 1172 instead of 17 locations)
#   distinct() %>% 
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# 
# #### GTMNERR shapefile ------------------------------------------------
# GTMNERR <- st_read("./03_Data_for_app/shapefiles_new/GTMNERR.shp")
# GTMNERR <- st_transform(GTMNERR, crs = 4326)
# 
# #### county shapefiles ------------------------------------------------
# counties_select <- st_read("./03_Data_for_app/shapefiles_new/counties_GTMNERR.shp")
# counties_select <- st_transform(counties_select, crs = 4326)
# 
# 
# 
# # Alternatively, with .Rds you can give it a different name
# #HAB_data <- readRDS("HAB.Rds")
# 
# ############################################
# #  Shiny user interface (ui)
# ############################################
# ui <- fluidPage(
#   #     # Layout for user options
#   #     Code from another app, as example
#   #     navbarPage(title = "FWC Harmful Algae Bloom (HAB) data",
#   #                theme = bs_theme( version = 5, bootswatch = "cerulean" ),
#   #                # Station dropdown menu
#   #                # div(style="display:inline-block;vertical-align:top;",
#   #                #     selectInput(inputId = "site",
#   #                #                 label = "Location",
#   #                #                 choices = Site, 
#   #                #                 selected = "North Ponte Vedra", 
#   #                #                 width = "350px")),
#   #                # 
#   #                # # Horizontal space
#   #                div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>"))
#   #                # Variable dropdown menu
#   #                # div(style="display:inline-block;vertical-align:top;",
#   #                #     selectInput(inputId = "variable", 
#   #                #                 label = "Variable",
#   #                #                 choices = vars,
#   #                #                 selected = "cells/L*")))
#   #                # 
#   #                ###### ADD SPECIES HERE???    
#   #                # # Horizontal space
#   #                #  div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
#   #                #  # Trend period radio buttons
#   #                #  div(style="display:inline-block;vertical-align:top;",
#   #                #      radioButtons( "trendperiod", "Trend Period"
#   #                #                    , choiceNames  = list('5 years','10 years')
#   #                #                    , choiceValues = list('5y','10y')
#   #                #                    , selected = list('5y')
#   #                #                    , inline = TRUE)),
#   #                # ),
#   # Application title
#   titlePanel("FWC Harmful Algal Bloom Data"),
#   fluidRow(
#     # Map occupies 1st column
#     column(width = 7, leafletOutput("map", height=750)),
#     # histogram occupies rows in the 2nd column
#     column(width = 5, plotOutput("distPlot"),
#            sliderInput("bins", "Number of bins:", 
#                        min = 1, max = 50, value = 30))
#     
#   )
# )
