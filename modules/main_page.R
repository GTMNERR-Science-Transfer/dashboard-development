########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# Created June 2024
# Last updated: 12 August 2024

# This page shows is the main Welcome page


### Define the UI -------------------------------------------------------------

mainPageUI <- function(id) {
  ns <- NS(id) # This is an important part to add to all subpages so they use the
  # correct sessions / ID's that connect the ui and server here
  tagList(
    h2("Welcome!"),
    p(htmltools::HTML("This is the main page of the Guana Estuary Data Dashboard. <br>
    The map below lets you explore characteristics of the area by turning
    different map layers on and off. <br>
    <br>
    There are various data sets available through this dashboard. You can explore the 
    availability of data and their locations under the tab 'Explore'. You can 
    visualize, filter, and download data for specific topics under their respective tabs")),
    
    # Add GTM logo
    fluidRow(
      column(width = 8, h2("Welcome to the Main Page"))
    )
  )
}

### Define the server logic ----------------------------------------------------

mainPageServer <- function(input, output, session) {
  #ns <- session$ns
  # NUTTIN'  
}

