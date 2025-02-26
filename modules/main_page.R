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
  ns <- NS(id)
  tagList(
    # Header with logos and title
    fluidRow(
      column(width = 3, align = "center",
             tags$img(src = "data/gtm_logo.png", height = "150px")
      ),
      column(width = 6, align = "center",
             h1("Guana Estuary Data Dashboard", 
                style = "font-size: 3em; text-shadow: 2px 2px 4px rgba(0,0,0,0.3); margin-top: 20px; margin-bottom: 20px;")
      ),
      column(width = 3, align = "center",
             tags$img(src = "data/noaa_logo.png", height = "150px")
      )
    ),
    hr(),
    # About the Project Section
    h2("About the Project"),
    p("The Guana Estuary Data Dashboard is an interactive, public-facing platform developed as part of a collaborative open science initiative. Its goal is to provide comprehensive access to a diverse array of ecological datasets from the Guana River Estuary – including water quality, harmful algal blooms, and other environmental indicators. By integrating long-term monitoring data with modern visualization tools, the dashboard supports scientists, students, and community members in exploring, analyzing, and downloading valuable data for research and decision making."),
    p("The project was conceived to address the need for improved data accessibility and community engagement with the estuary’s ecology. The dashboard makes complex environmental data both accessible and actionable, ultimately supporting informed coastal management and conservation efforts."),
    hr(),
    # How to Use the Platform Section
    h2("How to Use the Platform"),
    p("The dashboard is organized into several interactive tabs. Here’s a quick guide to help you navigate each section:"),
    tags$ul(
      h3("Explore:"), 
      tags$p("• ", tags$strong("View Data Availability:"), " Check which stations have available data for various datasets."),
      tags$p("• ", tags$strong("Select Data Type:"), " Use the dropdown menu to pick a specific type of data to visualize."),
      tags$p("• ", tags$strong("Interactive Map:"), " Click on map markers to see the time periods and locations with data."),
      h3("Water Quality:"), 
      tags$p("• ", tags$strong("Filter Data:"), " Select one or more stations, set a date range, and choose a water quality variable to visualize."),
      tags$p("• ", tags$strong("Dynamic Plots:"), " View interactive time series charts that update based on your filters."),
      tags$p("• ", tags$strong("Download Option:"), " Easily download the filtered dataset as a CSV file."),
      h3("Harmful Algal Blooms:"), 
      tags$p("• ", tags$strong("Set Filters:"), " Choose a station, adjust the date range, and select the algae type(s) of interest."),
      tags$p("• ", tags$strong("Interactive Visualization:"), " Explore data on an interactive map and view detailed summary tables."),
      tags$p("• ", tags$strong("Export Data:"), " Export the summary tables in multiple formats (CSV, Excel, PDF) using built-in buttons."),
      h3("Water Levels:"), 
      tags$p("•", tags$strong("Under Construction:"), "Additional ecological insights will be provided soon."),
      h3("Fish and Shellfish:"), 
      tags$p("•", tags$strong("Under Construction:"), "Additional ecological insights will be provided soon.")
    ),
    p(tags$strong("Interactive Features:")),
    tags$ul(
      tags$li("Click on map markers to select or deselect stations; your choices automatically update the plots."),
      tags$li("Hover over Plotly charts for detailed data points and use the toolbar (zoom, pan, reset, download) located in the upper right corner to further explore and save visualizations.")
    ),
    hr(),
    # Footer Section
    fluidRow(
      column(width = 12, align = "center",
             tags$footer("© 2025 Guana Estuary Data Dashboard. All rights reserved.",
                         style = "padding: 10px; background-color: #f0f0f0; border-top: 1px solid #d0d0d0; font-size: 0.9em;")
      )
    )
  )
}

### Define the server logic ----------------------------------------------------

mainPageServer <- function(input, output, session) {
  #ns <- session$ns
  # NUTTIN'  
}

