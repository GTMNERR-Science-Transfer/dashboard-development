########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# Email: gklarenberg@ufl.edu
# Christopher Marais
# Email: 
# Last updated: 12 August 2024

library(tidyverse)
library(shinyWidgets) #not sure if this is necessary? Loaded in global?

# import all WQ data for this page (WIN and Guana spreadsheet)
WQ_df <- readRDS("./03_Data_for_app/WQ_all.Rds")

# Location data
WQ_data_locations <- readRDS("./03_Data_for_app/WQ_data_locations.Rds") %>% 
  arrange(site_friendly) # Am straightaway ordering site names alphabetically so
# they show up alphabetically in the dropdown menu

WQ_data_units <- readRDS("./03_Data_for_app/WQ_data_units.Rds")

#### Run the app #### 
find_directory_of_file("app.R")

# Updated UI in WINPageUI
WINPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Water Quality Data"),
    
    fluidRow(
      # First row - explanation
      column(width = 12,
             div(style = "margin-bottom: 20px;",
                 p(htmltools::HTML('This section provides an overview of water quality data. 
                   You can select one or more stations from the list, or by
                   clicking on stations on the map. <br>
                   You can specify the time range of interest, and the dropdown 
                   menu lets you pick a water quality variable. <br>
                   Clicking "Create plot!" will render a time series plot for 
                   the available data. <br>
                   <br> <strong>NOTE:</strong> The plot only shows stations for
                   which data for the variable are available. Currently you do not
                   (yet) get a warning if data are unavailable for a station - unless
                   the data are unavailable for all selected stations.'))
             )
      )
    ),
    fluidRow(
      # Second row - inputs and map
      # Column 1: to define inputs
      column(width = 7,
             # 1. Stations
             fluidRow(
               column(width = 12,
                      multiInput(
                        inputId = ns("station_list"),
                        label = "Choose station(s) of interest:", 
                        choices = NULL,
                        choiceNames = paste0(WQ_data_locations$site_friendly, " (", WQ_data_locations$StationCode, ")"),
                        width = "100%",
                        options = list(
                          non_selected_header = "Choose between:",
                          selected_header = "You have selected:"
                        ),
                        choiceValues = WQ_data_locations$StationCode
                      )
               ),
             ),
             # 2. Date range
             fluidRow(
               column(width = 12, 
                      sliderInput(
                        inputId = ns("date_range"),
                        label = "Select a Date Range",
                        min = ymd(paste0(min(WQ_data_locations$minYear), "-01-01")), #NULL
                        max = ymd(paste0(max(WQ_data_locations$maxYear), "-12-31")), #NULL
                        value = c(ymd(paste0(min(WQ_data_locations$minYear), "-01-01")), 
                                  ymd(paste0(max(WQ_data_locations$maxYear), "-12-31"))),
                        timeFormat = "%m/%d/%Y",
                        width = "100%"
                      ), style = "position:relative;z-index:10000;" # style is to make sure the dropdown menu shows over the map zoom tools
               )
             ),
             # 3. Variables
             fluidRow(
               column(width = 12,
                      selectInput(
                        inputId = ns("column_selector"),
                        label = "Select a variable of interest",
                        choices = unique(filter(WQ_data_units, !is.na(ComponentLong)) %>% pull (ComponentLong))
                      ), style = "position:relative;z-index:10000;"
               )
             ),
             fluidRow(
               column(width = 12,
                      actionBttn( # function from shinyWidgets
                        inputId = ns("make_plot"),
                        label = "Create plot!", 
                        style = "fill",
                        color = "warning",
                        icon = icon("arrow-trend-up", library = "fa"),
                        block = TRUE # full-width button
                      )
               )
             )
      ),
      # Column 2: show map
      column(width = 5, 
             div(style = "margin-bottom: 20px;",
                 leafletOutput(ns("map"), height="500px"))
      )
    ),
    # Third row - plot
    fluidRow(
      column(width = 12, 
             div(style = "margin-bottom: 20px;",
                 plotlyOutput(ns("plot"), height="350px")
             )
      )
    ),
    # Fourth row - more info
    fluidRow(
      column(width = 12, 
             div(style = "padding: 20px; background-color: #f9f9f9;",
                 h3("About [variable] and [station]"),
                 p("Right now this is just some placeholder text. This will be 
                   updated to dynamically show information about the station and
                   the variable that the user has selected.")
             )
      )
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page")
  )
}

# Define custom icons -> move this to a separate script
redIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41,
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
  shadowWidth = 41, shadowHeight = 41,
  shadowAnchorX = 12, shadowAnchorY = 41
)

#### LIST OF INPUTS ####
# input$dropdown_ui -> to get variable
# input$date_range -> to get date range
# input$go_back -> to go back to main page


# Updated Server logic in WINPageServer
WINPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #### Create the map ####
    
    labels <- paste(
      "<strong>Station name:</strong> " , WQ_data_locations$site_friendly, "<br>",
      "<strong>Station:</strong> ", WQ_data_locations$StationCode) %>%
      lapply(htmltools::HTML)
    
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18, scrollWheelZoom = TRUE)) %>% # turn on scroll wheel for now
        #clearBounds() %>% # This makes the view default to the largest map object
        setView(lng=-81.347388, lat=30.075, zoom = 10) %>% # focus on the center of Guana River
        addTiles() %>%
        addPolygons(
          data = GTMNERR, color = "purple", fill = NA,
          weight = 2, opacity = 1, group = "GTMNERR boundaries"
        ) %>%
        addPolygons(
          data = counties_select,
          color = "black", weight = 2, opacity = 1,
          fill = TRUE, fillColor = "white", fillOpacity = 0.01,
          highlightOptions = highlightOptions(
            color = "white", weight = 2,
            bringToFront = TRUE
          ),
          group = "Counties", popup = ~NAME, layerId = ~NAME
        ) %>%
        addMarkers(
          data = WQ_data_locations,
          options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
          label = labels, # labels appear when hovering
          labelOptions = labelOptions(direction = "auto",
                                      style = list(
                                        "color" = "gray27",
                                        "font-size" = "12px",
                                        "border-color" = "rgba(0,0,0,0.5)"
                                      )
          ),
          group = "Water Quality Stations",
          layerId = ~geometry 
        ) %>%
        addLayersControl(
          overlayGroups = c("Counties", "GTMNERR boundaries", "Water Quality Stations"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles") 
    })
    
    #### Render an empty plot initially ####
    output$plot <- renderPlotly({
      plot_ly(type = 'scatter', mode = 'markers') %>%
        layout(title = "No data selected", xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    })
    
    # Note: all the input$... are already reactive values (i.e. they get updated reactively)
    
    # Reactive to keep track of selected stations from both inputs
    selected_stations <- reactiveVal(character())
    
    #### Observe map click and update list with selected_stations ####
    # The color of the markers is changed in a different observeEvent (that checks
    # if the station list has been updated)
    observeEvent({
      input$map_marker_click
    }, {
      # Initialize a vector to store / or get vector with currently selected stations
      current_selected_stations <- selected_stations()
      
      # Check if there is a map click and get the station code
      if(!is.null(input$map_marker_click)){
        clicked_station <- WQ_data_locations %>%
          filter(geometry == req(input$map_marker_click$id)) %>%
          pull(StationCode)
        print(paste("map click detected", clicked_station))
        
        # Toggle clicked_station: add if not in list, remove if already selected
        if (!is.null(clicked_station)) {
          print("Yes you clicked a station!")
          if (clicked_station %in% current_selected_stations) {
            # Remove station if it's already selected
            # setdiff returns elements that are in current_selected_stations that are not in clicked_station
            # The version used here is dplyr, but it works the same as the base R function
            current_selected_stations <- setdiff(current_selected_stations, clicked_station)
            print(paste("Station removed from list with map click, which now contains", paste(unique(current_selected_stations), collapse = ", ")))
          } else {
            # Add station if not already selected
            current_selected_stations <- unique(c(current_selected_stations, clicked_station))
            print(paste("Station added to list with map click, which now contains", paste(unique(current_selected_stations), collapse = ", ")))
          }
        }
        
        # Update selected_stations reactive value
        selected_stations(current_selected_stations)
        
        # Update the multiInput choices without triggering observeEvent
        isolate({
          updateMultiInput(session, "station_list", selected = current_selected_stations)
        })
      }
    }, ignoreInit = TRUE)
    
    observe({
      if (is.null(input$station_list)) {
        selected_stations(character())  # Clear the reactive value
        print("All stations deselected.")
        
        # Update all map markers to blue again
        leafletProxy("map") %>%
          addMarkers(
            data = WQ_data_locations,
            options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
            label = labels, # labels appear when hovering
            labelOptions = labelOptions(direction = "auto",
                                        style = list(
                                          "color" = "gray27",
                                          "font-size" = "12px",
                                          "border-color" = "rgba(0,0,0,0.5)"
                                        )
            ),
            group = "Water Quality Stations",
            layerId = ~geometry)
      }
    })
    
    #### Observe list selection and update selected_stations ####
    observeEvent({
      input$station_list
    }, {
      # Retrieve current selections
      current_selected_stations <- selected_stations()
        
      # Add all selected stations from the dropdown
      new_selections <- setdiff(input$station_list, current_selected_stations)
      current_selected_stations <- unique(c(current_selected_stations, new_selections))
        
      # Remove stations no longer in dropdown selection
      stations_to_remove <- setdiff(current_selected_stations, input$station_list)
      current_selected_stations <- setdiff(current_selected_stations, stations_to_remove)
        
      # Update selected_stations reactive value
      selected_stations(current_selected_stations)
        
    }, ignoreInit = TRUE)
    
    #### Update marker colors based on the list and map clicks ####
    observeEvent({
      input$station_list
    },{
      # Update marker colors
      # Get all selected stations
      selected_coords <- reactive({
        WQ_data_locations %>% 
          filter(StationCode %in% selected_stations())
      })
      # Get all non-selected stations
      unselected_coords <- reactive({
        WQ_data_locations %>% 
          filter(!(StationCode %in% selected_stations()))
      })
      
      leafletProxy("map") %>%
        # First remove original markers (otherwise they just keep plotting over each other)
        clearMarkers() %>% 
        # Make / keep unselected stations blue
        addMarkers(lng = unselected_coords()$Longitude, lat = unselected_coords()$Latitude,
                   layerId = unselected_coords()$geometry,
                   options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
                   label = paste(
                     "<strong>Station name:</strong> " , unselected_coords()$site_friendly, "<br>",
                     "<strong>Station:</strong> ", unselected_coords()$StationCode) %>%
                     lapply(htmltools::HTML),
                   # Had to play around with labelOptions to kind of get it in the correct place
                   labelOptions = labelOptions(direction = "auto",
                                               #offset = c(0, -20),
                                               style = list(
                                                 "color" = "gray27",
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )
                   )
        ) %>% 
        # Make selected stations red
        addMarkers(lng = selected_coords()$Longitude, lat = selected_coords()$Latitude,
                   icon = redIcon, 
                   layerId = selected_coords()$geometry,
                   options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
                   label = paste(
                     "<strong>Station name:</strong> " , selected_coords()$site_friendly, "<br>",
                     "<strong>Station:</strong> ", selected_coords()$StationCode) %>%
                     lapply(htmltools::HTML),
                   # Had to play around with labelOptions to kind of get it in the correct place
                   labelOptions = labelOptions(direction = "auto",
                                               #offset = c(0, -20),
                                               style = list(
                                                 "color" = "gray27",
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )
                   )
        )
    })
      
    df_filter <- reactiveVal() # Create as a global variable so it is available for plotting
    selected_col <- reactiveVal() # Same
    
    observeEvent(input$make_plot, { # When user clicks action button: update df_filter
      req(selected_stations(), input$column_selector, input$date_range) # make sure all 3 exist
      
      print(paste("Initiating filter dataframe for", input$column_selector, 
                  "at station", paste(unique(selected_stations()), collapse = ", "), 
                  "for", input$date_range[1], "to", input$date_range[2], sep = " "))
      
      # Make the (reactive) filtered dataframe -> also only changes when button is pressed
      df_filter(WQ_df %>%
                  filter_dataframe2(filter_value = input$column_selector,
                                    date_range = input$date_range,
                                    filter_station = selected_stations())
      )
      
      print(paste("CHECK: the filtered dataframe contains data for the stations:", paste(unique(df_filter()$StationCode), collapse = ", ")))
      
      # Make reactive input value
      selected_col(input$column_selector)
      
      }, ignoreInit = TRUE)
    
    # Create plot -> will also only run when button is pressed because it relies on
    # df_filter()
    output$plot <- renderPlotly({
      
      if (is.null(df_filter()) || nrow(df_filter()) == 0) {
        # Render empty plot
        return(plot_ly(type = 'scatter', mode = 'markers') %>%
                 layout(title = "No data selected", 
                        xaxis = list(visible = FALSE), 
                        yaxis = list(visible = FALSE)))
      }

      req(df_filter(), selected_col(), selected_stations())
      
      # Create the plot (has a built in method to show a message if there is no data available)
      create_plot(df = df_filter(), 
                  units_df = WQ_data_units, 
                  selected_column = selected_col())
    })
    
    #### The 'go back' button ####
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}
