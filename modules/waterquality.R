########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# Email: gklarenberg@ufl.edu
# Christopher Marais
# Email: 
# Last updated: 12 August 2024

library(tidyverse)

# import all WQ data for this page (WIN and Guana spreadsheet)
WQ_df <- readRDS("./03_Data_for_app/WQ_all.Rds")

# Location data
WQ_data_locations <- readRDS("./03_Data_for_app/WQ_data_locations.Rds")

WQ_data_units <- readRDS("./03_Data_for_app/WQ_data_units.Rds")

#### Run the app #### 
find_directory_of_file("app.R")

# Updated UI in WINPageUI
WINPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Water Quality Data"),
    
    fluidRow(
      column(width = 12,
             div(style = "margin-bottom: 20px;",
                 p("This section provides an overview of water quality data. 
                   You can select a water quality variable of interest by using
                   the dropdown menu. Click on a marker to see a graph of
                   all available data for that specific location")
             )
      )
    ),
    fluidRow(
      column(width = 6, uiOutput(ns("dropdown_ui")), style = "position:relative;z-index:10000;"),
      # style is to make sure the dropdown menu shows over the map zoom tools
      column(width = 6, uiOutput(ns("date_range")), style = "position:relative;z-index:10000;")
      # style is to make sure the date selection tool shows over the map zoom tools
    ),
    fluidRow(
      column(width = 8, 
             div(style = "margin-bottom: 20px;",
                 leafletOutput(ns("map"), height="500px") 
             )
      ),
      column(width = 4, 
             div(style = "padding: 20px; background-color: #f9f9f9;",
                 h3("About [variable] and [station]"),
                 p("Right now this is just some placeholder text. This will be 
                   updated to dynamically show information about the station and
                   the variable that the user has selected.")
             )
      )
    ),
    fluidRow(
      column(width = 12, 
             div(style = "margin-bottom: 20px;",
                 plotlyOutput(ns("plot"), height="350px")
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

# Updated Server logic in WINPageServer
WINPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #### Create the map ####
    
    labels <- paste(
      "<strong>Station name:</strong> " , WQ_data_locations$site_friendly, "<br>",
      "<strong>Building:</strong> ", WQ_data_locations$StationCode) %>%
      lapply(htmltools::HTML)
    
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18, scrollWheelZoom = FALSE)) %>% # turn off scroll wheel for now
        clearBounds() %>%
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
    
    #### Create the dropdown UI #####
    output$dropdown_ui <- renderUI({
      print("Rendering dropdown UI")
      df <- filter_dataframe(WQ_df) # we will have to make this reactive??
      create_dropdown(df, ns)
    })
    
    #### Create the date input UI #####
    output$date_range <- renderUI({
      print("Picking date range")
      df <- filter_dataframe(WQ_df) # we will have to make this reactive??
      create_date(df, ns)
    })
    
    #### Observe marker clicks and update plot when station changes ####
    # Reactive value to store the clicked station's ID
    clicked_station <- reactiveVal(NULL)
    popup_visible <- reactiveVal(FALSE) # Right now this doesn't do anything in this code
    
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click # this is a list with lat, lng, id, group, and layerID 
      if (!is.null(click)) {
        
        # Update the clicked marker's color using leafletProxy
        # If there was a previously clicked marker, reset its color
        leafletProxy("map", session) %>%
          addMarkers(
            data = WQ_data_locations,
            # popup = ~ paste("Station name: ", site_friendly, "<br>", 
            #                 "Station code: ", StationCode, "<br>"
            #                 ),
            options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
            label = labels,
              # ~ paste("Station name: ", site_friendly, "<br>", 
              #               "Station code: ", StationCode, "<br>"
            #), # labels appear when hovering
            labelOptions = labelOptions(direction = "auto",
                                        style = list(
                                          "color" = "gray27",
                                          "font-size" = "12px",
                                          "border-color" = "rgba(0,0,0,0.5)"
                                        )
            ),
            group = "Water Quality Stations",
            layerId = ~geometry)
        
        clicked_id <- click$id # the id is geometry
        clicked_lat <- click$lat # save these so the popup can stay visible after clicking
        clicked_lng <- click$lng
        
        clicked_data <- WQ_data_locations %>%
          filter(geometry == clicked_id) 
        
        #popup_visible(TRUE)
        
        # Store the StationCode in a reactive value
        clicked_station(clicked_data$StationCode)
        
        df <- filter_dataframe(WQ_df, filter_value = clicked_station(), date_range = selected_date_range()) 
        
        output$plot <- renderPlotly({
          if (selected_column() %in% names(df)) { # only plot if variable exists for that station
            create_plot(df, units_df = WQ_data_units, loc_name = unique(df$site_friendly), selected_column = selected_column())
          } else {
            plot_ly(type = 'scatter', mode = 'markers') %>%
              layout(title = paste0("No data available on ", selected_column(), "<br>at ", clicked_station()), 
                     xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
          }
        })
        
        # Make the popup pop up again
        # isolate({
        #   leafletProxy("map") %>%
        #     addPopups(lng = clicked_lng, lat = clicked_lat,
        #               popup = paste("Station name: ", clicked_data$site_friendly, "<br>",
        #                             "Station code: ", clicked_data$StationCode, "<br>"),
        #               options = popupOptions(offset = c(0, 12)))  # Adjust the offset as needed
        # })
        # Set the new marker color
        leafletProxy("map") %>%
          #clearPopups() %>%
          addMarkers(lng = input$map_marker_click$lng, lat = input$map_marker_click$lat,
                     icon = redIcon, layerId = click$id,
                     options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
                     # popup = paste("Station name: ", clicked_data$site_friendly, "<br>",
                     #                "Station code: ", clicked_data$StationCode, "<br>"),
                     # options = list(popupOptions(closeButton = TRUE, autoClose = FALSE),
                     #                markerOptions(riseOnHover = TRUE)), # Brings marker forward when hovering
                     label = paste(
                       "<strong>Station name:</strong> " , clicked_data$site_friendly, "<br>",
                       "<strong>Building:</strong> ", clicked_data$StationCode) %>%
                       lapply(htmltools::HTML),
                     labelOptions = labelOptions(direction = "auto",
                                                 #offset = c(0, -20),
                                                 style = list(
                                                   "color" = "gray27",
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)"
                                                 )
                     )
          )
      }
      
    })
    
    #### Update plot when dropdown selection and/or date changes ####
    # Reactive expression that can read reactive values and call other reactive
    # expressions -> whenever a reactive value changes, the reactive expression will
    # re-execute
    # (incorrect explanation?: Reactive value for the selected column)
    selected_column <- reactive({ # So I can also put this in the function directly??
      print(paste("Selected column changed to:", input$column_selector))
      input$column_selector
    })
    
    selected_date_range <- reactive({
      print(paste("Selected date range:", input$date_range))
      input$date_range
    })
    
    observeEvent(selected_column(), {
      req(clicked_station())  # Ensure station is selected -> if not, nothing happens
      req(selected_date_range()) # instead of req(), add this to observeEvent also
      
      print(paste("Updating plot for", selected_column(), "at station", clicked_station(), "for", date_range[1], " to ", date_range[2]))
      
      df <- filter_dataframe(WQ_df, filter_value = clicked_station(), date_range = selected_date_range())
      
      output$plot <- renderPlotly({ # also add the date range to the if statemenent?
        if (selected_column() %in% names(df)) { # only plot if variable exists for that station
          create_plot(df, units_df = WQ_data_units, loc_name = unique(df$site_friendly), selected_column = selected_column())
        } else {
          plot_ly(type = 'scatter', mode = 'markers') %>%
            layout(title = paste0("No data available on ", selected_column(), "<br>at ", clicked_station()), 
                   xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
        }
      })
    }, ignoreInit = FALSE) 
    
    # Add observeEvent function for input$map_shape_click
    # observeEvent(input$map_shape_click, {
    #   click <- input$map_shape_click
    #   if (!is.null(click)) {
    #     clicked_id <- click$id
    #     clicked_data <- counties_select %>%
    #       filter(NAME == clicked_id)
    #     
    #     popup_visible(TRUE)
    #     
    #     output$plot <- renderPlotly({
    #       df <- filter_dataframe(WQ_df, filter_value = clicked_data$NAME)
    #       create_plot(df, units_df = WQ_data_units, loc_name = clicked_data$NAME, selected_column = selected_column())
    #     })
    #   }
    # })
    
    # Observe map clicks to hide the popup
    # observeEvent(input$map_click, {
    #   popup_visible(FALSE)
    #   
    #   # Default plot
    #   output$plot <- renderPlotly({
    #     df <- filter_dataframe(WQ_df)
    #     create_plot(df, units_df = WQ_data_units, selected_column = selected_column())
    #   })
    # })
    
    #### Add 'go back' button ####
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}
