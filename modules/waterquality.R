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
      column(width = 6, 
             selectInput(
               inputId = ns("column_selector"),
               label = "Select a Variable of Interest",
               choices = c()
      ), style = "position:relative;z-index:10000;"),
      # style is to make sure the dropdown menu shows over the map zoom tools
      column(width = 6, 
             airDatepickerInput(
               inputId = ns("date_range"),
               label = "Select a Date Range",
               range = TRUE,
               minDate = NULL, #min(date_column),
               maxDate = NULL, #max(date_column),
               dateFormat = "MM/dd/yyyy",
               separator = " - "
      ), style = "position:relative;z-index:10000;")
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
    
    # Note: all the input$... are already reactive values (i.e. they get updated reactively)
    
    #### Update the dropdown menu and available dates after clicking on a station ####
    observeEvent(input$map_marker_click, {
      if(!is.null(input$map_marker_click)){
        # If station changes, update the available variables
        #click <- input$map_marker_click # this is a list with lat, lng, id, group, and layerID
        clicked_id <- input$map_marker_click$id # the id is geometry

        # Store the StationCode in a reactive value
        clicked_station <- reactive({
          WQ_data_locations %>%
            filter(geometry == clicked_id) %>%
            pull(StationCode)
        })

        # Make the (reactive) filtered dataframe
        df_filter <- reactive({
          WQ_df %>%
            filter_dataframe2(filter_station = clicked_station())
          # Here, clicked_station and selected_date_range are also reactive values
        })

        # Update the available variables based on the station clicked (and date range)
        updateSelectInput(
          session, "column_selector",
          choices = sort(colnames(df_filter())[!colnames(df_filter()) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])
        )
        # Update the date range based on the station clicked (and variable)
        #date_column <- df_filter %>%
        #  pull(SampleDate) # Get the date column from the filtered df
        
        updateAirDateInput(session = session, inputId = "date_range", #range = TRUE,
                           options = list(minDate = min(df_filter()$SampleDate),
                                          maxDate = max(df_filter()$SampleDate))
        )
      }
    }, ignoreInit = TRUE)

    #### Update the dropdown menu [and stations??] after selecting a date range ####
    observeEvent(input$date_range, {
      if(!is.null(input$date_range)){

        # Make the (reactive) filtered dataframe
        df_filter <- reactive({
          WQ_df %>%
            filter_dataframe2(date_range = input$date_range)
        })

        # Update the available variables based on the date range selected (and stations)
        updateSelectInput(
          session, "column_selector",
          choices = sort(colnames(df_filter())[!colnames(df_filter()) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])
        )

      }

    }, ignoreInit = TRUE)

    #### Update the date range menu [and stations??] after selecting a variable ####
    observeEvent(input$column_selector, {
      if(!is.null(input$column_selector)){

        # Make the (reactive) filtered dataframe
        df_filter <- reactive({
          WQ_df %>%
            filter_dataframe2(filter_value = input$column_selector)
        })

        # Update the date range based on the variable selected (and stations)
        updateAirDateInput(session = session, inputId = "date_range", #range = TRUE,
                           options = list(minDate = min(df_filter()$SampleDate),
                                          maxDate = max(df_filter()$SampleDate))
        )

      }

    }, ignoreInit = TRUE)

    #### Observe marker clicks and update map when station changes ####
    # Reactive value to store the clicked station's ID
    # clicked_station <- reactiveVal(NULL)
    # popup_visible <- reactiveVal(FALSE) # Right now this doesn't do anything in this code
    
    # observeEvent(
    #   eventExp = { # this is only for updating the map
    #     input$map_marker_click
    #   },
    #   handlerExp = {
    #     click <- input$map_marker_click # this is a list with lat, lng, id, group, and layerID 
    #     # click is already a reactive value (see above)
    #     clicked_id <- click$id # the id is geometry
    #     clicked_lat <- click$lat # save these so the popup can stay visible after clicking
    #     clicked_lng <- click$lng
    #     
    #     clicked_data <- WQ_data_locations %>% # ??? too much filtering?
    #       filter(geometry == clicked_id)
    #     
    #     if (!is.null(click)) {
    #       # Update the clicked marker's color using leafletProxy
    #       # If there was a previously clicked marker, reset its color (i.e. reset all)
    #       leafletProxy("map", session) %>%
    #         addMarkers(
    #           data = WQ_data_locations,
    #           options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
    #           label = labels, # labels appear when hovering
    #           labelOptions = labelOptions(direction = "auto",
    #                                       style = list(
    #                                         "color" = "gray27",
    #                                         "font-size" = "12px",
    #                                         "border-color" = "rgba(0,0,0,0.5)"
    #                                       )
    #           ),
    #           group = "Water Quality Stations",
    #           layerId = ~geometry)
    #       
    #       # Then set the new marker color -> can we do this together with above code?
    #       leafletProxy("map") %>%
    #         addMarkers(lng = input$map_marker_click$lng, lat = input$map_marker_click$lat,
    #                    icon = redIcon, layerId = click$id,
    #                    options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
    #                    label = paste(
    #                      "<strong>Station name:</strong> " , clicked_data$site_friendly, "<br>",
    #                      "<strong>Building:</strong> ", clicked_data$StationCode) %>%
    #                      lapply(htmltools::HTML),
    #                    # Had to play around with labelOptions to kind of get it in the correct place
    #                    labelOptions = labelOptions(direction = "auto", 
    #                                                #offset = c(0, -20),
    #                                                style = list(
    #                                                  "color" = "gray27",
    #                                                  "font-size" = "12px",
    #                                                  "border-color" = "rgba(0,0,0,0.5)"
    #                                                )
    #                    )
    #         )
    #     }
    #   }
    # )
    
    #### Observe marker clicks, date ranges, and variable and update plot when any change ####
    observeEvent(
      eventExpr = { # Put all three events in here: whenever one changes, the plot updates
        input$map_marker_click # when this happens, the reactive value clicked_station is updated
        input$column_selector # when this happens, the reactive value selected_column is updated
        input$date_range # when this happens, the reactive value selected_date_range is updated
      },
      handlerExpr = {
        req(input$map_marker_click)  # Ensure station is selected -> if not, nothing happens
        req(input$column_selector) # Same
        req(input$date_range) # same
        # DOES NOT WORK CURRENTLY: add !is.null?

        #click <- input$map_marker_click # this is a list with lat, lng, id, group, and layerID
        clicked_id <- input$map_marker_click$id # the id is geometry
        
        # Store the StationCode in a reactive value
        clicked_station <- reactive({
          WQ_data_locations %>%
            filter(geometry == clicked_id) %>%
            pull(StationCode)
        })
        
        print(paste("Updating plot for", input$column_selector, "at station", clicked_station(), "for", input$date_range, sep = " "))

        # Make the (reactive) filtered dataframe
        df_filter <- reactive({
          WQ_df %>%
            filter_dataframe2(filter_value = input$column_selector,
                              date_range = input$date_range,
                              filter_station = clicked_station())
          # Here, clicked_station and column_selector are also reactive values
        })
        output$plot <- renderPlotly({
          if (input$column_selector %in% names(df_filter())) { # only plot if variable exists for that station ALSO ADD DATE RANGE CHECK
            create_plot(df = df_filter(), units_df = WQ_data_units, loc_name = unique(df_filter()$site_friendly), selected_column = input$column_selector)
          } else {
            plot_ly(type = 'scatter', mode = 'markers') %>%
              layout(title = paste0("No data available on ", input$column_selector, "<br>at ", clicked_station()),
                     xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
          }
        })
      },
      ignoreInit = FALSE)

    #### The 'go back' button ####
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}
