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

#### Process data further ####
# MOVE THIS TO THE CLEANING SCRIPT!!
# make datframe for map display and hover data
WQ_data_locations = WQ_df %>% # site friendly and station code are the names in common
  filter(variable %in% c("StationCode", "site_friendly",
                         "geometry", 
                      #"HUC12Name", # Changed for now (bc GTM WQ data does not have that variable) BUT we should also include a station name of some sort
                       "SampleDate", # changed, from StartDate - but also not necessary for locations
                       "Latitude",
                       "Longitude",
                       "data_source")
         ) %>%
  #select(c(RowID, variable, value)) %>% # not necessary
  #distinct(RowID, variable, value) %>% # not necessary
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA)
  ) %>%
  distinct(Latitude, Longitude, data_source, geometry, StationCode, site_friendly) %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

WQ_data_units = WQ_df %>%
  filter(variable %in% c("ComponentLong", # some will be duplicated because they differ between the 2 sets
                         "Unit", "data_source")
  ) %>%
  #select(c(RowID, variable, value)) %>%
  distinct(RowID, variable, value) %>% # strictly speaking also not necessary
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA),
    values_fn = list(value = ~ first(.))
  ) %>%
  distinct(ComponentLong, Unit, data_source)

#### Functions ####
# make dataframe for click plots - filter for correct stations
filter_dataframe <- function(df, filter_value = NULL) {
  if (!is.null(filter_value)) {
    # Step 1: Identify the relevant RowIDs
    relevant_row_ids <- df %>%
      filter(value == filter_value) %>%
      pull(RowID)
    
    # Step 2: Filter the entire dataframe to keep only rows with the relevant RowIDs
    filtered_df <- df %>%
      filter(RowID %in% relevant_row_ids)
  } else {
    # If no filter_value is provided, skip the filtering step
    filtered_df <- df
  }
  
  # Step 3: Create wide dataframe
  wide_df <- filtered_df %>%
    pivot_wider(names_from = variable, values_from = value, values_fn = first) %>%
    filter(!is.na(ComponentLong)) %>% # stop gap measure because there are NAs from
    # replacing ComponentLong names in the cleaning script
    select(SampleDate, # we could also make these arguments for the function?
           ComponentLong, 
           Result,
           geometry, StationCode, site_friendly) %>%
    pivot_wider(names_from = ComponentLong, 
                values_from = Result,
                values_fn = list(Result = ~ mean(as.numeric(.), na.rm = TRUE))) %>%
    mutate(SampleDate = ymd_hms(SampleDate)) %>% # 
    #mutate(SampleDate = str_extract(SampleDate, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% 
    #mutate(SampleDate = ymd(SampleDate)) %>% # these two lines are another option to only get ymd
    mutate(across(-c(SampleDate, geometry, StationCode, site_friendly), ~ as.numeric(.))) %>%
    mutate(SampleDate = as.Date(SampleDate)) %>%
    group_by(SampleDate, geometry, StationCode, site_friendly) %>%
    summarize(across(everything(), ~mean(.x, na.rm = TRUE))) #%>% # across(everything()) is not necessary,
  # strictly speaking, but it's nice to keep for if we ever want to adjust this function
  # to work for more than 1 variable
    #select(where(~ n_distinct(.) > 2))
  
  return(wide_df)
}

# New function to create the dropdown
create_dropdown <- function(df, ns) {
  # Get the column names except the dates and column names and geometry
  column_names <- sort(colnames(df)[!colnames(df) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])
  print(paste("Creating dropdown with choices:", paste(column_names, collapse=", ")))
  
  selectInput(
    inputId = ns("column_selector"),
    label = "Select a Variable of Interest",
    choices = column_names,
    selected = column_names[1]
  )
}

# Modified create_plot function
create_plot <- function(df, units_df, loc_name = NULL, selected_column) {
  print(paste("Creating plot for", selected_column, "at", loc_name))
  # Initialize the plot with the x-axis
  fig <- plot_ly(df, x = ~SampleDate)
  
  # Get the column names except the dates and column names and geometry
  column_names <- sort(colnames(df)[!colnames(df) %in% c("SampleDate", "geometry", "StationCode", "site_friendly")])
  
  # Create a named vector for Y-axis titles
  y_axis_titles <- setNames(units_df$Unit, units_df$ComponentLong)
  
  # Ensure selected_column is not NULL or empty
  # if (is.null(selected_column) || selected_column == "") {
  #   selected_column <- column_names[1]
  # }
  
  # Loop through each column and add a trace
  for (i in seq_along(column_names)) {
    fig <- fig %>%
      add_trace(y = df[[column_names[i]]], name = column_names[i], type = 'scatter', mode = 'lines+markers',
                showlegend = FALSE,
                visible = if (column_names[i] == selected_column) TRUE else FALSE)
  }
  
  station_name <- unique(df$StationCode)
  
  # Customize the layout
  fig <- fig %>%
    layout(xaxis = list(title = 'Date'),
           yaxis = list(title = y_axis_titles[selected_column]),
           title = list(text = paste0("Mean daily ", selected_column, "<br>for ", loc_name, " (station code: ", station_name, ")"), 
                        y = 0.90), 
           margin = list(t = 60),
           plot_bgcolor = '#e5ecf6',
           xaxis = list(zerolinecolor = 'black',
                        zerolinewidth = 2,
                        gridcolor = 'azure1'),
           yaxis = list(zerolinecolor = 'black',
                        zerolinewidth = 2,
                        gridcolor = 'azure1'))
  
  return(fig)
}


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
      column(width = 12, uiOutput(ns("dropdown_ui")), style = "position:relative;z-index:10000;")
      # style is to make sure the dropdown menu shows over the map zoom tools
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
          group = "WQ",
          layerId = ~geometry 
        ) %>%
        addLayersControl(
          overlayGroups = c("Counties", "GTMNERR boundaries", "WQ"),
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
      df <- filter_dataframe(WQ_df)
      create_dropdown(df, ns)
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
            group = "WQ",
            layerId = ~geometry)
        
        clicked_id <- click$id # the id is geometry
        clicked_lat <- click$lat # save these so the popup can stay visible after clicking
        clicked_lng <- click$lng
        
        clicked_data <- WQ_data_locations %>%
          filter(geometry == clicked_id) 
        
        #popup_visible(TRUE)
        
        # Store the StationCode in a reactive value
        clicked_station(clicked_data$StationCode)
        
        df <- filter_dataframe(WQ_df, filter_value = clicked_station()) 
        
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
          clearPopups() %>%
          addMarkers(lng = input$map_marker_click$lng, lat = input$map_marker_click$lat,
                     icon = redIcon, layerId = click$id,
                     options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
                     # popup = paste("Station name: ", clicked_data$site_friendly, "<br>",
                     #                "Station code: ", clicked_data$StationCode, "<br>"),
                     # options = list(popupOptions(closeButton = TRUE, autoClose = FALSE),
                     #                markerOptions(riseOnHover = TRUE)), # Brings marker forward when hovering
                     label = labels,
                       # paste("Station name: ", clicked_data$site_friendly, "<br>",
                       #             "Station code: ", clicked_data$StationCode, "<br>"), # labels appear when hovering
                     labelOptions = labelOptions(direction = "auto",
                                                 style = list(
                                                   "color" = "gray27",
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)"
                                                 )
                     )
          )
      }
      
    })
    
    #### Update plot when dropdown selection changes ####
    # Reactive value for the selected column
    selected_column <- reactive({
      print(paste("Selected column changed to:", input$column_selector))
      input$column_selector
    })
    
    observeEvent(selected_column(), {
      req(clicked_station())  # Ensure station is selected
      
      print(paste("Updating plot for", selected_column(), "at station", clicked_station()))
      
      df <- filter_dataframe(WQ_df, filter_value = clicked_station())
      
      output$plot <- renderPlotly({
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
