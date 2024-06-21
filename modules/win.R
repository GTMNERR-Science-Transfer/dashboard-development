# import data for this page
load("./03_Data_for_app/WIN.RData")

#### Process data further ####
# make datframe for map display and hover data
WIN_data_locations = WIN_df %>%
  filter(variable %in% c("geometry", 
                       "HUC12.Name", 
                       "Start.Date",
                       "latitude",
                       "longitude")
         ) %>%
  select(c(RowID, variable, value)) %>%
  distinct(RowID, variable, value) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA)
  ) %>%
  distinct(geometry, HUC12.Name, Start.Date, latitude, longitude)%>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

#### Run the app #### 

find_directory_of_file("app.R")

WINPageUI <- function(id) {
  ns <- NS(id) # This is an important part to add to all sub pages so they use the
  # correct sessions / ID's that connect the ui and server here
  tagList(
    h2("Water Infrastructure Network"),
    fluidRow(
      # Map occupies 1st column
      column(width = 7, leafletOutput(ns("map"), height="500px")), # make sure to put the input inside ns()
      # histogram occupies rows in the 2nd column
      column(width = 5, plotOutput(ns("plot"))),
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page")
  )
}

WINPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is 
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns
    
    # Reactive value to track popup bubble state
    popup_visible <- reactiveVal(FALSE)
    
    # Default plot
    output$plot <- renderPlot({
      plot(1:10, 1:10, main = "Default Plot")
    })

    # Create the map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
        clearBounds() %>% # centers map on all min/max coords
        # Base map
        addTiles() %>% # Add default OpenStreetMap map tiles
        # Polygons, add groups
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
          data = WIN_data_locations,
          popup = ~ paste("Station Name: ", HUC12.Name, "<br>",
                          "Start Date: ", Start.Date, "<br>"
                          ),
          group = "WIN",
          layerId = ~geometry
        )  %>%
        # Layers control (turning layers on and off)
        addLayersControl(
          overlayGroups = c("Counties", "GTMNERR boundaries", "WIN"), # 
          options = layersControlOptions(collapsed = FALSE)
        )  %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })
    
    # Observe marker clicks
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click)) {
        clicked_id <- click$id
        clicked_data <- WIN_data_locations %>%
          filter(geometry == clicked_id)
        
        popup_visible(TRUE)
        
        output$plot <- renderPlot({
          plot(31:35, 11:15, main = paste("Plot for Station:", clicked_data$HUC12.Name))
          # Replace with actual plot code using clicked_data
        })
      }
    })
    
    # Add observeEvent function for input$map_shape_click
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (!is.null(click)) {
        clicked_id <- click$id
        clicked_data <- counties_select %>%
          filter(NAME == clicked_id)
        
        popup_visible(TRUE)
        
        output$plot <- renderPlot({
          plot(1:100, 21:120, main = paste("Plot for County:", clicked_data$NAME))
          # Replace with actual plot code using clicked_data
        })
      }
    })
    
    # Observe map clicks to hide the popup
    observeEvent(input$map_click, {
      popup_visible(FALSE)
      
      output$plot <- renderPlot({
        plot(1:10, 1:10, main = "Default Plot")
      })
    })

    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}