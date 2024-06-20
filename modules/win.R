# import data for this page
load("./03_Data_for_app/WIN.RData")
WIN_data_locations <- WIN_df 

#### Process data further ####


#### Run the app #### 

find_directory_of_file("app.R")

WINPageUI <- function(id) {
  ns <- NS(id) # This is an important part to add to all sub pages so they use the
  # correct sessions / ID's that connect the ui and server here
  tagList(
    h2("Water Infrastructure Network"),
    fluidRow(
      # Map occupies 1st column
      column(width = 7, leafletOutput(ns("map"), height=500)), # make sure to put the input inside ns()
      # # histogram occupies rows in the 2nd column
      # column(width = 5, plotOutput(ns("distPlot")),
      #        sliderInput(ns("bins"), "Number of bins:", 
      #                    min = 1, max = 50, value = 30)
      # )
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page")
  )
}

WINPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is 
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns

    # # Create the histogram
    # output$distPlot <- renderPlot({
    #   # generate bins based on input$bins from ui.R
    #   x <- filter(WIN, ComponentShort == "ATEMP") %>%
    #     select(Result) %>%
    #     pull()
    #   x <- as.numeric(x) # stop-gap measure because everything is characters
    #   bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
    #     length.out = input$bins + 1
    #   )
    # 
    #   # draw the histogram with the specified number of bins
    #   hist(x,
    #     breaks = bins, col = "darkgray", border = "white",
    #     xlab = "Air temperature (degrees Celsius)",
    #     main = "Histogram of air temperatures"
    #   )
    # })

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
          group = "Counties", popup = ~NAME
        ) %>%
        addMarkers(
          data = WIN_data_locations,
          popup = ~ paste("Station Name: ", Station.Name, "<br>"),
          group = "WIN"
        )  %>%
        # Layers control (turning layers on and off)
        addLayersControl(
          overlayGroups = c("Counties", "GTMNERR boundaries", "WIN"), # 
          options = layersControlOptions(collapsed = FALSE)
        )  %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })

    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
    })
  })
}