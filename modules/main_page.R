mainPageUI <- function(id) {
  ns <- NS(id) # This is an important part to add to all subpages so they use the
  # correct sessions / ID's that connect the ui and server here
  tagList(
    h2("Main Page"),
    p("This is the main page of the Guana River Data Dashboard."),
    fluidRow(
      # Map occupies 1st column
      column(width = 10, leafletOutput(ns("map"), height=750)),
      # histogram occupies rows in the 2nd column
      # column(width = 5, plotOutput(ns("distPlot")),
      #        sliderInput(ns("bins"), "Number of bins:", 
      #                    min = 1, max = 50, value = 30)
      # )
    ),
    # fluidRow(
    #   column(12, 
    #          actionButton(ns("go_to_subpage"), "Go to Subpage")
    #   )
    #)
  )
}


mainPageServer <- function(input, output, session) {
  ns <- session$ns
  
  # Create the histogram
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   x    <- filter(WQ,  ComponentShort == "ATEMP") %>% select(Result) %>% pull()
  #   x <- as.numeric(x) # stop-gap measure because everything is characters
  #   bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
  #               length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #        xlab = 'Air temperature (degrees Celsius)',
  #        main = 'Histogram of air temperatures')
  # })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
      #setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
      clearBounds() %>% # centers map on all min/max coords
      # Base map
      addTiles() %>%  # Add default OpenStreetMap map tiles
      # Polygons, add groups
      addPolygons(data = GTMNERR, color = "purple", fill = NA, 
                  weight = 2, opacity = 1, group = "GTMNERR boundaries") %>% 
      addPolygons(data = counties_select, 
                  color = "black", weight = 2, opacity = 1,
                  fill = TRUE, fillColor = "white", fillOpacity = 0.01,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Counties", popup = ~NAME) %>% 
      addPolygons(data = mangroves, 
                  color = "darkgreen", weight = 2, opacity = 1,
                  fill = TRUE, fillColor = "darkgreen", fillOpacity = 0.4,
                  group = "Mangroves") %>% #, popup = ~Area_ha
      addPolygons(data = ofw, 
                  color = "darkorange", weight = 2, opacity = 1,
                  fill = TRUE, fillColor = "darkorange", fillOpacity = 0.4,
                  group = "Outstanding Florida Waters") %>%
      addPolygons(data = saltmarsh, 
                  color = "darkslateblue", weight = 2, opacity = 1,
                  fill = TRUE, fillColor = "darkslateblue", fillOpacity = 0.4,
                  group = "Salt marshes") %>%
      addPolygons(data = HUC10, 
                  color = "royalblue", weight = 2, opacity = 1,
                  fill = TRUE, fillColor = "royalblue", fillOpacity = 0.2,
                  group = "HUC10", popup = ~NAME) %>%
      addPolygons(data = HUC12, 
                  color = "darkblue", weight = 2, opacity = 1,
                  fill = TRUE, fillColor = "darkblue", fillOpacity = 0.2,
                  group = "HUC12", popup = ~NAME) %>%
      # addMarkers(data = HAB_data_locations,
      #            popup = ~paste("Site: ", Site, "<br>",
      #                           "County: ", County),
      #            group = "HAB") %>% 
      addMarkers(data = WQ_data_locations,
                 popup = ~paste("Station: ", site_friendly, "<br>",
                                "Location: ", wbid, "<br>",
                                "Latest year of sampling: ", maxYear, "<br",
                                "Sampling start year: ", minYear, "<br"),
                 group = "WQ") %>% 
      # # Layers control (turning layers on and off)
      addLayersControl(overlayGroups = c("Counties", "GTMNERR boundaries", 
                                         "WQ", "Mangroves", "Salt marshes",
                                         "Outstanding Florida Waters", "HUC10",
                                         "HUC12"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
  })
  
  # Add buttons to go to other pages
  # observeEvent(input[[ns("go_to_subpage")]], {
  #   print("Go to subpage button clicked")
  #   updateTabItems(session, "tabs", selected = "subpage")
  # })
}
