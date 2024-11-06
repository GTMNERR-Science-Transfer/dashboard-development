### HAB Data------------------------------
load("./03_Data_for_app/HAB.RData")
GeneraData <- separate_wider_delim(data = HAB, cols = Species, delim = " ",
                                   names = c("genus", "species"), too_few = "align_start", too_many = "merge")

HABPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Harmful Algal Bloom Data"),
    fluidRow(
      #User inputs in 1st column
      column(width = 3, selectInput(ns("genus"), "What genus of Algae do you want data for?", c("All", unique(GeneraData$genus))),
             uiOutput(ns("selectSpecies")),
             uiOutput(ns("selectDate"))), #Reserve space for species/date dropdown input
      # Map occupies 2nd column
      column(width = 5, leafletOutput(ns("map"), height=750)),
      # plot occupies rows in the 3rd column
      column(width = 4, 
             plotOutput(ns("timePlot")),
             # sliderInput("bins", "Number of bins:", 
             #             min = 1, max = 50, value = 30)
      )
    ),
    actionButton(inputId = ns("go_back"), label = "Back to Main Page") #All input IDs need to be inside ns()
  )
}

HABPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns
    output$selectSpecies <- renderUI(selectInput(ns("species"), "Select what species you are interested in (optional)", unique(GeneraData$species[GeneraData$genus == input$genus])))
    output$selectDate <- renderUI(selectInput(ns("date"), "The following dates have data for your selected algal bloom. Select one to view data on map (optional)", unique(GeneraData$`Sample Date`[GeneraData$genus == input$genus & GeneraData$species == input$species])))
    
    ### Selected Data (for map) -----------------------
    select_HAB_data <- reactive({
      req(input$genus, input$species, input$date)
      
      GeneraData %>%
        filter(genus == input$genus,
               species == input$species,
               Description == "present") %>% #,
               #`Sample Date` == input$date) %>%
        select(Latitude, Longitude, Date, Site, County, vars, vals) %>% 
        filter(vars == "cells/L*")
    })
    
    output$timePlot <- renderPlot({
      if(!is.null(select_HAB_data)){
        ggplot(select_HAB_data(), aes(x = Date, y = vals, color = Site))+
          geom_point()+
          labs(x = "", y = "Concentration (cells/L)")+
          theme_bw()+
          theme(legend.position = "bottom")
      }
      
      
      
      # # generate bins based on input$bins from ui.R
      # x    <- filter(HAB,  vars == "Temperature (C)") %>% select(vals) %>% pull()
      # bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
      #             length.out = input$bins + 1)
      # 
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white',
      #      xlab = 'Temperature (degrees Celsius)',
      #      main = 'Histogram of water temperatures')
    })
    
    ### Selected Data (for map) -----------------------
    HAB_data_locations <- reactive({
      req(input$genus, input$species, input$date)
      
      GeneraData %>%
        filter(genus == input$genus,
               species == input$species,
               `Sample Date` == input$date) %>%
        select(Latitude, Longitude, Site, County) %>% # , `cells/L*` This is not a column
        distinct() %>% 
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    })
    
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
        addMarkers(data = HAB_data_locations(),
                   #color = ~colorQuantile("YlOrRd",`cells/L*`)(`cells/L*`), #This is currently not working
                   popup = ~paste("Site: ", Site, "<br>",
                                  "County: ", County),
                   group = "HAB") %>% 
        # # Layers control (turning layers on and off)
        addLayersControl(overlayGroups = c("Counties", "GTMNERR boundaries"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })
    
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
      })
  })
}
