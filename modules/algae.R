### HAB Data------------------------------
HAB <- readRDS("./03_Data_for_app/HAB.Rds")

# Create long format so it can be used in the Shiny app
# Only doing this with numeric variables for now

GeneraData <- separate_wider_delim(data = HAB, cols = Species, delim = " ",
                                   names = c("genus", "species"), too_few = "align_start", too_many = "merge")

GeneraData$userMessage <- vector(length = length(GeneraData$genus))
GeneraData$userMessage[] <- "System Error; please report"

for(i in 1:length(GeneraData$userMessage)){
  if(!is.na(GeneraData$Description[i])){
    GeneraData$userMessage[i] = "Algae is Present"  
  } else{
    GeneraData$userMessage[i] = paste("Algae is present at ", toString(GeneraData$'cells/L*'[i]), " cells/L")
  }
}

HABPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Harmful Algal Bloom Data"),
    fluidRow(
      # First row - explanation
      column(width = 12,
             div(style = "margin-bottom: 20px;",
                 p(htmltools::HTML('This section provides an overview of harmful algal bloom data. 
                   Start by selecting a genus and species of algae you are interested in. <br>
                   Next, select the date that you are interested in. The map will automatically show
                   stations for which there is data for your selected parameters.<br>
                   Hovering over one of the station markers will provide the station name.
                   Algae quantity will be shown if available. 
                   Where quantity is not available, presence or absence will be indicated.'))
             )
      )
    ),
    fluidRow(
      #User inputs in 1st column
      column(width = 3, selectInput(ns("genus"), "What genus of Algae do you want data for?", c("All", unique(GeneraData$genus))),
             uiOutput(ns("selectSpecies")),
             sliderInput(
               inputId = ns("date_range"),
               label = "Select a Date Range",
               min = ymd(min(GeneraData$'Sample Date')), #NULL
               max = ymd(max(GeneraData$'Sample Date')), #NULL
               value = c(ymd(min(GeneraData$'Sample Date')), 
                         ymd(max(GeneraData$'Sample Date'))),
               timeFormat = "%m/%d/%Y",
               width = "100%"
             ),
             #uiOutput(ns("selectDate")) #Keeping as old code in case I need to go back to a single date option
             ),
      # Map occupies 2nd column
      column(width = 9, leafletOutput(ns("map"), height=750)),
      # plot occupies rows in the 3rd column
      ##column(width = 4, ##THIS PLOT IS WHAT I AM CURRENTLY WORKING ON! WILL SHOW A TIME SERIES OF DATA FOR A SPECIFIC STATION ONCE CLICKED, THIS LINE AND THE TWO BELOW IT WILL BE UN-COMMENTED
             ##plotOutput(ns("timePlot")), 
      ##)
    ),
    fluidRow(
      #User inputs in 1st column
      column(width = 9, 
      plotOutput(ns("timePlot")), 
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
    output$selectDate <- renderUI(sliderInput(ns("date_range"), "The following dates have data for your selected algal genus. Set a range to narrow data on the map", min = ymd(min(GeneraData$`Sample Date`[GeneraData$genus == input$genus & GeneraData$species == input$species])), max = ymd(max(GeneraData$`Sample Date`[GeneraData$genus == input$genus & GeneraData$species == input$species]))))
    
    ### Selected Data (for map) -----------------------
    select_HAB_data <- reactive({
      req(input$genus, input$species, input$date_range)
      
      GeneraData %>%
        filter(genus == input$genus,
               species == input$species) %>% #,
               #`Sample Date` == input$date) %>%
        select(Latitude, Longitude, Date, Site, County, userMessage)
    })
    
    ### Selected Data (for map) -----------------------
    HAB_data_locations <- reactive({
      req(input$genus, input$species, input$date_range)
      
      GeneraData %>%
        filter(genus == input$genus,
               species == input$species,
               ymd(`Sample Date`) >= input$date_range[1] & ymd(`Sample Date`) <= input$date_range[2]) %>%
        select(Latitude, Longitude, Site, County, userMessage, 'Sample Date') %>%
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
                   #color = ~colorQuantile("YlOrRd",`cells/L*`)(`cells/L*`), #This is currently not working because data is location only
                   popup = ~paste("Site: ", Site, "<br>",
                                  "County: ", County, "<br>",
                                  "Sample Date: ", `Sample Date`, "<br>",
                                  userMessage),
                   group = "HAB") %>% 
        # # Layers control (turning layers on and off)
        addLayersControl(overlayGroups = c("Counties", "GTMNERR boundaries"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })
    
    ##### THIS DOES NOT WORK BECAUSE THERE ARE NO VALS FOR THE 'PRESENT' ALGAE....
    output$timePlot <- renderPlot({
      ggplot(select_HAB_data(), aes(x = Date, y = 'cells/L*', color = Site))+
        geom_point()+
        labs(x = "", y = "Concentration (cells/L)")+
        theme_bw()+
        theme(legend.position = "bottom")
      
    })
    
    observeEvent(input$go_back, {
      updateTabItems(session = parentSession, inputId = "tabs", selected = "main_page")
      })
  })
}
