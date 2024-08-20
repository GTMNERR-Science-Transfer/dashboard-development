########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# Created June 2024
# Last updated: 12 August 2024

# This page shows a map, with a dropdown menu to pick types of 
# datasets, as well as shapefiles for the area

#### Get WIN data locations
# I am putting this here right now, but I feel we should move this to a cleaning
# script so it doesn't need to be run every time someone uses the app (as with
# WQ locations)
WIN_df <- readRDS("./03_Data_for_app/WIN.Rds")

WIN_data_locations = WIN_df %>%
  filter(variable %in% c("geometry", 
                         "HUC12Name", 
                         "SampleDate",
                         "Latitude",
                         "Longitude")
  ) %>%
  select(c(RowID, variable, value)) %>%
  distinct(RowID, variable, value) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA)
  ) %>%
  distinct(geometry, HUC12Name, SampleDate, Latitude, Longitude) %>%
  mutate(
    SampleDate = ymd_hms(SampleDate),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    type = "Water quality",
    dataset = "Watershed Information Network (DEP)", # Update this so we use data_source
    minYear = min(year(SampleDate)),
    maxYear = max(year(SampleDate))
  ) %>% 
  select(-geometry, -SampleDate)

#### WQ locations data ------------------------------------------------
WQ_data_locations <- readRDS("./03_Data_for_app/WQ_locations.Rds")

# For now, for testing, make WQ_locations the dataframe to be used for filtering
datasets_location <- full_join(WQ_data_locations, WIN_data_locations)
datasets_location <- st_as_sf(datasets_location, coords = c("Longitude", "Latitude"), crs = 4326)
datasets_location <- datasets_location[!duplicated(datasets_location),]

color_palette <- colorFactor(palette = c("red", "goldenrod1"), #, "blue", "green"
                             domain = datasets_location$dataset)

#### HAB locations data ------------------------------------------------
# Add this at some point so there is another dataset that shows up in the dropdown
# menu

### Define the UI -------------------------------------------------------------
mainPageUI <- function(id) {
  ns <- NS(id) # This is an important part to add to all subpages so they use the
  # correct sessions / ID's that connect the ui and server here
  tagList(
    h2("Main Page"),
    p("This is the main page of the Guana River Data Dashboard."),
    # Dropdown menu for markers is above the map
    fluidRow(
      column(width = 7, uiOutput(ns("dropdown_ui"))),
    ),
    fluidRow(
      column(width = 10, leafletOutput(ns("map"), height="500px")),
    ),
    # fluidRow(
    #   column(12, 
    #          actionButton(ns("go_to_subpage"), "Go to Subpage")
    #   )
    #)
  )
}

### Define the server logic ----------------------------------------------------

mainPageServer <- function(input, output, session) {
  ns <- session$ns
  
  # Create the dropdown UI
  output$dropdown_ui <- renderUI({
    print("Rendering dropdown UI")
    selectInput(
      inputId = ns("datatype_selector"),
      label = "Select a type of data to see locations with data availability",
      choices = unique(datasets_location$type),
      selected = unique(datasets_location$type)[1]
    )
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
      setView(-81.289, lat=29.905, zoom = 10) %>% 
      #clearBounds() %>% # centers map on all min/max coords
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
      # Layers control (turning layers on and off)
      addLayersControl(overlayGroups = c("GTMNERR boundaries", "Counties", 
                                         "Mangroves", "Salt marshes",
                                         "Outstanding Florida Waters", "HUC10",
                                         "HUC12"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Counties", "Mangroves", "Salt marshes",
                  "Outstanding Florida Waters", "HUC10", "HUC12")) %>% 
      addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
  })
  
  # Select dataset to add markers to the plot
  observeEvent(input$datatype_selector, {
    # Filter data based on selected group
    filtered_data <- datasets_location[datasets_location$type == input$datatype_selector,]
    print(filtered_data)
    # Add markers to the map - commented out the popup bc this only works for WQ data
    print("Adding markers")
    leafletProxy(ns("map")) %>%
      clearMarkers() %>%
      addAwesomeMarkers(icon = makeAwesomeIcon(icon = "flask", markerColor = "orange", library = "fa",
                                               iconColor = "black"),
                        data = filtered_data) #%>%
      # addCircleMarkers(
      #   data = filtered_data,
      #   color = ~color_palette(dataset),
      #   opacity = 1,
      #   fillOpacity = 0.5,
      #   fillColor = ~color_palette(dataset),
      #   fill = TRUE,
      #   weight = 3,
      #   radius = 8,
      # popup = ~paste("Station: ", site_friendly, "<br>",
      #                                           "Location: ", wbid, "<br>",
      #                                           "Latest year of sampling: ", maxYear, "<br",
      #                                           "Sampling start year: ", minYear, "<br")
      #)
  })
  # Add buttons to go to other pages
  # observeEvent(input[[ns("go_to_subpage")]], {
  #   print("Go to subpage button clicked")
  #   updateTabItems(session, "tabs", selected = "subpage")
  # })
}
