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
# WIN_df <- readRDS("./03_Data_for_app/WIN.Rds")
# 
# WIN_data_locations = WIN_df %>%
#   filter(variable %in% c("geometry", 
#                          "StationCode", 
#                          "SampleDate",
#                          "Latitude",
#                          "Longitude")
#   ) %>%
#   select(c(RowID, variable, value)) %>%
#   distinct(RowID, variable, value) %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = value,
#     values_fill = list(value = NA)
#   ) %>%
#   distinct(geometry, StationCode, SampleDate, Latitude, Longitude) %>%
#   mutate(
#     SampleDate = ymd_hms(SampleDate),
#     Latitude = as.numeric(Latitude),
#     Longitude = as.numeric(Longitude),
#     type = "Water quality",
#     dataset = "Watershed Information Network (DEP)", # Update this so we use data_source
#     minYear = min(year(SampleDate)),
#     maxYear = max(year(SampleDate))
#   ) %>% 
#   select(-geometry, -SampleDate)

#### Location data ------------------------------------------------
all_data_locations <- readRDS("./03_Data_for_app/all_data_locations.Rds")

# add info for icons and colors
all_data_locations <- all_data_locations %>%
  mutate(group_icon = case_when(
    type == "Water Quality" ~ "flask",
    type == "Algae" ~ "microscope"),
    group_color = case_when(
      type == "Water Quality" ~ "orange",
      type == "Algae" ~ "purple"))


### Define the UI -------------------------------------------------------------
mainPageUI <- function(id) {
  ns <- NS(id) # This is an important part to add to all subpages so they use the
  # correct sessions / ID's that connect the ui and server here
  tagList(
    h2("Welcome!"),
    p("This is the main page of the Guana River Data Dashboard. Use the dropdown menu 
      below to see locations with a certain data type. To view these data, use 
      the menu on the left of the screen."),
    # Create a button that opens a pop up with a message
    actionButton(ns("help_button"), "Click here for help", icon = icon("info-circle")),
    # Dropdown menu for markers is above the map
    fluidRow(
      column(width = 7, uiOutput(ns("dropdown_ui")), style = "position:relative;z-index:1001;"),
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
      choices = unique(all_data_locations$type),
      selected = unique(all_data_locations$type)[1]
    )
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18, scrollWheelZoom = FALSE)) %>%
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
                  group = "Watershed Basins", popup = ~NAME) %>%
      addPolygons(data = HUC12, 
                  color = "darkblue", weight = 2, opacity = 1,
                  fill = TRUE, fillColor = "darkblue", fillOpacity = 0.2,
                  group = "Watershed Subbasins", popup = ~NAME) %>%
      # Layers control (turning layers on and off)
      addLayersControl(overlayGroups = c("GTMNERR boundaries", "Counties", 
                                         "Mangroves", "Outstanding Florida Waters", 
                                         "Salt marshes", "Watershed Basins",
                                         "Watershed Subbasins"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Counties", "Mangroves", "Outstanding Florida Waters", 
                  "Salt marshes", "Watershed Basins", "Watershed Subbasins")) %>% 
      addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles") 
  })
  
  # Select dataset to add markers to the plot
  observeEvent(input$datatype_selector, {
    # Filter data based on selected group
    filtered_data <- all_data_locations[all_data_locations$type == input$datatype_selector,]
    print(filtered_data)
    # Add markers to the map - commented out the popup bc this only works for WQ data
    print("Adding markers")
    leafletProxy(ns("map")) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
                        data = filtered_data,
                        icon = makeAwesomeIcon(icon = ~group_icon, markerColor = ~group_color, library = "fa",
                                               iconColor = "black"),
                        options = markerOptions(riseOnHover = TRUE), # Brings marker forward when hovering
                        popup = ~paste("<b>Station:</b> ", site_friendly, "<br>", # popups appear when clicking
                                       "<b>Sampling start year:</b> ", minYear, "<br>",
                                       "<b>Latest year of sampling:</b> ", maxYear, "<br"),
                        label = ~paste("Station: ", site_friendly), # labels appear when hovering
                        labelOptions = labelOptions(direction = "auto",
                                                    style = list(
                                                      "color" = "gray27",
                                                      "font-style" = "italic",
                                                      "font-size" = "12px",
                                                      "border-color" = "rgba(0,0,0,0.5)"
                                                      )
                                                    )
                        )#%>%
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
  
  # Show modal for help
  observeEvent(input$help_button, {
    showModal(modalDialog(
      title = "Help",
      tags$div(
        tabsetPanel(
          tabPanel("Overview", 
                   tagList(
                     h4("Main Page"),
                     p("This is the main page of the Guana River Data Dashboard. Use the dropdown menu above to see locations with data availability. To view these data, use the menu on the left of the screen.")
                   )),
          tabPanel("Using the Dashboard",
                   tagList(
                     h4("Navigation"),
                     p("Use the left-hand menu to navigate through different sections."),
                     img(src = "dashboard.gif", height = "300px"),
                     p("You can view different types of data by selecting the appropriate options."),
                     p(HTML("On the map, click into the stations represented by the icon <img src='icon.png
' height=40px /> to display the dashboard for the selected data type.")),
                     p("The dashboard will be displayed below the map."),
                     p(HTML("You can also click on the icons <img src='plus.svg' height=20px /> to zoom in and <img src='minus.svg' height=20px /> to zoom out on the map.")),
                     p(HTML("Click on the icon <img src='http://127.0.0.1:5569/leaflet-1.3.1/images/layers-2x.png' height=20px /> to toggle layers on and off.")),
                     p(HTML("To measure distances on the map, click on the icon <img src='http://127.0.0.1:5569/leaflet-measure-2.1.7/images/rulers_@2X.png' height=20px />."))
                   ))
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      style = "position:relative;z-index:1002;"
    ))
  })
  
}
