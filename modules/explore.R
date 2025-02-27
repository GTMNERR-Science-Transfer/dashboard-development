########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# Created June 2024
# Last updated: 12 August 2024

# This page shows a map, with a dropdown menu to pick types of 
# datasets, as well as shapefiles for the area

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
dash_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone"
) |>
  bs_add_variables(
    "navbar-bg" = "$primary",
    "navbar-color" = "$light",
    #"progress-bar-bg" = "$secondary",
    .where = "declarations"
  ) |>
  bs_add_rules("
    .navbar { color: var(--bs-light) !important; }
    .navbar .navbar-brand, .navbar .nav-link { color: var(--bs-light) !important; }
  ")

explPageUI <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "sandstone"),
    
    title = "Explore Data",
    
    sidebar = sidebar(
      title = "Data Selection",
      selectInput(
        inputId = ns("datatype_selector"),
        label = "Select a type of data to see locations with data availability",
        choices = unique(all_data_locations$type),
        selected = unique(all_data_locations$type)[1],
        width = "100%"
      ),
      actionButton(
        inputId = ns("reset_view"),
        label = "Reset map view",
        icon = icon("rotate-right", library = "fa")
      ),
      
      card(
        full_screen = TRUE, # Let's you click and enlarge the card to full screen
        fill = TRUE,
        height = "80vh",
        card_header("Dataset Summary"),
        layout_columns(
          fill = TRUE,
          col_widths = c(12), # Ensures value boxes stack properly
          value_box(
            title = "Total stations",
            value = textOutput(ns("total_stations")),
            showcase = div(bsicons::bs_icon("geo-fill", size = 40)),
            showcase_layout = "top right",
            theme = "primary"
          ),
          value_box(
            title = "First year of available data",
            value = textOutput(ns("first_year")),
            showcase = div(bsicons::bs_icon("calendar-check", size = 40)),
            showcase_layout = "top right",
            theme = "info"
          ),
          value_box(
            title = "Last year of available data",
            value = textOutput(ns("last_year")),
            showcase = div(bsicons::bs_icon("calendar-x", size = 40)),
            showcase_layout = "top right",
            theme = "info"
          ),
          value_box(
            title = "Average measurements per station",
            value = textOutput(ns("avg_measurements")),
            showcase = div(bsicons::bs_icon("clipboard2-data", size = 40)),
            showcase_layout = "top right",
            theme = "success"
          )
        )
      )
    ),
    
    # Main content (text and map)
    layout_columns(
      col_widths = c(12), # Ensures full width for the header card
      card(
        fill = TRUE, # Ensures no scroll bars as long as height is set
        height = "20vh",
        card_header("Welcome!"),
        card_body(
          p(HTML("This is the overview and exploration page of the Guana Estuary Data Dashboard. <br><br>
          There are various data sets available through this dashboard. The dropdown menu 
          shows you the locations with data availability for different data sets. Clicking
          on the stations shows you the period of data availability. <br><br>
          To explore and view the actual data, pick your data set of interest from
          the tabs at the top of your screen (or the tabs in the fold-out menu at the top)."))
        )
      )
    ),
    
    layout_columns(
      col_widths = c(12),
      card(
        full_screen = TRUE,  # Allows full screen mode if needed
        height = "80vh",     # Card takes up 80% of the viewport height
        card_header("Map View"),
        card_body(
          shinycssloaders::withSpinner(
            leafletOutput(ns("map"), height = "80vh")  # Map fills the card body
          ),
          style = "padding: 0; overflow: hidden;"  # Remove extra padding and disable scrolling
        )
      )
    ),
    br()
  )
}

### Define the server logic ----------------------------------------------------

explPageServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) { # this nested approach is
    # necessary to be able to us the "back" button, otherwise Shiny cannot find
    # the id for "tabs"
    ns <- session$ns
    # Define initial view coordinates and zoom level
    initial_lat <- 29.905 
    initial_lng <- -81.289
    initial_zoom <- 10
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18, scrollWheelZoom = TRUE)) %>%
        setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>% 
        # Base map
        addTiles(group = "Map") %>%  # Add default OpenStreetMap map tiles
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% # Add satellite as an option
        # addWMSTiles() # Putting this here as a reminder that you can also add
        # custom third party layers, e.g. Nexrad, see https://rstudio.github.io/leaflet/articles/basemaps.html#wms-tiles
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
        addLayersControl(baseGroups = c("Map", "Satellite"),
                         overlayGroups = c("GTMNERR boundaries", "Counties", 
                                           "Mangroves", "Outstanding Florida Waters", 
                                           "Salt marshes", "Watershed Basins",
                                           "Watershed Subbasins"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Counties", "Mangroves", "Outstanding Florida Waters",
                    "Salt marshes", "Watershed Basins", "Watershed Subbasins")) %>%
        addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles") 
    })
    
    # Select dataset to add markers to the plot
    observeEvent(input$datatype_selector, {
      req(input$datatype_selector)
      # Filter data based on selected group
      filtered_data <- all_data_locations[all_data_locations$type == input$datatype_selector,]
      #print(filtered_data)
      # Add markers to the map
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
        )
    }, ignoreInit = FALSE)
    # Add buttons to go to other pages
    # observeEvent(input[[ns("go_to_subpage")]], {
    #   print("Go to subpage button clicked")
    #   updateTabItems(session, "tabs", selected = "subpage")
    # })
    
    # Caclculate the stats to add to the value boxes
    filtered_data <- reactive({
      req(input$datatype_selector)
      all_data_locations %>%
        filter(type == input$datatype_selector)
    })
    
    output$total_stations <- renderText({
      n_distinct(filtered_data()$site_friendly)
    })
    
    output$first_year <- renderText({
      min(filtered_data()$minYear, na.rm = TRUE)
    })
    
    output$last_year <- renderText({
      max(filtered_data()$maxYear, na.rm = TRUE)
    })
    
    output$avg_measurements <- renderText({
      df <- filtered_data() %>% #### This still needs to be updated, this is currently not a count of obs
        group_by(site_friendly) %>%
        summarise(measurements = n(), .groups = "drop")
      round(mean(df$measurements, na.rm = TRUE), 1)
    })
    
    # Observe reset button click to restore initial view
    observeEvent(input$reset_view, {
      leafletProxy(ns("map")) %>%
        setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom)
    })
  }
  )
}

