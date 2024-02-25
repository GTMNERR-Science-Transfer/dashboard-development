#
# This is a Shiny web application. 
# Test for making leaflet map

if(!require(shiny)){ install.packages("shiny") } ;  library(shiny)
if(!require(tidyverse)){ install.packages("tidyverse") } ;  library(tidyverse)
if(!require(sf)){ install.packages("sf") } ;  library(sf)
if(!require(leaflet)){ install.packages("leaflet") } ;  library(leaflet)

ui <- fluidPage(
    titlePanel("Guana Tolomato Matanzas NERR"),
    leafletOutput("map")
)

GTMNERR <- st_read("~/github/GTMNERR Science Transfer/App_dev/03_Data_for_app/shapefiles_new/GTMNERR.shp")# CRS: Albers Conical Equal Area
GTMNERR <- st_transform(GTMNERR, crs = 4326)

counties_select <- st_read("~/github/GTMNERR Science Transfer/App_dev/03_Data_for_app/shapefiles_new/counties_GTMNERR.shp")
counties_select <- st_transform(counties_select, crs = 4326)

HAB_data <- readRDS("~/github/GTMNERR Science Transfer/App_dev/03_Data_for_app/HAB.Rds")
HAB_data_locations <- HAB_data %>% 
    select(Latitude, Longitude, Site, `Collection Agency`, County) %>% # took out `HAB ID` (otherwise 1172 instead of 17 locations)
    distinct() %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Add buttons for people to pick county, site, algal present not present?
# Show timeseries plot with it first, for now? Also histogram?

server <- function(input, output) {
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
            addMarkers(data = HAB_data_locations,
                       popup = ~paste("Site: ", Site, "<br>",
                                      "County: ", County),
                       group = "HAB") %>% 
            # # Layers control (turning layers on and off)
            addLayersControl(overlayGroups = c("Counties", "GTMNERR boundaries", "HAB"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addMeasure(primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
    })
}

# Run the application
shinyApp(ui, server)
