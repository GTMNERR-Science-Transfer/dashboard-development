
library(leaflet)
library(sf)
library(tidyverse)

# setView() sets the center of the map view and the zoom level;
# fitBounds() fits the view into the rectangle [lng1, lat1] – [lng2, lat2];
# clearBounds() clears the bound, so that the view will be automatically 
# determined by the range of latitude/longitude data in the map layers 
# if provided;
GTMNERR <- st_read("leaflet_test1/shapefiles/GTMNERR.shp")# CRS: Albers Conical Equal Area

GTMNERR <- st_transform(GTMNERR, crs = 4326)

m <- leaflet(data = GTMNERR, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
  setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "purple", fill = NA) #%>% 
  #addMarkers(lng=-81.347388, lat=30.075, popup="Guana")
m 

counties_select <- st_transform(counties_select, crs = 4326)

leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
  setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
  # Base map
  addTiles() %>%  # Add default OpenStreetMap map tiles
  # Polygons, add groups
  addPolygons(data = GTMNERR, color = "purple", fill = NA, 
              weight = 2, opacity = 1, group = "NAME") %>% 
  addPolygons(data = counties_select, color = "black", fill = NA, 
              weight = 2, opacity = 1, 
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE), # there is only a 
              #tiny sliver of another county here so highlighting does not work well
              group = "NAMELSAD") %>% 
  # addMarkers(data = HAB,
  #            lat = ~Latitude,
  #            lng = ~Longitude,
  #            popup = ~paste("Site: ", Site, "<br>",
  #                           "Species: ", Species)) %>% 
  # Layers control
  addLayersControl(
    #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("NAME", "NAMELSAD"),
    options = layersControlOptions(collapsed = FALSE)
  )

ui <- fluidPage(
  titlePanel("Guana Tolomato Matanzas NERR"),
  leafletOutput("map")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(
        data = stations,
        lat = ~lat,
        lng = ~lng,
        popup = ~paste("Station: ", station, "<br>",
                       "Value: ", value)
      ) %>%
      addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")
  })
}

shinyApp(ui, server)
