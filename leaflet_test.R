
library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)

# setView() sets the center of the map view and the zoom level;
# fitBounds() fits the view into the rectangle [lng1, lat1] – [lng2, lat2];
# clearBounds() clears the bound, so that the view will be automatically 
# determined by the range of latitude/longitude data in the map layers 
# if provided;
GTMNERR <- st_read("shapefiles_new/GTMNERR.shp")# CRS: Albers Conical Equal Area

GTMNERR <- st_transform(GTMNERR, crs = 4326)

m <- leaflet(data = GTMNERR, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
  #setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
  clearBounds() %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "purple", fill = NA) #%>% 
  #addMarkers(lng=-81.347388, lat=30.075, popup="Guana")
m 

counties_select <- st_read("shapefiles_new/counties_GTMNERR.shp")
counties_select <- st_transform(counties_select, crs = 4326)

HAB_data <- readRDS("HAB.Rds")
HAB_data_locations <- HAB_data %>% 
  select(Latitude, Longitude, Site, `Collection Agency`, County) %>% # took out `HAB ID`
  distinct() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
  setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
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
                   options = layersControlOptions(collapsed = FALSE)) 

