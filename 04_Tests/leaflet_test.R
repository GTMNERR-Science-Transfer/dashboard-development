
# Test to make an interactive map with leaflet (not in Shiny)

library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)

# setView() sets the center of the map view and the zoom level;
# fitBounds() fits the view into the rectangle [lng1, lat1] – [lng2, lat2];
# clearBounds() clears the bound, so that the view will be automatically 
# determined by the range of latitude/longitude data in the map layers 
# if provided;
GTMNERR <- st_read("03_Data_for_app/shapefiles_new/GTMNERR.shp")# CRS: Albers Conical Equal Area

GTMNERR <- st_transform(GTMNERR, crs = 4326)

# Leaflet uses EPSG 3857, change this
# https://gis.stackexchange.com/questions/48949/epsg-3857-or-4326-for-web-mapping
#epsg4326 <- leafletCRS(crsClass = "L.CRS.EPSG4326")
# BUT! It actually seems that Leaflet does still want shapefiles in 4326, see
# https://github.com/Leaflet/Leaflet/issues/4146 I also tried to change GTMNERR 
# to crs 3857 but that threw errors. See second paragraph here: 
# https://rstudio.github.io/leaflet/articles/projections.html

m <- leaflet(data = GTMNERR, 
             options = leafletOptions(#crs = epsg4326,
                                      minZoom = 9, 
                                      maxZoom = 18)) %>%
  setView(lng=-81.347388, lat=30.075, zoom = 11) %>% 
  #clearBounds() %>% # This makes the view default to the largest map object!!
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "purple", fill = NA) %>% 
  addMarkers(lng=-81.347388, lat=30.075, popup="Guana")
m 

counties_select <- st_read("03_Data_for_app/shapefiles_new/counties_GTMNERR.shp")
counties_select <- st_transform(counties_select, crs = 4326)

HAB_data <- readRDS("03_Data_for_app/HAB.Rds")
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
  addAwesomeMarkers(icon = makeAwesomeIcon(icon = "flask", markerColor = "blue", library = "fa",
                                           iconColor = "black"), #"",
                    data = HAB_data_locations,
                    #lng= rep(-81.347388, 34), lat = rep(30.075,34)
                    ) %>% 
  # addMarkers(data = HAB_data_locations,
  #             popup = ~paste("Site: ", Site, "<br>",
  #                            "County: ", County),
  # group = "HAB") %>% 
# # Layers control (turning layers on and off)
  addLayersControl(overlayGroups = c("Counties", "GTMNERR boundaries", "HAB"),
                   options = layersControlOptions(collapsed = FALSE)) 

