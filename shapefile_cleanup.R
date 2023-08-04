########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 5 June 2023

# Cleaning up shapefiles needed

library(sf)
library(tidyverse)
library(ggmap)
register_google(key = "AIzaSyBBgPDaJao2MVa0yvlULKvAbE7lEosOthQ")

#### GTMNERR boundary and aquatic preserves ####
GTM_boundaries <- st_read("shapefiles/flnerr_jul10/flnerr_jul10.shp")
# CRS: Albers Conical Equal Area

# Filter for only GTMNERR and take out the polygon that is only ocean
GTMNERR <- GTM_boundaries %>% 
  filter(str_starts(NAME, "GUANA"), OBJECTID != 9)
# Also - we only want the Guana River area...
ggplot()+
  geom_sf(data=GTMNERR)
# Get map of aquatic preserves and get only Guana
aqua_preserve <- st_read("shapefiles/aquap_aug21/aquap_aug21.shp")
aqua_preserve <- filter(aqua_preserve, SHORT_NAME == "GUANA RIVER MARSH")

ggplot()+
  geom_sf(data = aqua_preserve)

# Get min/max coordinates (plus a little extra) to use in cropping other
# shapefiles
minmax_coords <- st_bbox(aqua_preserve)
minmax_coords["xmin"] <- minmax_coords["xmin"] - 5000
minmax_coords["xmax"] <- minmax_coords["xmax"] + 5000
minmax_coords["ymin"] <- minmax_coords["ymin"] - 5000
minmax_coords["ymax"] <- minmax_coords["ymax"] + 5000

GTMNERR <- st_crop(GTMNERR, minmax_coords)
st_write(GTMNERR, "shapefiles/GTMNERR.shp")


#### Google base map ####
# For now I am getting a Google base map. But for the Shiny app we should use
# leaflet to make it interactive.
google_base <- get_map(location = c(lon = -81.347388, lat = 30.075), zoom = 12, maptype = 'roadmap')
test <- ggmap(google_base)
test

##### Counties #####
counties <- st_read("shapefiles/countyshore_areas_sep15/countyshore_areas_sep15.shp")
# CRS: Albers Conical Equal Area

# Select area
counties_select <- st_crop(counties, minmax_coords) %>% 
  filter(NAME != "WATER")

ggplot()+
  geom_sf(data=counties_select, fill = NA)+
  geom_sf(data=GTMNERR, fill = "blue", alpha = 0.3)+
  geom_sf(data = aqua_preserve, fill = NA, color = "red")+
  theme_bw()

##### Salt marshes #####
salt_marsh <- st_read("shapefiles/salt_marsh_2020/salt_marsh_2020.shp")
# CRS: NAD83(HARN) / Florida GDL Albers
salt_marsh <- st_crop(salt_marsh, minmax_coords) 
ggplot()+
  geom_sf(data = salt_marsh)+
  geom_sf(data=GTMNERR, fill = "blue", alpha = 0.3)

##### Hydrology (several shapefiles) #####
hydro_6 <- st_read("shapefiles/nhdwbd_huc6_dec17/nhdwbd_huc6_dec17.shp")
hydro_6 <- st_crop(hydro_6, minmax_coords) 
ggplot()+
  geom_sf(data = hydro_6)

hydro_8 <- st_crop(st_read("shapefiles/nhdwbd_huc8_dec17/nhdwbd_huc8_dec17.shp"), minmax_coords) 
ggplot()+
  geom_sf(data = hydro_8)
# About same as 6

hydro_10 <- st_crop(st_read("shapefiles/nhdwbd_huc10_dec17/nhdwbd_huc10_dec17.shp"), minmax_coords) 
ggplot()+
  geom_sf(data = hydro_10)

hydro_12 <- st_crop(st_read("shapefiles/nhdwbd_huc12_dec17/nhdwbd_huc12_dec17.shp"), minmax_coords) 
ggplot()+
  geom_sf(data = hydro_12)

ggplot()+
  geom_sf(data = GTMNERR, fill = NA, color = "blue", size = 3)+
  #geom_sf(data = hydro_6, fill = NA)+
  #geom_sf(data = hydro_10, fill = NA)+
  geom_sf(data = hydro_12, fill = NA)

#### Water bodies ####
waterbodies <- st_crop(st_read("shapefiles/nhd24waterbody_dec17/nhd24waterbody_dec17.shp"), minmax_coords) 

ggplot()+
  geom_sf(data = GTMNERR, fill = NA, color = "blue", size = 3)+
  geom_sf(data = waterbodies, aes(fill = DESCRIPT))

#### Land use ####
landuse1 <- st_crop(st_read("shapefiles/lu_sjrwmd_2014/lu_sjrwmd_2014.shp"), minmax_coords)

ggplot()+
  geom_sf(data = landuse1, aes(fill = LEVEL1))

#### Mangroves ####
mangrove <- st_crop(st_read("shapefiles/mangroves_2020/mangroves_2020.shp"), minmax_coords)

ggplot()+
  geom_sf(data = GTMNERR, fill = "blue", alpha = 0.3)+
  geom_sf(data = mangrove)

#### OFW? #### gdb file




