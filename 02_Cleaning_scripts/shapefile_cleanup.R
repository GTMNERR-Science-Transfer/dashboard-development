########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 5 June 2023

# Cleaning up (modifying) shapefiles needed

library(sf)
library(tidyverse)
library(ggmap)
register_google(key = "AIzaSyBBgPDaJao2MVa0yvlULKvAbE7lEosOthQ")

#### GTMNERR boundary and aquatic preserves ####
# From Nikki Dix
GTMNERR <- st_read("01_Data_raw/shapefiles/GTMNERR Boundary_query update 2021/GTM_RB_2016_Merge (1).shp")
# CRS: NAD83 / UTM zone 17N

# Check what the 8 polygons are
ggplot()+
  geom_sf(data=GTMNERR, aes(fill = Res_Name)) # Hmmm

st_write(GTMNERR, "03_Data_for_app/shapefiles_new/GTMNERR.shp")

# Get min/max coordinates for selecting from other shapefiles
st_bbox(GTMNERR)
# UTM is in meters. Add/subtract 5 km (5000 m)
xmin <- st_bbox(GTMNERR)$xmin - 5000
xmax <- st_bbox(GTMNERR)$xmax + 5000
ymin <- st_bbox(GTMNERR)$ymin - 5000
ymax <- st_bbox(GTMNERR)$ymax + 5000
# Create points from these coordinates
pt1 <- st_point(c(xmin, ymin))
pt2 <- st_point(c(xmax, ymin))
pt3 <- st_point(c(xmin, ymax))
pt4 <- st_point(c(xmax, ymax))
# Put together as sf object and get bounding box
bound_box <- st_bbox(st_sfc(pt1, pt3, pt4, pt2, crs = st_crs(GTMNERR)))

#### Google base map ####
# # For now I am getting a Google base map. But for the Shiny app we should use
# # leaflet to make it interactive.
# google_base <- get_map(location = c(lon = -81.347388, lat = 30.075), zoom = 12, maptype = 'roadmap')
# test <- ggmap(google_base)
# test
# 
# st_write(google_base, "shapefiles_new/google_base.shp")

##### Counties #####
counties <- st_read("01_Data_raw/shapefiles/countyshore_areas_sep15/countyshore_areas_sep15.shp")
# CRS: Albers Conical Equal Area
counties <- st_transform(counties, crs = st_crs(GTMNERR))

# Select area -> base on GTMNERR shapefiles
counties_select <- st_crop(counties, bound_box) %>% 
  filter(NAME != "WATER")

ggplot()+
  geom_sf(data=counties_select, fill = NA)+
  geom_sf(data=GTMNERR, fill = "blue", alpha = 0.3)+
  #geom_sf(data = aqua_preserve, fill = NA, color = "red")+
  theme_bw()

st_write(counties_select, "03_Data_for_app/shapefiles_new/counties_GTMNERR.shp")

##### Salt marshes #####
salt_marsh <- st_read("01_Data_raw/shapefiles/salt_marsh_2020/salt_marsh_2020.shp")
# CRS: NAD83(HARN) / Florida GDL Albers
salt_marsh <- st_transform(salt_marsh, crs = st_crs(GTMNERR))
salt_marsh <- st_crop(salt_marsh, bound_box) 
ggplot()+
  geom_sf(data = salt_marsh, fill = "red")+
  geom_sf(data=GTMNERR, fill = "blue", alpha = 0.3)

st_write(salt_marsh, "03_Data_for_app/shapefiles_new/salt_marsh_GTMNERR.shp")

##### Hydrology (several shapefiles) #####
hydro_6 <- st_read("01_Data_raw/shapefiles/nhdwbd_huc6_dec17/nhdwbd_huc6_dec17.shp")
# CRS: Albers Conical Equal Area 
hydro_6 <- st_transform(hydro_6, crs = st_crs(GTMNERR))
hydro_6 <- st_crop(hydro_6, bound_box) 
ggplot()+
  geom_sf(data = hydro_6)

# Transform and crop straightaway (large file)
hydro_8 <- st_crop(st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc8_dec17/nhdwbd_huc8_dec17.shp"),
                                crs = st_crs(GTMNERR)),
                   bound_box) 
ggplot()+
  geom_sf(data = hydro_8)
# About same as 6

hydro_10 <- st_crop(st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc10_dec17/nhdwbd_huc10_dec17.shp"),
                                crs = st_crs(GTMNERR)),
                   bound_box) 
ggplot()+
  geom_sf(data = hydro_10)
# higher resolution

hydro_12 <- st_crop(st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc12_dec17/nhdwbd_huc12_dec17.shp"),
                                 crs = st_crs(GTMNERR)),
                    bound_box) 
ggplot()+
  geom_sf(data = hydro_12)
# even higher

ggplot()+
  geom_sf(data = GTMNERR, fill = NA, color = "blue", size = 3)+
  #geom_sf(data = hydro_6, fill = NA)+
  #geom_sf(data = hydro_10, fill = NA)+
  geom_sf(data = hydro_12, fill = NA)

# Look up differences between HUCs to add to metadata
st_write(hydro_6, "03_Data_for_app/shapefiles_new/nhdwbd_huc6_dec17_GTMNERR.shp")
st_write(hydro_8, "03_Data_for_app/shapefiles_new/nhdwbd_huc8_dec17_GTMNERR.shp")
st_write(hydro_10, "03_Data_for_app/shapefiles_new/nhdwbd_huc10_dec17_GTMNERR.shp")
st_write(hydro_12, "03_Data_for_app/shapefiles_new/nhdwbd_huc12_dec17_GTMNERR.shp")
#### CHECK WARNINGS of some values not being written (too long) #####


#### Water bodies ####
waterbodies <- st_crop(st_read("01_Data_raw/shapefiles/nhd24waterbody_dec17/nhd24waterbody_dec17.shp"), minmax_coords) 

ggplot()+
  geom_sf(data = GTMNERR, fill = NA, color = "blue", size = 3)+
  geom_sf(data = waterbodies, aes(fill = DESCRIPT))

#### Land use ####
landuse1 <- st_crop(st_transform(st_read("01_Data_raw/shapefiles/lu_sjrwmd_2014/lu_sjrwmd_2014.shp"),
                                 crs = st_crs(GTMNERR)),
                    bound_box)

ggplot()+
  geom_sf(data = landuse1, aes(fill = LEVEL1))

st_write(landuse1, "03_Data_for_app/shapefiles_new/lu_sjrwmd_2014_GTMNERR.shp")

#### Mangroves ####
mangrove <- st_crop(st_read("01_Data_raw/shapefiles/mangroves_2020/mangroves_2020.shp"), minmax_coords)

ggplot()+
  geom_sf(data = GTMNERR, fill = "blue", alpha = 0.3)+
  geom_sf(data = mangrove)

#### OFW? #### gdb file




