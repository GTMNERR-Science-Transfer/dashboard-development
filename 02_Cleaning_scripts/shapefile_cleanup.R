########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 29 March 2024
# Updated 01 May 2024

# Cleaning up (modifying) shapefiles needed

library(sf)
library(tidyverse)

#### GTMNERR boundary and aquatic preserves ####
# From Nikki Dix
GTMNERR <- st_read("01_Data_raw/shapefiles/GTMNERR Boundary_query update 2021/GTM_RB_2016_Merge (1).shp")
# CRS: NAD83 / UTM zone 17N

# Transform to WGS84, EPGS 4326
GTMNERR <- st_transform(GTMNERR, crs = 4326)

# Make valid 
# This has to be done if some polygons overlap
GTMNERR <- st_make_valid(GTMNERR)

# Check what the 8 polygons are
ggplot()+
  geom_sf(data = GTMNERR, aes(fill = as.factor(Area_ha))) # Hmmm

# Keep only the one large shapefile
GTMNERR_all <- GTMNERR %>% filter(Area_ha == 0 & Res_Name == "Guana Tolomato Matanzas")

# Keep only the small polygons, but remove the part that's in the sea
GTMNERR_small <- GTMNERR %>% filter(Area_ha > 0 & Area_ha < 10000)

ggplot()+
  geom_sf(data = GTMNERR_small, aes(fill = as.factor(Area_ha)))

st_write(GTMNERR_small, "03_Data_for_app/shapefiles_new/GTMNERR_small_nosea.shp", append = FALSE)
st_write(GTMNERR_all, "03_Data_for_app/shapefiles_new/GTMNERR.shp", append = FALSE)

# Get min/max coordinates for selecting from other shapefiles
st_bbox(GTMNERR)
# UTM is in meters. Add/subtract 5 km (more or less 0.05 degrees)
xmin <- st_bbox(GTMNERR)$xmin - 0.05
xmax <- st_bbox(GTMNERR)$xmax + 0.05
ymin <- st_bbox(GTMNERR)$ymin - 0.05
ymax <- st_bbox(GTMNERR)$ymax + 0.05
# Create points from these coordinates
pt1 <- st_point(c(xmin, ymin))
pt2 <- st_point(c(xmax, ymin))
pt3 <- st_point(c(xmin, ymax))
pt4 <- st_point(c(xmax, ymax))
# Put together as sf object and get bounding box
bound_box <- st_bbox(st_sfc(pt1, pt3, pt4, pt2, crs = st_crs(GTMNERR)))

##### Counties #####
counties <- st_read("01_Data_raw/shapefiles/countyshore_areas_sep15/countyshore_areas_sep15.shp")
# CRS: Albers Conical Equal Area
counties <- st_transform(counties, crs = st_crs(GTMNERR))
# Make valid (this takes a short while to run)
# This has to be done if some polygons overlap
counties <- st_make_valid(counties)

# Select counties within which GTMNERR falls
counties_select <- st_intersection(counties, GTMNERR)
counties_keep <- unique(counties_select$NAME)
counties_keep <- counties_keep[counties_keep != "WATER"]
counties_select <- counties %>% 
  filter(NAME %in% counties_keep)

ggplot()+
  geom_sf(data=counties_select, fill = NA)+
  geom_sf(data=GTMNERR, fill = "blue", alpha = 0.3)+
  theme_bw()

st_write(counties_select, "03_Data_for_app/shapefiles_new/counties_GTMNERR.shp",
         append = FALSE)
# Ignore warnings when writing data: checked saved shapefile and it is correct

##### Salt marshes #####
salt_marsh <- st_read("01_Data_raw/shapefiles/salt_marsh_2020/salt_marsh_2020.shp")
# CRS: NAD83(HARN) / Florida GDL Albers -> for visualization this is not a problem,
# for consistency I am still changing the CRS
salt_marsh <- st_transform(salt_marsh, crs = st_crs(GTMNERR))
salt_marsh <- st_make_valid(salt_marsh)
salt_marsh <- st_crop(salt_marsh, bound_box) 
ggplot()+
  geom_sf(data=GTMNERR, fill = "blue", alpha = 0.3)+
  geom_sf(data = salt_marsh, fill = "red", color = "red")

# This one is not ideal as it lots of small shapefiles, but I do not really want 
# to turn it into one big one (not sure if that will work)
st_write(salt_marsh, "03_Data_for_app/shapefiles_new/salt_marsh_GTMNERR.shp",
         append = FALSE)

##### Hydrology (several shapefiles) #####
hydro_6 <- st_read("01_Data_raw/shapefiles/nhdwbd_huc6_dec17/nhdwbd_huc6_dec17.shp")
# CRS: Albers Conical Equal Area 
hydro_6 <- st_transform(hydro_6, crs = st_crs(GTMNERR))
hydro_6 <- st_intersection(hydro_6, GTMNERR) 
ggplot()+
  geom_sf(data = GTMNERR_all, fill = "red")+
  geom_sf(data = hydro_6, fill = NA)
# GTMNERR is all within the same HUC6 (is high level). No need to save this
  
  geom_sf(data = hydro_6[1,], fill = NA, color = "red")+
  geom_sf(data = hydro_6[2,], fill = NA, color = "green")+
  geom_sf(data = hydro_6[3,], fill = NA, color = "blue")+
  geom_sf(data = hydro_6[4,], fill = NA, color = "hotpink")+
  geom_sf(data = hydro_6[5,], fill = NA, color = "yellow")+
  geom_sf(data = hydro_6[6,], fill = NA, color = "orange")+
  geom_sf(data = hydro_6[7,], fill = NA, color = "black")+
  geom_sf(data = hydro_6[8,], fill = NA, color = "black")

# Transform and crop straightaway (large file)
hydro_8_select <- st_intersection(st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc8_dec17/nhdwbd_huc8_dec17.shp"),
                                crs = st_crs(GTMNERR)),
                   GTMNERR) 
hydro_8_keep <- unique(hydro_8_select$NAME)
hydro_8_select <- st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc8_dec17/nhdwbd_huc8_dec17.shp"),
                               crs = st_crs(GTMNERR)) %>% 
  filter(NAME %in% hydro_8_keep)

ggplot()+
  geom_sf(data = hydro_8_select)
# About same as 6, no need to save

hydro_10_select <- st_crop(st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc10_dec17/nhdwbd_huc10_dec17.shp"),
                                crs = st_crs(GTMNERR)),
                   GTMNERR) 
hydro_10_keep <- unique(hydro_10_select$NAME)
hydro_10_select <- st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc10_dec17/nhdwbd_huc10_dec17.shp"),
                               crs = st_crs(GTMNERR)) %>% 
  filter(NAME %in% hydro_10_keep)

ggplot()+
  geom_sf(data = hydro_10_select)
# higher resolution!

hydro_12_select <- st_crop(st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc12_dec17/nhdwbd_huc12_dec17.shp"),
                                 crs = st_crs(GTMNERR)),
                    GTMNERR)
hydro_12_keep <- unique(hydro_12_select$NAME)
hydro_12_select <- st_transform(st_read("01_Data_raw/shapefiles/nhdwbd_huc12_dec17/nhdwbd_huc12_dec17.shp"),
                                crs = st_crs(GTMNERR)) %>% 
  filter(NAME %in% hydro_12_keep)
ggplot()+
  geom_sf(data = hydro_12_select)
# even higher

ggplot()+
  geom_sf(data = GTMNERR, fill = NA, color = "blue", size = 3)+
  #geom_sf(data = hydro_6, fill = NA)+
  #geom_sf(data = hydro_10, fill = NA)+
  geom_sf(data = hydro_12_select, fill = NA)

# Look up differences between HUCs to add to metadata
st_write(hydro_10_select, "03_Data_for_app/shapefiles_new/nhdwbd_huc10_dec17_GTMNERR.shp", append = FALSE)
st_write(hydro_12_select, "03_Data_for_app/shapefiles_new/nhdwbd_huc12_dec17_GTMNERR.shp", append = FALSE)
#### CHECK WARNINGS of some values not being written (too long) #####


#### Water bodies ####
waterbodies <- st_read("01_Data_raw/shapefiles/nhd24waterbody_dec17/nhd24waterbody_dec17.shp")
# metadata/info: https://geodata.dep.state.fl.us/datasets/FDEP::florida-national-hydrography-dataset-nhd-waterbodies-24k/about
# https://www.arcgis.com/sharing/rest/content/items/121cffac550f4937a98caf615454b595/info/metadata/metadata.xml?format=default&output=html

# Transform to WGS84, EPGS 4326 (original is Albers Conical Equal Area)
waterbodies <- st_transform(waterbodies, crs = 4326)
# You will get warnings along the lines of:
# GDAL Message 1: Sub-geometry 1 has coordinate dimension 2, but container has 3
# Can be ignored: this shapefile also has a third (Z) dimension, this will
# automatically be dropped in the next functions

# This dataframe contains over half a million observations (shapefiles), I tried
# to run st_make_valid on it, but it takes forever (and it needs to be done to use
# st_crop). I let it run for hours and it never finished.
# Instead I am going to to use the bound_box coordinates (enlarged) to select
# the area needed
# UTM is in meters. Add/subtract 25 km (more or less 0.25 degrees)
xmin2 <- st_bbox(GTMNERR)$xmin - 0.25
xmax2 <- st_bbox(GTMNERR)$xmax + 0.25
ymin2 <- st_bbox(GTMNERR)$ymin - 0.25
ymax2 <- st_bbox(GTMNERR)$ymax + 0.25

# Extract lat and lon from the geometry column, and use those to make a smaller 
# sf dataframe (but keep the geometry column so it is still a proper sf dataframe!)
waterbodies_small <- waterbodies %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  filter(lon >= xmin2 && lon <= xmax2,
         lat >= ymin2 && lat <= ymax2)

########## Does not work; takes too long. 06/20/2024. Make separate issue on Github
# to deal with this
# Make valid 
# This has to be done if some polygons overlap
time1 <- Sys.time()
waterbodies <- st_make_valid(waterbodies)
time2 <- Sys.time()
time2 - time1
# This takes a loooong time 

waterbodies <- st_crop(waterbodies, bound_box) 

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
mangrove <- st_read("01_Data_raw/shapefiles/mangroves_2020/mangroves_2020.shp")
mangrove <- st_transform(mangrove, crs = st_crs(GTMNERR))
mangrove <- st_make_valid(mangrove)

mangrove_select <- st_intersection(mangrove, GTMNERR)

ggplot()+
  geom_sf(data = GTMNERR, fill = "blue", color = NA, alpha = 0.3)+
  geom_sf(data = mangrove_select, fill = "green", color = NA)

st_write(mangrove_select, "03_Data_for_app/shapefiles_new/mangrove_GTMNERR.shp",
         append = FALSE)

#### OFW: Outstanding Florida Waters #### gdb file
ofw <- st_read("01_Data_raw/shapefiles/ofw_other_apr23.gdb")
ofw <- st_transform(ofw, crs = st_crs(GTMNERR))
ofw <- st_make_valid(ofw)

ofw_select <- st_intersection(ofw, GTMNERR)

ggplot()+
  geom_sf(data = GTMNERR, fill = "blue", color = NA, alpha = 0.3)+
  geom_sf(data = ofw_select, fill = "darkred", color = NA)

st_write(mangrove_select, "03_Data_for_app/shapefiles_new/ofw_GTMNERR.shp",
         append = FALSE)
