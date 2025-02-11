#### Import data and packages ####

# Libraries
library(sf)
library(tidyverse)

# Data
# From Nikki Dix
GTMNERR <- st_read("03_Data_for_app/shapefiles_new/counties_GTMNERR.shp")
# CRS: NAD83 / UTM zone 17N
# WIN Data

# Already select which variables we want straightaway. Saves resources. Pick 
# things that have "Org" in the name because those are the units/names/etc 
# submitted by the sampling organization - which should be the GTM for this (?)
gps_data <- read_csv("./01_Data_raw/Water_Quality/WIN/WIN_data_merged_20240501.csv",
                     col_select = c(`Monitoring Location ID`, `Activity Type`, 
                                    `Activity Start Date Time`, `Activity Depth`,
                                    `DEP Result ID`, `Org Analyte Name`, `Org Result Value`,
                                    `Org Result Unit`, `Org MDL`, `RowID`, `LocationID`,
                                    `Station ID`, `Station Name`, `Station Type`, `County`,
                                    `Location_1`, `Location_2`))

lookup_names <- read_csv("03_Data_for_app/WQ_lookup_names.csv")

# Change column names so we can later merge this with other WQ data
recode_vec <- setNames(lookup_names$original_name, lookup_names$dashboard_name)
gps_data <- gps_data %>% 
  rename(any_of(recode_vec))

#### GTMNERR boundary and aquatic preserves ####

# Transform to WGS84, EPGS 4326
GTMNERR <- st_transform(GTMNERR, crs = 4326)

# Make valid 
# This has to be done if some polygons overlap
GTMNERR <- st_make_valid(GTMNERR)

# Check what the polygons are
ggplot()+
  geom_sf(data = GTMNERR) # Hmmm

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

# Filter GPS coordinates
# Convert to sf object
gps_sf <- st_as_sf(gps_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Crop GPS points within the bounding box
gps_cropped <- st_crop(gps_sf, bound_box) 

# Plot the results
ggplot() +
  geom_sf(data = GTMNERR, fill = NA, color = "blue") +
  geom_sf(data = gps_cropped, color = "red") +
  ggtitle("Filtered GPS Points within GTMNERR Bounding Box")

# View the filtered GPS coordinates
print(gps_cropped)

# Convert the cropped GPS points back to a dataframe and retain original data
WIN_df <- as.data.frame(gps_cropped)

# Extract coordinates (latitude and longitude) from the geometry column
coordinates <- st_coordinates(gps_cropped)

# Combine the original data without the geometry column and the coordinates
WIN_df <- cbind(WIN_df, coordinates)

# Rename the coordinates columns if necessary
colnames(WIN_df)[(ncol(WIN_df)-1):ncol(WIN_df)] <- c("Longitude", "Latitude")

#### Keep only columns with varying information ####
# Function to remove columns with the same value in the whole column
# remove_constant_columns <- function(df) {
#   df <- df[, sapply(df, function(col) length(unique(col)) > 1)]
#   return(df)
# }
# 
# WIN_df <- remove_constant_columns(WIN_df)

#### Reformat data to visualize easily ####
# turn WIN_df into long format with the following columns
# [dates, location_GPS, key, value] 
# visualize only unique locations

# Convert all columns to character before pivoting and retain the original row identifier
WIN_df <- WIN_df %>%
  # Add a column to record the data source/provider
  mutate(data_source = "WIN") %>% # or change this to DEP?
  mutate(across(everything(), as.character)) %>%
  mutate_all(~ na_if(., "")) %>%
  pivot_longer(
    cols =  -c(RowID), # Exclude the Row_ID column from pivoting
    names_to = "variable",
    values_to = "value"
  ) 
  # filter(!is.na(value) & value != "") # use this if space is an issue

#### Save data ####
# Save the filtered data to a new CSV file
# write.csv(WIN_df, 
#           "03_Data_for_app/Filtered_WIN_data_merged_20240501.csv", 
#           row.names = FALSE)
# Save the filtered data to a .RDs file

saveRDS(WIN_df, "03_Data_for_app/WIN.Rds")
