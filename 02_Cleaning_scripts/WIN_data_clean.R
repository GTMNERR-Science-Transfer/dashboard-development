# # Load necessary libraries
# library(sf)
# library(dplyr)
# library(leaflet)
# 
# # Import the CSV data
# win_df <- read.csv("./01_Data_raw/WIN/WIN_data_merged_20240501.csv")
# 
# # Convert the coordinate columns to numeric and remove rows with missing coordinates
# win_df <- win_df %>%
#   mutate(Location_1 = as.numeric(Location_1),
#          Location_2 = as.numeric(Location_2)) %>%
#   filter(!is.na(Location_1) & !is.na(Location_2))
# 
# # Convert the CSV data to a spatial object
# win_sf <- win_df %>%
#   st_as_sf(coords = c("Location_1", "Location_2"), crs = 4326)
# 
# # Import and transform the GTMNERR shapefile, ensuring geometries are valid
# GTMNERR <- st_read("./03_Data_for_app/shapefiles_new/GTMNERR.shp") %>%
#   st_transform(crs = 4326) %>%
#   st_make_valid()
# 
# # Import and transform the counties shapefile, ensuring geometries are valid
# counties_select <- st_read("./03_Data_for_app/shapefiles_new/counties_GTMNERR.shp") %>%
#   st_transform(crs = 4326) %>%
#   st_make_valid()
# 
# # Filter the CSV data to only include locations within the GTMNERR shapefile
# filtered_win_sf_GTMNERR <- win_sf[st_within(win_sf, GTMNERR, sparse = FALSE), ]
# 
# # Filter the CSV data to only include locations within the counties shapefile
# filtered_win_sf_counties <- win_sf[st_within(win_sf, counties_select, sparse = FALSE), ]
# 
# # Convert the filtered spatial objects back to data frames and extract coordinates
# filtered_win_df_GTMNERR <- filtered_win_sf_GTMNERR %>%
#   mutate(Location_1 = st_coordinates(filtered_win_sf_GTMNERR)[,2],
#          Location_2 = st_coordinates(filtered_win_sf_GTMNERR)[,1]) %>%
#   st_drop_geometry() %>%
#   mutate(Location_1 = as.numeric(Location_1),
#          Location_2 = as.numeric(Location_2))
# 
# filtered_win_df_counties <- filtered_win_sf_counties %>%
#   mutate(Location_1 = st_coordinates(filtered_win_sf_counties)[,2],
#          Location_2 = st_coordinates(filtered_win_sf_counties)[,1]) %>%
#   st_drop_geometry() %>%
#   mutate(Location_1 = as.numeric(Location_1),
#          Location_2 = as.numeric(Location_2))
# 
# # Save the filtered data as CSV files
# write.csv(filtered_win_df_GTMNERR, "./01_Data_raw/WIN/filtered_WIN_data_GTMNERR.csv", row.names = FALSE)
# write.csv(filtered_win_df_counties, "./01_Data_raw/WIN/filtered_WIN_data_counties.csv", row.names = FALSE)
# 
# # Create a leaflet map for GTMNERR filtered points with polygon outlines
# map_GTMNERR <- leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = GTMNERR, color = "blue", weight = 2, fillOpacity = 0.2, group = "GTMNERR") %>%
#   addCircleMarkers(data = filtered_win_df_GTMNERR,
#                    lng = ~Location_2, lat = ~Location_1,
#                    color = "blue", radius = 3,
#                    popup = ~paste("Location:", Location_1, Location_2),
#                    group = "Filtered Points") %>%
#   addLayersControl(overlayGroups = c("GTMNERR", "Filtered Points"),
#                    options = layersControlOptions(collapsed = FALSE))
# 
# # Print the map
# map_GTMNERR
# 
# # Create a leaflet map for county filtered points with polygon outlines
# map_counties <- leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = counties_select, color = "red", weight = 2, fillOpacity = 0.2, group = "Counties") %>%
#   addCircleMarkers(data = filtered_win_df_counties,
#                    lng = ~Location_2, lat = ~Location_1,
#                    color = "red", radius = 3,
#                    popup = ~paste("Location:", Location_1, Location_2),
#                    group = "Filtered Points") %>%
#   addLayersControl(overlayGroups = c("Counties", "Filtered Points"),
#                    options = layersControlOptions(collapsed = FALSE))
# 
# # Print the map
# map_counties
# 
# 
# 




# Load necessary libraries
library(sf)
library(dplyr)
library(leaflet)

# Define the distance threshold (25 kilometers)
distance_threshold <- 25000  # 25 kilometers in meters

# Import the CSV data
win_df <- read.csv("./01_Data_raw/WIN/WIN_data_merged_20240501.csv")

# Convert the coordinate columns to numeric and remove rows with missing coordinates
win_df <- win_df %>%
  mutate(Location_1 = as.numeric(Location_1),
         Location_2 = as.numeric(Location_2)) %>%
  filter(!is.na(Location_1) & !is.na(Location_2))

# Convert the CSV data to a spatial object
win_sf <- win_df %>%
  st_as_sf(coords = c("Location_1", "Location_2"), crs = 4326)

# Import and transform the GTMNERR shapefile, ensuring geometries are valid
GTMNERR <- st_read("./03_Data_for_app/shapefiles_new/GTMNERR.shp") %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

# Import and transform the counties shapefile, ensuring geometries are valid
counties_select <- st_read("./03_Data_for_app/shapefiles_new/counties_GTMNERR.shp") %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

# Print bounding boxes to verify the extent of data
print("Bounding box of win_sf:")
print(st_bbox(win_sf))

print("Bounding box of GTMNERR:")
print(st_bbox(GTMNERR))

print("Bounding box of counties_select:")
print(st_bbox(counties_select))

# Plot the original points and polygons
plot(st_geometry(GTMNERR), col = 'blue', main = 'Original GTMNERR and Points')
plot(st_geometry(counties_select), col = 'red', add = TRUE)
plot(st_geometry(win_sf), col = 'green', add = TRUE, pch = 16)

# Transform to a projected CRS for accurate distance calculations
projected_crs <- 32617  # UTM zone 17N

win_sf_proj <- st_transform(win_sf, crs = projected_crs)
GTMNERR_proj <- st_transform(GTMNERR, crs = projected_crs)
counties_select_proj <- st_transform(counties_select, crs = projected_crs)

# Calculate distances from points to the nearest GTMNERR polygon
distances_to_GTMNERR <- st_distance(win_sf_proj, GTMNERR_proj)
min_distances_to_GTMNERR <- apply(distances_to_GTMNERR, 1, min)

# Calculate distances from points to the nearest counties polygon
distances_to_counties <- st_distance(win_sf_proj, counties_select_proj)
min_distances_to_counties <- apply(distances_to_counties, 1, min)

# Debug: Print minimum distances to check if they make sense
print("Minimum distances to GTMNERR:")
print(min_distances_to_GTMNERR)
print("Minimum distances to counties:")
print(min_distances_to_counties)

# Filter points based on the distance threshold
filtered_win_sf_GTMNERR <- win_sf[min_distances_to_GTMNERR <= distance_threshold, ]
filtered_win_sf_counties <- win_sf[min_distances_to_counties <= distance_threshold, ]

# Debug: Check how many points are within the distance
print(paste("Number of points within 25 km of GTMNERR:", nrow(filtered_win_sf_GTMNERR)))
print(paste("Number of points within 25 km of counties:", nrow(filtered_win_sf_counties)))

# Convert the filtered spatial objects back to data frames and extract coordinates
filtered_win_df_GTMNERR <- filtered_win_sf_GTMNERR %>%
  mutate(Location_1 = st_coordinates(filtered_win_sf_GTMNERR)[,2],
         Location_2 = st_coordinates(filtered_win_sf_GTMNERR)[,1]) %>%
  st_drop_geometry() %>%
  mutate(Location_1 = as.numeri
         

