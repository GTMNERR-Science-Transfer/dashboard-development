# Load necessary libraries
library(sf)
library(dplyr)
library(leaflet)

# Import the filtered data points
filtered_win_df_GTMNERR <- st_read("./01_Data_raw/WIN/filtered_WIN_data_GTMNERR.csv")
filtered_win_df_counties <- st_read("./01_Data_raw/WIN/filtered_WIN_data_counties.csv")

# Create a leaflet map for GTMNERR filtered points
map_GTMNERR <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = filtered_win_df_GTMNERR,
                   lng = ~Location_2, lat = ~Location_1,
                   color = "blue", radius = 3,
                   popup = ~paste("Location:", Location_1, Location_2))

# Print the map
map_GTMNERR

# Create a leaflet map for county filtered points
map_counties <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = filtered_win_df_counties,
                   lng = ~Location_2, lat = ~Location_1,
                   color = "red", radius = 3,
                   popup = ~paste("Location:", Location_1, Location_2))

# Print the map
map_counties
