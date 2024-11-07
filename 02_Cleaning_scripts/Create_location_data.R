########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 6 Sep 2024

# Load packages
library(tidyverse)

# Script to create file with locations, for mapping

#### Water Quality ####
# import all WQ data
WQ_df <- readRDS("./03_Data_for_app/WQ_all.Rds")

WQ_years <- WQ_df %>% # site friendly and station code are the names in common
  filter(variable %in% c("StationCode", "site_friendly",
                         "geometry", 
                         "SampleDate", # changed, from StartDate - but also not necessary for locations
                         "Latitude",
                         "Longitude",
                         "data_source")
  ) %>%
  #select(c(RowID, variable, value)) %>% # not necessary
  #distinct(RowID, variable, value) %>% # not necessary
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA)
  ) %>%
  mutate(
    SampleDate = parse_date_time(SampleDate, c("ymd_HMS", "ymd")), # get a warning about 311 failed to parse, but they did, only as ymd
    year = year(SampleDate),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>% 
  group_by(Latitude, Longitude, site_friendly) %>% 
  summarize(minYear = min(year(SampleDate)),
            maxYear = max(year(SampleDate)))

# make dataframe for map display and hover data
WQ_data_locations = WQ_df %>% # site friendly and station code are the names in common
  filter(variable %in% c("StationCode", "site_friendly",
                         "geometry", 
                         "SampleDate", # changed, from StartDate - but also not necessary for locations
                         "Latitude",
                         "Longitude",
                         "data_source")
  ) %>%
  #select(c(RowID, variable, value)) %>% # not necessary
  #distinct(RowID, variable, value) %>% # not necessary
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = list(value = NA)
  ) %>%
  mutate(
    #SampleDate = ymd_hms(SampleDate), # get a warning about 311 failed to parse, but they did, only as ymd
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>% 
  select(-SampleDate) %>% 
  distinct(Latitude, Longitude, geometry, data_source, StationCode, site_friendly)
  
WQ_data_locations <- WQ_data_locations %>% 
  left_join(WQ_years)

# Save data
saveRDS(WQ_data_locations, "03_Data_for_app/WQ_data_locations.Rds")

WQ_data_locations <- WQ_data_locations %>% 
  select(-geometry)

#### Algae ####
HAB_df <- readRDS("03_Data_for_app/HAB.Rds")

# Get min/max years of measurements
HAB_years <- HAB_df %>% 
  mutate(date = dmy(`Sample Date`),
         year = year(date)) %>% 
  group_by(Latitude, Longitude, Site) %>% 
  summarize(minYear = min(year),
            maxYear = max(year)) %>% 
  rename(site_friendly = Site)

HAB_data_locations <- HAB_df %>%
  distinct(Latitude, Longitude, Site) %>% 
  rename(site_friendly = Site) %>% 
  mutate(data_source = "FWC") # or GTMNERR?

HAB_data_locations <- HAB_data_locations %>% 
  left_join(HAB_years)

saveRDS(HAB_data_locations, "03_Data_for_app/HAB_data_locations.Rds")


#### Shellfish ####



#### All - main page ####
# Add dataset type to dfs
WQ_data_locations["type"] <- "Water Quality"
HAB_data_locations["type"] <- "Algae"


# Merge
all_data_locations <- WQ_data_locations %>% 
  full_join(HAB_data_locations)

# Save
saveRDS(all_data_locations, "03_Data_for_app/all_data_locations.Rds")

