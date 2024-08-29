########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 01 March 2024

# Initial script to clean/visualize GTMNERR WQ data, before transferring to a 
# Shiny script

library(tidyverse)
library(readxl)

### 1. Read in data -----------------------------------------------------------
WQ <- read_excel("01_Data_raw/Water_Quality/Guana_WQ/Guana_masterdata.xlsx",
                 sheet = 1, # There is only one sheet, but just to be safe
                 guess_max = 13000) # This is not ideal + cols 14 and 16 have a 
# mix of logical and numbers. Lord.
                 
# CHECK if also add the following? PIA because you need to specify *every*
# column data type... Not just the one that is not working. Right now these two
# cols do not show up okay
#col_types = c("SampleDate" = "date", "#RQ" = "text")) # If not specified you get
# warnings (as it expects logical; text only starts after row 1445)

WQ_meta <- read_csv("01_Data_raw/Water_Quality/Guana_WQ/guana_data_dictionary_updateGK.csv")
# Some stations have two codes due to a name change (see Word doc with metadata)
# Don't remove

lookup_names <- read_csv("03_Data_for_app/WQ_lookup_names.csv")

# Change column names so we can later merge this with other WQ data
recode_vec <- setNames(lookup_names$original_name, lookup_names$dashboard_name)
WQ <- WQ %>% 
  rename(any_of(recode_vec))

### 2. Check categorical values ------------------------------------------------
# Check station names, componentLong and componentShort (spelling etc)
unique(WQ$StationCode)
unique(WQ$ComponentShort)
unique(WQ$ComponentLong)
# Looks good

unique(WQ$Flag) # See Word doc for flag meanings:
# Flag	Description
# -4	Outside Low Sensor Range
# -3	Data Rejected due to QAQC
# -2	Missing Data
# -1	Optional SWMP Supported Parameter
# 0	Data Passed Initial QAQC Checks
# 1	Suspect Data
# 4	Historical Data: Pre-Auto QAQC
# 5	Corrected Data

# Remove: -4 and -3. Keep 1
WQ <- WQ %>% 
  filter(!grepl("*<-3>*", Flag),
         !grepl("*<-4>*", Flag))
# From 18967 to 16295
# Separate the flag info (flag and error code; see metadata in Word doc)

unique(WQ$Remark) # inconsistent... But there are some capital letters that
# are associated with data qualifiers in metadata Word doc

### 3. Merge to get coordinates ------------------------------------------------
# Change station code column name so it is the same as the column in the data
names(WQ_meta)[names(WQ_meta) == "station_code"] <- "StationCode"

WQ$StationCode <- str_trim(WQ$StationCode)

WQ <- WQ %>% 
  left_join(WQ_meta, by = c("StationCode")) %>% # not all stations in the original have lat/lon
  mutate(Latitude = coalesce(Latitude.y, Latitude.x),
         Longitude = coalesce(Longitude.y, Longitude.x)) %>% 
  select(-c(Latitude.x, Latitude.y, Longitude.x, Longitude.y))

# Stations missing from metadata: GL1.5, GL2.5 and GL3.5 -> added manually and
# emailed Nikki

# Check
which(is.na(WQ$Latitude))
which(is.na(WQ$Longitude))

# There is GTMMOLNUT and GTMMOLNUT_dup...? I think we can leave it in for now;I give
# them the same site, site acronym and site friendly name...
# And GTMLMNUT is the same? But DEP code? Fix this...

# WQ[which(is.na(WQ$Latitude)),] # duplicates?? 
# WQ <- WQ[-which(is.na(WQ$Latitude)),]

# Create a separate dataframe with only station info, not the data (makes map
# too heavy)
WQ_locations <- WQ %>% 
  mutate(Year = year(SampleDate)) %>% 
  select(site_friendly, Year, site_acronym, Latitude, Longitude, wbid, location) %>% 
  group_by(site_friendly, site_acronym, Latitude, Longitude, wbid, location) %>% 
  summarize(maxYear = max(Year), minYear = min(Year)) %>% 
  mutate(type = "Water quality",
         dataset = "Guana Water Quality Monitoring (GTMNERR)")

WQ_data_available <- WQ %>% 
  mutate(Year = year(SampleDate)) %>% 
  select(StationCode, Year, SampleType, ComponentShort, ComponentLong, site_friendly, 
         site_acronym, Latitude, Longitude, wbid, location) %>% 
  distinct()

### 4. Save data ---------------------------------------------------------------

# Save it as an .Rds file so it can be read into the Shiny app
saveRDS(WQ, "03_Data_for_app/WQ.Rds")

saveRDS(WQ_locations, "03_Data_for_app/WQ_locations.Rds")
