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
WQ <- read_excel("01_Data_raw/Guana_WQ/Guana_masterdata.xlsx",
                 sheet = 1, # There is only one sheet, but just to be safe
                 col_types = c("#RQ" = "text")) # If not specified you get
# warnings (as it expects logical; text only starts after row 1445)

WQ_meta <- read_csv("01_Data_raw/Guana_WQ/guana_data_dictionary_updateGK.csv")
# Some stations have two codes due to a name change (see Word doc with metadata)
# Don't remove

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

WQ <- WQ %>% 
  left_join(WQ_meta) %>% 
  select(-lat, -long)

# Stations missing from metadata: GL1.5, GL2.5 and GL3.5 -> emailed Nikki

### 4. Save data ---------------------------------------------------------------

# Save it as an .Rdata (and .Rds?) file so it can be read into the Shiny app
save(WQ, file = "03_Data_for_app/WQ.RData")

saveRDS(WQ, "03_Data_for_app/WQ.Rds")
