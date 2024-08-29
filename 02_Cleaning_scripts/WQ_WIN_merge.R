########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 12 August 2024

# Load packages
library(tidyverse)

# Merge WIN and WQ data into one

WIN <- readRDS("03_Data_for_app/WIN.Rds")
WQ_GTMNERR <- readRDS("03_Data_for_app/WQ.Rds")

# Make WQ_GTMNERR long format as well, just like WIN
# Having a column for the dates is advisable though, as it is a separate data
# type. Update that later, not right now.

#### We need to update the variable list so the names are the same!

# First make sure that every row has a UNID and also add a column for the data
# source / provider
WQ_GTMNERR <- WQ_GTMNERR %>% 
  mutate(data_source = "GTMNERR") %>% 
  arrange(UNID, StationCode, SampleDate, ComponentShort)

for (i in 1:nrow(WQ_GTMNERR)){
  if (is.na(WQ_GTMNERR$UNID[i])){
    WQ_GTMNERR$UNID[i] <- WQ_GTMNERR$UNID[i-1] + 1
  }
}
# Check there are no duplicates
sum(duplicated(WQ_GTMNERR$UNID))

# Add a geometry column, to later use for clicking markers (we might change this)
WQ_GTMNERR <- st_as_sf(WQ_GTMNERR, coords = c("Longitude", "Latitude"), 
                       crs = 4326, remove = FALSE)
# Turn back into dataframe with geometry as a column
WQ_GTMNERR <- as.data.frame(WQ_GTMNERR)

WQ_GTMNERR_long <- WQ_GTMNERR %>% 
  mutate(across(everything(), as.character)) %>%
  mutate_all(~ na_if(., "")) %>% 
  pivot_longer(cols = -UNID, 
               names_to = "variable",
               values_to = "value")

# How to deal with UNID when merging? Start counting anew (or add however far the
# one dataset is?)

min(as.numeric(WQ_GTMNERR_long$UNID)) # 1
max(as.numeric(WQ_GTMNERR_long$UNID)) # 17098 

min(as.numeric(WIN$RowID)) # 55135
max(as.numeric(WIN$RowID)) # 3677602
# Appears there will be no overlap. Merge.

WQ_GTMNERR_long <- WQ_GTMNERR_long %>% 
  rename(RowID = UNID)

WQ_all <- WIN %>% 
  full_join(WQ_GTMNERR_long)

unique(WQ_all$variable)
unique(WQ_all$value)

# Read in the WQ vars lookup table and replace variables with the names we need
lookup_WQ_vars <- read_csv("03_Data_for_app/WQ_lookup_variables.csv")

# # I am sure there is a nicer/quicker/tidyverse way of doing this, but whatevs for now
# for (i in 1:nrow(WQ_all)){
#   if (is.na(WQ_all$value[i])){
#     next
#   }
#   for (j in 1:nrow(lookup_WQ_vars)){
#     if (WQ_all$value[i] == lookup_WQ_vars$value[j]){
#       WQ_all$value[i] <- lookup_WQ_vars$new[j]
#     }
#   }
# }

# Quicker way
WQ_all <- WQ_all %>%
  left_join(lookup_WQ_vars, by = "value") %>%
  mutate(value = coalesce(new, value)) %>%
  select(-new)

# Filter for only the things that we need? Save for later?

# Save data
saveRDS(WQ_all, "03_Data_for_app/WQ_all.Rds")
