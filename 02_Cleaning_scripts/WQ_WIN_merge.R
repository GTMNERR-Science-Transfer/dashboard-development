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

# First make sure that every row has a UNID
WQ_GTMNERR <- WQ_GTMNERR %>% 
  arrange(UNID, StationCode, SampleDate, ComponentShort)

for (i in 1:nrow(WQ_GTMNERR)){
  if (is.na(WQ_GTMNERR$UNID[i])){
    WQ_GTMNERR$UNID[i] <- WQ_GTMNERR$UNID[i-1] + 1
  }
}
# Check there are no duplicates
sum(duplicated(WQ_GTMNERR$UNID))

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

saveRDS(WQ_all, "03_Data_for_app/WQ_all.Rds")