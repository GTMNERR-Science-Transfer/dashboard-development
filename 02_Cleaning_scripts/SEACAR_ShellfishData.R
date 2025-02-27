# Chad Palmer
# palmer.cr@ufl.edu
# 7 February 2024

# Initial script to visualize SEACAR Shellfish data, before transferring to a shiny script

library(tidyverse)
shellData <- read_csv("01_Data_raw/Shellfish/Reef_SEACAR.csv")
###This script is currently being used for reef locations and cover percent
###NAs in percentages are converted to 0s and a "data not available" column added so that
#cover percents (should) always add to 100%
#Note that the current process is to average across transects for a given reef on a given day, but this could change
shellData <- shellData %>%
  mutate(across(21:25, ~ replace_na(.x, 0))) #This is all of the %Cover columns
shellData <- shellData %>%
  mutate(`NoDataCover_%` = 100 - rowSums(shellData[, 21:25])) #Some of these sum to more than 100% (This is an issue)

saveRDS(shellData, "03_Data_for_app/reefs.Rds")

###This data is currently being used for oyster, barnacle, and muscle counts only
###Clam, crab, and gastropod data will need conversions for NA values (gastropods are in presence/absence)
countData <- read_csv("01_Data_raw/Shellfish/counts.csv")
saveRDS(countData, "03_Data_for_app/counts.Rds")
