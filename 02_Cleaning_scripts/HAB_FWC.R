########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 27 March 2023

# Initial script to visualize FWC HAB data, before transferring to a 
# Shiny script

library(tidyverse)

HAB <- read_csv("01_Data_raw/HAB/HAB_FWC.csv")

# Save it as an .Rdata file so it can be read into the Shiny app
save(HAB, file = "03_Data_for_app/HAB.RData")

saveRDS(HAB, "03_Data_for_app/HAB.Rds")



# Create long format so it can be used in the Shiny app
# Only doing this with numeric variables for now
HAB <- HAB %>% 
  pivot_longer(cols = c("cells/L*", "Temperature (C)", "Salinity", 
                        "DO (%)", "DO (mg/L)", "pH"),
               names_to = "vars", values_to = "vals")
HAB$Date <- dmy(HAB$`Sample Date`)

save(HAB, file = "03_Data_for_app/HAB.RData")

# Check all unique categories
unique(HAB$Description)
unique(HAB$Site) # Also get coordinates
unique(HAB$`Collection Agency`)
unique(HAB$County)
#unique(HAB$Proofed)
unique(HAB$Date)
unique(HAB$vars)

# Histogram
hist(HAB[which(HAB$vars == "Temperature (C)"),"vals"], breaks = 6, col = 'darkgray', border = 'white',
     xlab = 'Temperature (degrees Celsius)',
     main = 'Histogram of water temperatures')

ggplot(HAB %>% filter(vars == "Temperature (C)"), aes(x = vals))+
  geom_histogram(bins = 10)

# Algae counts over time
# NOTE: some only have "present" but no count
# Make a list of algae that have counts, and those that only have "present"



algae_present <- HAB %>% filter(Description == "present")
algae_count <- HAB %>% filter(Description != "present")

algae_time <- ggplot(HAB %>% filter(vars == "cells/L*"), aes(x = Date, y = vals,
                                               color = Species))+
  geom_point()+
  theme(legend.position = "none")

algae_time


# Algae counts overall


# Algae vs other variables
ggplot(HAB %>% filter(vars == "cells/L*"))



