########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 27 March 2023

# Initial script to visualize FWC HAB data, before transferring to a 
# Shiny script

library(tidyverse)

HAB <- read_csv("../Data/Datasets/HAB_FWC.csv")

# Save it as an .Rdata file so it can be read into the Shiny app
save(HAB, file = "HAB.RData")

saveRDS(HAB, "HAB.Rds")



# Create long format so it can be used in the Shiny app
# Only doing this with numeric variables for now
HAB <- HAB %>% 
  pivot_longer(cols = c("cells/L*", "Temperature (C)", "Salinity", 
                        "DO (%)", "DO (mg/L)", "pH"),
               names_to = "vars", values_to = "vals")
save(HAB, file = "HAB.RData")

# Histogram
hist(HAB[which(HAB$vars == "Temperature (C)"),"vals"], breaks = 6, col = 'darkgray', border = 'white',
     xlab = 'Temperature (degrees Celsius)',
     main = 'Histogram of water temperatures')

ggplot(HAB %>% filter(vars == "Temperature (C)"), aes(x = vals))+
  geom_histogram(bins = 10)
