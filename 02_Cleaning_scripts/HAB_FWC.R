########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 27 March 2023

# Initial script to visualize FWC HAB data, before transferring to a 
# Shiny script

library(tidyverse)

HAB <- read_csv("01_Data_raw/Algae/HAB/HAB_FWC.csv")

# Create a file with all unique species names
write_csv(data.frame(Species = unique(HAB$Species)), "03_Data_for_app/HAB_species_list.csv")

# Separate genus and species; add column on type based on lookup table
HAB <- HAB %>% 
  separate_wider_delim(Species, 
                       delim = " ", 
                       names = c("genus"),
                       too_few = "align_start", # if only one word, make species col NA
                       too_many = "drop", # if more than 2 words, ignore the extra words
                       cols_remove = FALSE) 

# Create a file with all unique genus names
write_csv(data.frame(genus = unique(HAB$genus)), "03_Data_for_app/HAB_genus_list.csv")
# I (GK) manually added algal types to the genus file (diatoms, cyanobacteria, 
# dinoflagellates, other)
# Read in this lookup table
lookup_genus <- read_csv("03_Data_for_app/HAB_lookup_genus.csv")

HAB <- HAB %>% 
  left_join(lookup_genus) # Add those types to the main dataset

# Create long format so it can be used in the Shiny app
# Only doing this with numeric variables for now
# HAB <- HAB %>% 
#   pivot_longer(cols = c("cells/L*", "Temperature (C)", "Salinity", 
#                         "DO (%)", "DO (mg/L)", "pH"),
#                names_to = "vars", values_to = "vals")
# HAB$Date <- dmy(HAB$`Sample Date`)

# Save it as an .Rds file so it can be read into the Shiny app
saveRDS(HAB, "03_Data_for_app/HAB.Rds")

