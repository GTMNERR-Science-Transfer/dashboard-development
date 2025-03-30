########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg
# Email: gklarenberg@ufl.edu
# University of Florida
# Last updated: see commit history

# Script to import and tidy data for use in the Guana Data Dashboard
### Only run this code if any of the RAW DATA or SHAPEFILES has been changed or
# updated

source("global.R") # Load packages needed
source("functions.R") # Load functions needed

# 1. Location Data (for use in maps)
source("02_Cleaning_scripts/Create_location_data.R")

# 2. Water Quality Data
source("02_Cleaning_scripts/WIN_data_clean.R")
source("02_Cleaning_scripts/WQ_GTMNERR.R")
source("02_Cleaning_scripts/WQ_WIN_merge.R")

# 3. Algal Bloom Data
source("02_Cleaning_scripts/HAB_FWC.R")

# 4. Shellfish Data
source("02_Cleaning_scripts/SEACAR_ShellfishData.R")

# 5. Water Levels and Precipitation Data
source("02_Cleaning_scripts/levels_rain_clean.R")

# 6. Shapefiles
source("02_Cleaning_scripts/shapefile_cleanup.R")