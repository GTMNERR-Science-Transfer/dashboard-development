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

WQ <- read_excel("01_Data_raw/Guana_WQ/Guana_masterdata.xlsx")
# warnings about column #RQ; after row 1445, expecting logical but got text (NE-DIST-2018-07-12-02)
# I checked the data and everything before row 1445 is empty hence the expectation
# to get logical. Add code to make that column text when reading in (not
# sure what the column is).

# Check station names, componentLong and componentShort (spelling etc)


# Save it as an .Rdata (and .Rds?) file so it can be read into the Shiny app
save(WQ, file = "03_Data_for_app/WQ.RData")

saveRDS(WQ, "03_Data_for_app/WQ.Rds")