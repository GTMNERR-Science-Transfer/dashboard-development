########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 25 Feb 2025

# Initial script to clean/visualize rain and water level data, before transferring to a 
# Shiny script

# Use list files
# Loop and read in data
# Pick correct columns (2 rainfall and 2 water level)
# Probably pivot to make long
# Add to larger dataset
# Make separate location file with stations and coordinates
# But maybe ask Nikki for photos, instead of using a map?

library(tidyverse)

precip_files <- list.files("01_Data_raw/Hydrology/years/", pattern = ".csv")

all_data <- data.frame()
for (fil in precip_files){
  tst <- read_csv(paste0("01_Data_raw/Hydrology/years/", fil), col_names = FALSE)
  # Fill missing values in the first row (e.g., January, January, February, February)
  tst[1, ] <- t(zoo::na.locf(t(tst[1, ]), na.rm = FALSE))
  # Combine first 2 rows as column names
  colnames(tst) <- paste(tst[1, ], tst[2, ], sep = "_")
  # remove the first two rows, as well as the third row with "Date"
  tst <- tst[-c(1,2,3), ] 
  # Add year as a column
  tst$year <- str_extract(basename(paste0("01_Data_raw/Hydrology/years/", precip_files[1])), "\\d{4}")
  # Change the first col name to day
  colnames(tst)[1] <- "day"
  # Remove the last rows, with Totals and YTD, and NAs (at the bottom)
  tst <- tst %>% 
    filter(day != "Totals", day != "YTD", !is.na(day))
  # Make long so we can start adding everything together
  tst_long <- tst %>% 
    pivot_longer(cols = -c(day, year),
                 names_to = "month_location", 
                 values_to = "value") %>%
    separate(
      col = month_location, 
      into = c("month", "location"), 
      sep = "_", 
      remove = TRUE
    )
  all_data <- bind_rows(all_data, tst_long)
}

tst = read_csv(paste0("01_Data_raw/Hydrology/years/", precip_files[1]), col_names = FALSE)
# Fill missing values in the first row (e.g., January, January, February, February)
tst[1, ] <- t(zoo::na.locf(t(tst[1, ]), na.rm = FALSE))
# Combine first 2 rows as column names
colnames(tst) <- paste(tst[1, ], tst[2, ], sep = "_")
# remove the first two rows, as well as the third row with "Date"
tst <- tst[-c(1,2,3), ] 
# Add year as a column
tst$year <- str_extract(basename(paste0("01_Data_raw/Hydrology/years/", precip_files[1])), "\\d{4}")
# Change the first col name to day
colnames(tst)[1] <- "day"
# Remove the last rows, with Totals and YTD, and NAs (at the bottom)
tst <- tst %>% 
  filter(day != "Totals", day != "YTD", !is.na(day))
# Make long so we can start adding everything together
tst_long <- tst %>% 
  pivot_longer(cols = -c(day, year),
               names_to = "month_location", 
               values_to = "value") %>%
  separate(
    col = month_location, 
    into = c("month", "location"), 
    sep = "_", 
    remove = TRUE
  )


# Clean column names (optional, removes spaces and makes names syntactically valid)
df <- df %>% clean_names()

# View the cleaned data
print(df)
