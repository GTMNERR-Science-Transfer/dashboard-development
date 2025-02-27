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
  tst$year <- str_extract(fil, "\\d{4}")
  # Change the first col name to day
  colnames(tst)[1] <- "day"
  # Remove the last rows, with Totals and YTD, and NAs (at the bottom)
  tst <- tst %>% 
    filter(day != "Totals", 
           day != "Total", 
           day != "TOTAL",
           day != "YTD", 
           !is.na(day))
  
  # Make sure that "N.Lot" is written without a space
  
  
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
    ) %>% 
    mutate(location = case_when(location == "N. Lot" ~ "N.Lot",
                                location == "Dam lvl." ~ "Dam_level",
                                TRUE ~ location))
  all_data <- bind_rows(all_data, tst_long)
}

# Check
unique(all_data$day)
unique(all_data$month)
unique(all_data$year)
unique(all_data$value) # contains "trace" and "IRMA". Will be turned into NA

# count NAs
sum(is.na(all_data$day))
sum(is.na(all_data$month))
sum(is.na(all_data$year))
sum(is.na(all_data$value)) #3836

# Create proper dates
all_data <- all_data %>% 
  mutate(year = as.numeric(year),
         day = as.numeric(day),
         value = as.numeric(value)) %>% 
  mutate(date = ymd(paste(year, month, day, sep = "/"))) %>% 
  filter(!is.na(date))
# This gives a warning that some dates were unable to parse, but that is fine
# because those are combinations that don't exist, e.g. 31 November

ggplot(all_data, aes(x = date, y = value, color = location))+
  geom_point()

precip <- all_data %>% 
  filter(location == "N.Lot" | location == "Shop")

dam_level <- all_data %>% 
  filter(location == "Dam_level")

ggplot(precip, aes(x = date, y = value, color = location))+
  geom_point()
ggplot(dam_level, aes(x = date, y = value, color = location))+
  geom_point()

# Check the really high value
dam_level[which(dam_level$value == max(dam_level$value, na.rm = TRUE)),]
# 21 March 2023 (value 35)... Outside hurricane season. Suspect it's wrong
# Checked manually and yes, all month is 3.3, 3.4, 3.5 etc. Replace
dam_level$value[which(dam_level$value == max(dam_level$value, na.rm = TRUE))] <- 3.5
ggplot(dam_level, aes(x = date, y = value, color = location))+
  geom_point()

saveRDS(precip, "03_Data_for_app/precip.Rds")
saveRDS(dam_level, "03_Data_for_app/dam_level.Rds")


