########################################################################
########## NERRS Science Transfer project - GTMNERR        #############
########################################################################

# Geraldine Klarenberg, PhD
# gklarenberg@ufl.edu
# 24 Jan 2025

# Script for testing/playing with HAB data viz

library(tidyverse)
library(plotly)
HAB <- readRDS("./03_Data_for_app/HAB.Rds")

###############################################

# Plotting experiments

# Richness (number of species)
HAB %>% 
  filter(type == "Diatoms",
         Site == "Guana Lake") %>% 
  group_by(`Sample Date`, `Sample Time`, genus) %>% 
  count() # species richness, number of genera detected (types are based on genera)

# For numeric data - bar plots, by genus NOT IDEAL
HAB %>% 
  filter(type == "Diatoms",
         Site == "Guana Lake",
         !is.na(`cells/L*`))  %>% 
  mutate(date = dmy(`Sample Date`)) %>% 
  group_by(date, `Sample Time`, genus) %>% 
  summarize(total_genus = sum(`cells/L*`)) %>% 
  ggplot(aes(x = date, y = total_genus, fill = genus)) +
  geom_col(position = "stack") +
  scale_y_log10()

# "Balloon" plots (this way zeros are also shown) - NOT by genus
# Daily
type_choice = c("Diatoms", "Cyanobacteria")
site_choice = c("Guana Lake", "Crescent Beach")

p <- HAB %>% 
  filter(type %in% type_choice,
         Site %in% site_choice,
         !is.na(`cells/L*`))  %>% 
  mutate(date = dmy(`Sample Date`)) %>% 
  mutate(Site_type = paste(Site, type, sep = " - ")) %>% 
  group_by(Site, date, `Sample Time`, type, Site_type) %>% 
  summarize(total = sum(`cells/L*`)) %>% 
  ggplot(aes(x = date, y = total, color = type)) +
  geom_segment(aes(x = date, xend = date, y = 0, yend = total)) +
  geom_point(size = 2, pch = 1) +
  labs(x = "", y = "Total cells/liter") +
  theme_bw() +
  facet_wrap(
    ~ Site_type, 
    ncol = 1, 
    scales = "free_y"
  ) +
  theme(
    strip.text = element_text(size = 12), # Adjust strip text size
    strip.placement = "outside",         # Place strips outside plot area
    strip.background = element_rect(fill = NA),
    legend.position = "none"
  )

gp <- ggplotly(p,
         dynamicTicks = TRUE) %>% 
  layout(margin = list(r = 50, l=70)) # Add more margin space to the left and the right
# Set the y-axis label position (more to the left)    
gp[["x"]][["layout"]][["annotations"]][[1]][["x"]] <- -0.02

gp

# So in app: filter for site and type. Add type to y-axis. If done like WQ, maybe
# show more sites or variables (types)? Maybe start with doing types?
# Or do something like Shannon did: facet_wraps, add a facet if there are more than 1
# station selected/


# For numeric data by month, average (add error bars?)
HAB %>% 
  filter(type == "Diatoms",
         Site == "Guana Lake; Ponte Vedra",
         !is.na(`cells/L*`))  %>% 
  mutate(date = dmy(`Sample Date`),
         month = month(date),
         year = year(date)) %>% 
  group_by(genus, month, year) %>% 
  summarize(ave_genus = mean(`cells/L*`)) %>% 
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>% 
  ggplot(aes(x = date, y = ave_genus, fill = genus)) +
  geom_col(position = "stack") +
  scale_y_log10() +
  theme_bw()

# For non-numeric data
HAB %>% 
  filter(type == "Diatoms",
         Site == "Guana Lake",
         !is.na(Description))  %>% 
  group_by(`Sample Date`, `Sample Time`, genus) 



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

