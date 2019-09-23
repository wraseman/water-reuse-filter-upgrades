# Purpose: calculate percent removal for deep bed filters and disinfection
# Output: ./data/pct-rmv_dbf.rds
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization

# read in cleaned data for all filters
## dataset contains information on pathogen, contaminants of emerging concern (CEC), 
##  turbidity, and chlorine concentrations and flow rates
data.dir <- "./data/"
filename <- "cleaned_all-filters.rds"
data.path <- str_c(data.dir, filename)
all.df <- read_rds(path = data.path) 

# create dataframe for deep bed filter (DBF) analysis
dbf.df <- filter(all.df, Location %in% c("DBF-E", "DBF-I", "Final Ph2"), 
                 Parameter != "Flow") %>%
  select(-DateTimeCollected)

## get mean value for all triplicates
## source: https://stackoverflow.com/questions/31215795/removing-duplicate-rows-and-calculate-the-average-in-a-dataframe-in-r
dbf.df <- group_by(dbf.df, DateCollected, Location, Parameter) %>% 
  mutate_each(list(mean), -(c(Units, Biodeg, Sorption, ClOxidation))) %>% 
  distinct

## match units for Giardia and Cryptosporidium for all locations.
##  some measures are oocysts (or cysts) per Liter and others per 100 Liters
dbf.units <- mutate(dbf.df, 
                 Value = ifelse(Units == "oocysts/L", 
                                        Value*100,
                                        Value), 
                 Units = ifelse(Units == "oocysts/L",
                                "oocysts/100L", 
                                Units)) %>%
  mutate(Value = ifelse(Units == "cysts/L", 
                        Value*100, 
                        Value), 
         Units = ifelse(Units == "cysts/L", 
                        "cysts/100L",
                        Units)
         )

## spread the data to wide format to calculate percent removals
dbf.wide <- spread(dbf.units, Location, Value)
colnames(dbf.wide)[7:9] <- c("FilterEffluent", "FilterInfluent", "ChlorEffluent")
dbf.wide <- select(dbf.wide, DateCollected, Parameter, FilterInfluent, 
                   FilterEffluent, ChlorEffluent,
                   Biodeg, Sorption, ClOxidation, Units)

# calculate % removal due to filtration and chlorination
dbf.wide <- mutate(dbf.wide, 
                   PctRmvFilter = (FilterInfluent - FilterEffluent) / FilterInfluent * 100)
dbf.wide <- mutate(dbf.wide, 
                   PctRmvChlorination = (FilterEffluent - ChlorEffluent) / FilterEffluent * 100)

# save cleaned, combined dataframe
data.path <- str_c(data.dir, "pct-rmv_dbf.rds")
write_rds(x=dbf.wide, path=data.path)
