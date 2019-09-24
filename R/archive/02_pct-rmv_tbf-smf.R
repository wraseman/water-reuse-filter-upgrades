# Purpose: calculate percent removal for traveling bed and synthetic media filters and disinfection
# Output: ./data/pct-rmv_tbf-smf.rds
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

# create dataframe for TBF and SMF analysis
tsf.df <- filter(all.df, Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1"), 
                 Parameter != "Flow") %>%
  select(-DateTimeCollected)

## get mean value for all triplicates
## source: https://stackoverflow.com/questions/31215795/removing-duplicate-rows-and-calculate-the-average-in-a-dataframe-in-r
tsf.df <- group_by(tsf.df, DateCollected, Location, Parameter) %>%
  mutate_each(list(mean), -(c(Units, Biodeg, Sorption, ClOxidation))) %>%
  distinct

## spread the data to wide format to calculate percent removals
tsf.wide <- spread(tsf.df, Location, Value)
colnames(tsf.wide)[7:10] <- c("ChlorEffluent", "SMFiltEffluent", "TBFiltEffluent", "TBFiltInfluent")
tsf.wide <- select(tsf.wide, DateCollected, Parameter, TBFiltInfluent, 
                   TBFiltEffluent, SMFiltEffluent, ChlorEffluent,
                   Biodeg, Sorption, ClOxidation, Units)

# create column for chlorination influent
flw.tbf <- 1.71  # based on average flow 1.81 and 1.60 MGD from "Pages from LRD SCADA SCREENS.pdf" 
flw.smf <- 6.25  # based on average flow 6.34 and 6.15 MGD from "Pages from LRD SCADA SCREENS.pdf"
flw.total <- flw.tbf + flw.smf
flwfrc.tbf <- flw.tbf/flw.total  # flow fraction sent to traveling bed filters
flwfrc.smf <- flw.smf/flw.total  # flow fraction sent to synthetic media filters
# flwfrc.tbf <- 0.5  # assume 50% of flow goes to traveling bed filters
# flwfrc.smf <- 0.5  # assume 50% of flow goes to synthetic media filters
tsf.wide <- mutate(tsf.wide,
                   ChlorInfluent = flwfrc.tbf*TBFiltEffluent + flwfrc.smf*SMFiltEffluent)

# calculate % removal due to filtration and chlorination
## note: assume that TBF influent concentration is the same as SMF influent
tsf.wide <- mutate(tsf.wide, 
                   PctRmvSMFilt = (TBFiltInfluent - SMFiltEffluent) / TBFiltInfluent * 100)
tsf.wide <- mutate(tsf.wide, 
                   PctRmvTBFilt = (TBFiltInfluent - TBFiltEffluent) / TBFiltInfluent * 100)
tsf.wide <- mutate(tsf.wide, 
                   PctRmvChlorination = (ChlorInfluent - ChlorEffluent) / ChlorInfluent * 100)

# save cleaned, combined dataframe
data.path <- str_c(data.dir, "pct-rmv_tbf-smf.rds")
write_rds(x=tsf.wide, path=data.path)
