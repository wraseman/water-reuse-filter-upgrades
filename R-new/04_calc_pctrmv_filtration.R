# purpose: create dataset for percent removal of CECs and pathogens due to filtration
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in both of Eurofin's labs (California and South Bend)
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
df1 <- read_rds(path = clean.path) 

# calculate percent removal for each analyte due to filtration (assuming concentration of non-detects is the detection limit)
df2 <- df1 %>% 
  filter(Duplicate == FALSE, 
         Triplicate == FALSE) %>%  # remove extra values from triplicate analysis
  filter(Process != "lab_blank") %>% 
  mutate(SampleDate = lubridate::floor_date(SampleDateTime, unit = "6 hours")) %>%  # round datetime to nearest 6 hours
  select(Analyte, SampleDate, Process, DilutAndLimAdjResult, ProcessInfEff, Biodegradation, Sorption)

## create influent and effluent columns for each filter
df3 <- df2 %>%
  group_by(SampleDate, Analyte, ProcessInfEff) %>%
  spread(key = ProcessInfEff, value = DilutAndLimAdjResult) %>%
  filter(Process != "clearwell")  # just look at removal due to filtration for now 

## calculate percent removal
df4 <- df3 %>%
  mutate(PctRmvFilt = ((Influent - Effluent)/Influent) * 100, 
         PctRmvFilt_Sign = if_else(sign(PctRmvFilt) == 1, "Positive",
                                   if_else(sign(PctRmvFilt) == 0, "Zero",
                                           "Negative")))

# save percent removal data
write.df <- df4
ef.dir <- "./data/eurofins-data/"
clean.path <- str_c(ef.dir, "calculated/", "pctrmv-filtration.rds")
write_rds(x=write.df, path=clean.path)