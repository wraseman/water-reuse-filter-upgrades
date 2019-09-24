# Purpose: read in, clean data, and combine data for all filters from Loxahatchee plant. 
#   Data includes pathogen and contaminants of emerging concern (CEC) concentrations, 
#   turbidity, and loading rates.
# Output: ./data/cleaned_all-filters.rds (tidy, cleaned dataframe)
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in contaminant (pathogens and CEC) removal data
data.dir <- "./data/"
raw.filename <- "LRD all filters raw data.xlsx"
raw.path <- str_c(data.dir, raw.filename)
pc.raw <- read_excel(path = raw.path, sheet = "Data")  # pathogen and CEC data

## keep columns and analytes necessary for analysis
analytes <- c("Cryptosporidium", "Giardia",  # pathogens
              "Meprobamate", "Dilantin", "Sulfamethoxazole", 
              "Tris(2-carboxyethyl)phosphine hydrochloride",
              "Tris(1,3-dichloro-2-propyl) phosphate",
              "Carbamazepine", "Salicylic Acid", 
              "Gemfibrozil", "Ibuprofen", "Acetaminophen",
              "Triclosan", "Caffeine", "Fluoxetine")
# "Bisphenol A", "Triclosan", "Caffeine", "Fluoxetine")  # Bisphenol A only collected in Phase 2

pc.df <- select(pc.raw, DateTimeCollected, DateCollected, LOCGeneral, 
                Analyte, Result, Units, Result_Flag, MRL) %>%
  rename(Location = LOCGeneral) %>%  
  filter(Analyte %in% analytes) %>%  # remove unecessary analytes from dataframe
  filter(Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1",
                         "DBF-E", "DBF-I", "Final Ph2"))

### fill out Result_Flag information for Phase 1 data
pc.df <- mutate(pc.df, Detect = ifelse(MRL < Result, TRUE, FALSE)) %>%
  select(-Result_Flag)

# read in flow and chlorine data
fc.raw <- read_excel(path = raw.path, sheet = "FlowCl")  # flow and chlorine (mg/L) by date

## reformat data (to "tidy" format)
fc.df <- gather(data = fc.raw, "Parameter", "Value", -DateCollected) %>% 
  mutate(Parameter = ifelse(Parameter == "FlowMGD", "Flow", "Chlorine")) %>%
  mutate(Units = ifelse(Parameter == "Flow", "MGD", "mg/L"))

# read in turbidity data
tb.raw <- read_excel(path = raw.path, sheet = "Turbidity")  # turbidity (ntu) date
tb.df <- select(tb.raw, DateCollected, LOCGeneral, Analyte, Result, Units) %>%
  rename(Location = LOCGeneral)  # rename to match column names of pc.df

# combine pathogen, CEC, flow, chlorine, and turbidity data
comb.df <- bind_rows(pc.df, tb.df) %>%  # combine pathogen, CEC, and turbidity
  rename(Parameter = Analyte) %>%  # rename "Analyte" column to "Parameter" to account for flow data
  rename(Value = Result)  %>%  # rename "Result" column to "Value"
  bind_rows(fc.df)  # add flow and chlorine data

# add biodegradation, sorption, and chlorine oxidation categories
## biodegradation category
biodeg.poor <- c("Meprobamate", "Dilantin", "Sulfamethoxazole", 
                 "Tris(1,3-dichloro-2-propyl) phosphate", 
                 "Tris(2-carboxyethyl)phosphine hydrochloride", 
                 "Carbamazepine")
biodeg.good <- c("Salicylic Acid", "Gemfibrozil", "Ibuprofen",
                 "Acetaminophen", "Bisphenol A", "Triclosan", 
                 "Caffeine", "Fluoxetine")

## sorption category
sorption.poor <- c("Meprobamate", "Dilantin", "Sulfamethoxazole", 
                   "Salicylic Acid", "Gemfibrozil", "Ibuprofen")
sorption.good <- c("Tris(1,3-dichloro-2-propyl) phosphate", 
                   "Tris(2-carboxyethyl)phosphine hydrochloride", 
                   "Carbamazepine", 
                   "Acetaminophen", "Bisphenol A", "Triclosan", 
                   "Caffeine", "Fluoxetine")

## chlorine oxidation category
chlroxid.good <- c("Salicylic Acid", "Caffeine", "Sulfamethoxazole", 
                   "Acetaminophen", "Triclosan")
chlroxid.mod <- c("Gemfibrozil")
chlroxid.poor <- c("Carbamazepine", "Meprobamate", "Fluoxetine", 
                   "Ibuprofen", 
                   "Tris(1,3-dichloro-2-propyl) phosphate", 
                   "Tris(2-carboxyethyl)phosphine hydrochloride",
                   "Dilantin")

comb.df <- comb.df %>%
  mutate(Biodeg = ifelse(Parameter %in% biodeg.poor, "Poor", 
                         ifelse(Parameter %in% biodeg.good, "Good", 
                                NA))) %>%
  mutate(Sorption = ifelse(Parameter %in% sorption.poor, "Poor", 
                           ifelse(Parameter %in% sorption.good, "Good", 
                                  NA))) %>%
  mutate(ClOxidation = ifelse(Parameter %in% chlroxid.poor, "Poor", 
                              ifelse(Parameter %in% chlroxid.mod, "Moderate", 
                                     ifelse(Parameter %in% chlroxid.good, "Good",
                                            NA)))) 
# save cleaned, combined dataframe
comb.path <- str_c(data.dir, "cleaned_all-filters.rds")
write_rds(x=comb.df, path=comb.path)
