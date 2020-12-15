# purpose: explore relationship between turbidity and pathogens
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets
library(zoo)  # date manipulation
library(GGally)  # scatterplot matrices

filters <- c("SMF+TBF", "DBF")

# read in influent pathogen data
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
pathogen.df1 <- read_rds(path = clean.path) %>%
  select(SampleDateTime, Analyte, Result, Detect, DetectionLimit, ProcessInfEff, LocationProcessType) %>%
  filter(Analyte %in% c("Giardia", "Cryptosporidium")) %>%
  mutate(date = as.Date(SampleDateTime))  # convert from datetime to date data type

# read in influent turbidity data 
particle.dir <- "./data/tss-turbidity/"
particles.df1 <- read_rds(path = str_c(particle.dir, "TSS-turbidity.rds")) %>%
  select(date, turb_postfilt_SCADA_daily_avg) %>%
  mutate(date = as.Date(date))  # convert from datetime to date data type

# find co-incident pathogen and turbidity data for filter effluent
df1 <- inner_join(pathogen.df1 %>% 
                    filter(ProcessInfEff == "Effluent") %>%  # process effluent
                    filter(LocationProcessType != "Final"),  # remove measurements from after contact tank (keep only filter effluent)
                  particles.df1, by="date") %>%
  rename(Turbidity = turb_postfilt_SCADA_daily_avg)

df2 <- select(df1, date, Analyte, Result, LocationProcessType, Turbidity) %>%
  pivot_wider(names_from = Analyte, values_from = Result)
  # pivot_longer(cols = c("Turbidity", "Cryptosporidium", "Giardia"), names_to = "Analyte", values_to = "Value")

ggplot(df2, aes(x = Turbidity, y = Cryptosporidium)) + 
  geom_point(aes(color = LocationProcessType))

ggplot(df2, aes(x = Turbidity, y = Giardia)) + 
  geom_point(aes(color = LocationProcessType))
