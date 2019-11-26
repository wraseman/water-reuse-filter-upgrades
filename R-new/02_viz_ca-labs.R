# Purpose: read in and clean data from California laboratory results
#   Data includes pathogen and contaminants of emerging concern (CEC) concentrations
# Output: ./data/cleaned_all-filters.rds (tidy, cleaned dataframe)
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in data from lab samples tested in Eurofin's California lab
ef.dir <- "./data/eurofins-data/"
ca.dir <- str_c(ef.dir, "california-lab-results/")
