# Purpose: read in and clean deep bed filtration data from Loxahatchee plant. 
# Output: ./data/cleaned_dbf-data.rds (tidy, cleaned dataframe)
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in raw data from deep bed filters
data.dir <- "./data/"
filename <- "LBR DBF raw data.xlsx"
data.path <- str_c(data.dir, filename)
raw.df <- read_excel(path = data.path)

# clean and reformat data
column.names <- c("eea_id", "location", "sample_type", "collect_dttm", 
                  "receive_dttm", "exctract_dttm", "analyze_dttm", 
                  "method", "analyte", "cas_number", "matrix", 
                  "result_flag", "result", "units", "mrl", "dilution_factor")

colnames(raw.df) <- column.names

## keep columns and analytes necessary for analysis
analytes <- c("Cryptosporidium", "Giardia",  # pathogens
              "Meprobamate", "Dilantin", "Sulfamethoxazole", 
              "Tris(2-carboxyethyl)phosphine hydrochloride",
              "Tris(1,3-dichloro-2-propyl) phosphate",
              "Carbamazepine", "Salicylic Acid", 
              "Gemfibrozil", "Ibuprofen", "Acetaminophen",
              "Bisphenol-A", "Triclosan", "Caffeine", "Fluoxetine")
clean.df <- select(raw.df, collect_dttm, location, analyte, result, units) %>%
  filter(analyte %in% analytes) %>% # remove unecessary analytes from dataframe
  filter(location %in% c("DBF-E", "DBF-I", "Final"))

# save dataframe
clean.path <- str_c(data.dir, "cleaned_dbf-data.rds")
write_rds(x=clean.df, path=clean.path)


