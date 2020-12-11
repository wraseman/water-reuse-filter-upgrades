# Purpose: visualize summary statistics for TrOCs (i.e., CECs) and pathogens
# Author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# function purpose: convert numeric (less than 100) into two character string
## input: num (numeric)
## return: two_char_string (string)
## example: num = 1 would return "01"
## example: num = 12 would return "12"
num_to_two_char_string <- function (num) {
  if (num < 10) {
    two_char_string <- str_c("0", num)  # if less than 10, add a 0 in front of the value
  } else {
    two_char_string <- str_c(num)
  }
  return(two_char_string)
}

# read in cleaned data from lab samples tested in Eurofin's California lab
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
df1 <- read_rds(path = clean.path) 

# create summary based on dilution adjusted results
## the reason that "DilutAdjResult" is used over the "Result" is that before running the 
## samples, they were diluted. Therefore, a dilution of 10x means that the "Result" is 
## actually 10x less than true value. 
df2 <- select(df1, SampleDateTime, Analyte, DilutAdjResult, Detect, DetectionLimit, ProcessInfEff, LocationProcessType)

## Table 1: Statistics for TrOCs and pathogens in treatment plant influent for data associated with TBF and SMF
## columns: Analytes	Analyte Type	Min	Max	Median	Mean	Standard Deviation	MRL	Count <MRL

### calculate summary statistics
tbl1.df1 <- df2 %>% 
  filter(LocationProcessType == "TBF-I") %>%
  group_by(Analyte) %>%
  summarize(Min = min(DilutAdjResult, na.rm = TRUE), 
            Max = max(DilutAdjResult, na.rm = TRUE),
            Median = median(DilutAdjResult, na.rm = TRUE),
            Mean = mean(DilutAdjResult, na.rm = TRUE), 
            SD = sd(DilutAdjResult, na.rm = TRUE), 
            MRL = min(DetectionLimit, na.rm = TRUE))

### calculate nondetects
tbl1.df2 <- select(df2, Analyte, Detect) %>%
  filter(Detect == FALSE) %>%
  group_by(Analyte) %>% 
  count(name = "Count <MRL")

tbl1.df3 <- inner_join(tbl1.df1, tbl1.df2)
  
# Table 2. Statistics for TrOCs and pathogens in treatment plant effluent for data associated with TBF and SMF
# 
# Table 3. Statistics for TrOCs and pathogens in treatment plant influent for data associated with DBF
# 
# Table 4. Statistics for TrOCs and pathogens in treatment plant effluent for data associated with DBF
