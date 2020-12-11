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
df2 <- select(df1, SampleDateTime, Analyte, Result, Detect, DetectionLimit, ProcessInfEff, LocationProcessType)

## Table 1: Statistics for TrOCs and pathogens in treatment plant influent for data associated with TBF and SMF
## columns: Analytes	Analyte Type	Min	Max	Median	Mean	Standard Deviation	MRL	Count <MRL

### calculate summary statistics
tbl1.df1 <- df2 %>% 
  filter(LocationProcessType == "TBF-I") %>%
  group_by(Analyte) %>%
  summarize(SampleSize = n(),
            Min = min(Result, na.rm = TRUE),
            Max = max(Result, na.rm = TRUE),
            Median = median(Result, na.rm = TRUE),
            Mean = mean(Result, na.rm = TRUE),
            SD = sd(Result, na.rm = TRUE),
            MRL = min(DetectionLimit, na.rm = TRUE))  # using minimum reported detection limit because that is instrument limit (not adjusted for dilution)

### calculate nondetects
tbl1.df2 <- select(df2, LocationProcessType, Analyte, Detect) %>%
  filter(LocationProcessType == "TBF-I") %>%
  filter(Detect == FALSE) %>%
  group_by(Analyte) %>% 
  count(name = "CountLessThanMRL") 

tbl1.df3 <- full_join(tbl1.df1, tbl1.df2) %>%
  mutate(CountLessThanMRL = if_else(is.na(CountLessThanMRL), 0, CountLessThanMRL %>% as.double)) %>%
  select(Analyte, MRL, SampleSize, CountLessThanMRL, Min, Max, Median, Mean, SD)  # rearrange columns
  
# Table 2. Statistics for TrOCs and pathogens in treatment plant effluent for data associated with TBF and SMF
tbl2.df1 <- df2 %>% 
  filter(LocationProcessType %in% c("TBF-E", "SMF-E")) %>%
  group_by(LocationProcessType, Analyte) %>%
  summarize(SampleSize = n(),
            Min = min(Result, na.rm = TRUE), 
            Max = max(Result, na.rm = TRUE),
            Median = median(Result, na.rm = TRUE),
            Mean = mean(Result, na.rm = TRUE), 
            SD = sd(Result, na.rm = TRUE), 
            MRL = min(DetectionLimit, na.rm = TRUE))  # using minimum reported detection limit because that is instrument limit (not adjusted for dilution)

### calculate nondetects
tbl2.df2 <- select(df2, LocationProcessType, Analyte, Detect) %>%
  filter(LocationProcessType %in% c("TBF-E", "SMF-E")) %>%
  filter(Detect == FALSE) %>%
  group_by(LocationProcessType, Analyte) %>%
  count(name = "CountLessThanMRL") 

tbl2.df3 <- full_join(tbl2.df1, tbl2.df2) %>%
  filter(LocationProcessType %in% c("TBF-E", "SMF-E")) %>%
  mutate(CountLessThanMRL = if_else(is.na(CountLessThanMRL), 0, CountLessThanMRL %>% as.double)) %>%
  select(LocationProcessType, Analyte, MRL, SampleSize, CountLessThanMRL, Min, Max, Median, Mean, SD)  # rearrange columns

# Table 3. Statistics for TrOCs and pathogens in treatment plant influent for data associated with DBF
### calculate summary statistics
tbl3.df1 <- df2 %>% 
  filter(LocationProcessType == "DBF-I") %>%
  group_by(Analyte) %>%
  summarize(SampleSize = n(),
            Min = min(Result, na.rm = TRUE),
            Max = max(Result, na.rm = TRUE),
            Median = median(Result, na.rm = TRUE),
            Mean = mean(Result, na.rm = TRUE),
            SD = sd(Result, na.rm = TRUE),
            MRL = min(DetectionLimit, na.rm = TRUE))  # using minimum reported detection limit because that is instrument limit (not adjusted for dilution)

### calculate nondetects
tbl3.df2 <- select(df2, LocationProcessType, Analyte, Detect) %>%
  filter(LocationProcessType == "DBF-I") %>%
  filter(Detect == FALSE) %>%
  group_by(Analyte) %>% 
  count(name = "CountLessThanMRL") 

tbl3.df3 <- full_join(tbl3.df1, tbl3.df2) %>%
  mutate(CountLessThanMRL = if_else(is.na(CountLessThanMRL), 0, CountLessThanMRL %>% as.double)) %>%
  select(Analyte, MRL, SampleSize, CountLessThanMRL, Min, Max, Median, Mean, SD)  # rearrange columns

# Table 4. Statistics for TrOCs and pathogens in treatment plant effluent for data associated with DBF
tbl4.df1 <- df2 %>% 
  filter(LocationProcessType == "DBF-E") %>%
  group_by(LocationProcessType, Analyte) %>%
  summarize(SampleSize = n(),
            Min = min(Result, na.rm = TRUE), 
            Max = max(Result, na.rm = TRUE),
            Median = median(Result, na.rm = TRUE),
            Mean = mean(Result, na.rm = TRUE), 
            SD = sd(Result, na.rm = TRUE), 
            MRL = min(DetectionLimit, na.rm = TRUE))  # using minimum reported detection limit because that is instrument limit (not adjusted for dilution)

### calculate nondetects
tbl4.df2 <- select(df2, LocationProcessType, Analyte, Detect) %>%
  filter(LocationProcessType == "DBF-E") %>%
  filter(Detect == FALSE) %>%
  group_by(LocationProcessType, Analyte) %>%
  count(name = "CountLessThanMRL") 

tbl4.df3 <- full_join(tbl4.df1, tbl4.df2) %>%
  filter(LocationProcessType == "DBF-E") %>%
  mutate(CountLessThanMRL = if_else(is.na(CountLessThanMRL), 0, CountLessThanMRL %>% as.double)) %>%
  select(LocationProcessType, Analyte, MRL, SampleSize, CountLessThanMRL, Min, Max, Median, Mean, SD)  # rearrange columns
