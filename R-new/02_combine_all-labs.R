# purpose: combine dataframes from California and South Bend datasets; clean data where necessary
# author: Billy Raseman 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in Eurofin's California lab
clean.dir <- "./data/eurofins-data/clean/"
ca.path <- str_c(clean.dir, "california-lab-results_clean.rds")
sb.path <- str_c(clean.dir, "southbend-lab-results_clean.rds")
ca.df1 <- read_rds(path = ca.path) %>%
  mutate(Lab = "California")
sb.df1 <- read_rds(path = sb.path)  %>%
  mutate(Lab = "SouthBend")

# check whether columns match between dataframes
## check column names
ca.cols <- colnames(ca.df1) %>% sort
sb.cols <- colnames(sb.df1) %>% sort
all(ca.cols == sb.cols)

## check column types
ca.class <- sapply(ca.df1, class)
sb.class <- sapply(sb.df1, class)

## convert SampleID to numeric
ca.df2 <- ca.df1 %>%
  mutate(SampleID = as.numeric(SampleID))

# combine data into a single dataframe
comb.df1 <- bind_rows(ca.df2, sb.df1) %>%
  mutate(ProcessInfEff = fct_explicit_na(ProcessInfEff))  # make missing values explicit (https://forcats.tidyverse.org/reference/fct_explicit_na.html)

## if measured values are less than the detection limit, set value to detection limit
comb.df2 <- comb.df1 %>%
  mutate(LimitAdjResult = if_else(is.na(Result), DetectionLimit,   # if Result is "NA", set value to Detection Limit
                                  if_else(Result < DetectionLimit, DetectionLimit,  # if Result is less than Detection Limit, set to limit
                                          Result))) %>% # otherwise, keep Result the same
  mutate(Detect = if_else(LimitAdjResult == DetectionLimit, FALSE, TRUE)) %>%  # if Result was adjusted to Detection Limit, specify that that was a non-detect
  mutate(DilutAndLimAdjResult = LimitAdjResult * Dilution)  # adjust for dilution

# save dataframe
write.df <- comb.df1
ef.dir <- "./data/eurofins-data/"
clean.path <- str_c(ef.dir, "clean/", "combined-lab-results_clean.rds")
write_rds(x=write.df, path=clean.path)
