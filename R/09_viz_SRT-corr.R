# Purpose: visualize relationship between SRT and turbidity, TSS, pathogens, and CECs
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr, and lubridate for data wrangling and visualization
library(readxl)  # read Excel spreadsheets
library(zoo)  # datetime and time series library
# source("./R/lib/spread_lib.R")  # spread with duplicate values (https://github.com/tidyverse/tidyr/issues/474)

# # read in data
# data.dir <- "./data/"
# rds.filename <- "cleaned_all-filters+SRT.rds"
# rds.path <- str_c(data.dir, rds.filename)
# df1 <- read_rds(path = rds.path) %>%
#   mutate(DateCollected = lubridate::as_date(DateCollected)) %>%  # convert DateCollected to "date" type
#   select(-DateTimeCollected)
# 
# # find and remove redundant values due to triplicates
# duplicate.rows <- select(df1, DateCollected, Parameter) %>% 
#   duplicated
# df2 <- df1[!duplicate.rows,] 
# df3 <- intersect(df1, df2) %>%
#   spread(key="Parameter", value="Value") 

# read in data
data.dir <- "./data/"
rds.filename <- "TSS-turbidity_7day-avg+SRT.rds"
rds.path <- str_c(data.dir, rds.filename)
df1 <- read_rds(path = rds.path)

# visualize correlations between SRT and other parameters
## TSS
ggplot(df1, aes(y=weekly_rollmean_TSS_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date, shape=phase)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Total Suspended Solids (mg/L), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average")

## turbidity
ggplot(df1, aes(y=weekly_rollmean_turb_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date, shape=phase)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Turbidity (ntu), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average")

## TSS by Phase
ggplot(df1, aes(y=weekly_rollmean_TSS_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Total Suspended Solids (mg/L), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average") +
  facet_wrap(phase ~ .)

## turbidity by Phase
ggplot(df1, aes(y=weekly_rollmean_turb_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Turbidity (ntu), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average") +
  facet_wrap(phase ~ .)


