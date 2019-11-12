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
df1 <- read_rds(path = rds.path) %>%
  mutate(date = as.Date(date))  # technically, this data is datetime data. Convert to "date" type

# visualize timeseries of TSS
parameter.names <- c('SRT_7day_avg'= "SRT (days)", 
                     'weekly_rollmean_TSS_effluent' = "Effluent TSS (mg/L)", 
                     'weekly_rollmean_turb_effluent' = "Effluent Turbidity (ntu)")

ggplot(gather(df1, key = "parameter", value = "value", -date, -phase), 
       aes(x = date, y = value)) +
  geom_point(aes(color=parameter)) +
  xlab("Date") +
  ylab("Value") +
  ggtitle("7-day Rolling Average Data") +
  facet_grid(parameter ~ ., labeller = as_labeller(parameter.names))  # https://stackoverflow.com/questions/3472980/how-to-change-facet-labels/34811062#34811062

# visualize correlations between SRT and other parameters
## TSS
ggplot(df1, aes(y=weekly_rollmean_TSS_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date, shape=phase)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Total Suspended Solids (mg/L), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average")

## TSS by Phase
ggplot(df1, aes(y=weekly_rollmean_TSS_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Total Suspended Solids (mg/L), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average") +
  facet_wrap(phase ~ .)

# find dates of anomolous events and look at correlations without these events
## Phase 1 - events #1 and #2 investigation
df1 %>% filter(phase == 1, SRT_7day_avg > 2)
### event #1: 2014-11-09 to 11-15
### event #2: 2015-10-15 to 10-21

## Phase 2 - event #3 investigation
df1 %>% filter(weekly_rollmean_TSS_effluent > 1.75, phase == 2) %>%
  ggplot(aes(x=date, y=weekly_rollmean_TSS_effluent)) +
  geom_point()  
### event #3: 2018-08-02 to 09-02

df2 <- df1 %>% 
  mutate(datetype = as.Date(date)) %>%
  filter(!between(datetype, as.Date("2014-11-09"), as.Date("2014-11-15"))) %>%  # event #1
  filter(!between(datetype, as.Date("2015-10-15"), as.Date("2015-10-21"))) %>%  # event #2
  filter(!between(datetype, as.Date("2018-08-02"), as.Date("2018-09-02")))  # event #3

ggplot(df2, aes(y=weekly_rollmean_TSS_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Total Suspended Solids (mg/L), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average") +
  facet_wrap(phase ~ .) +
  ggtitle("Anomolous events removed")


## turbidity
ggplot(df1, aes(y=weekly_rollmean_turb_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date, shape=phase)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Turbidity (ntu), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average")

## turbidity by Phase
ggplot(df1, aes(y=weekly_rollmean_turb_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Turbidity (ntu), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average") +
  facet_wrap(phase ~ .)

## turbidity by Phase (anomolous events removed)
ggplot(df2, aes(y=weekly_rollmean_turb_effluent, x=SRT_7day_avg)) +
  geom_point(aes(color=date)) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  ylab("Effluent Turbidity (ntu), 7-Day Average") +
  xlab("Solids Retention Time (days), 7-Day Average") +
  facet_wrap(phase ~ .) +
  ggtitle("Anomolous events removed")



