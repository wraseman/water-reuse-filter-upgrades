# Purpose: read in and visualize SRT data
# Output: ./data/cleaned_all-filters+SRT.rds and ./data/TSS-turbidity+SRT.rds and ./data/TSS-turbidity_7day-avg+SRT.rds
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr, and lubridate for data wrangling and visualization
library(readxl)  # read Excel spreadsheets
library(zoo)  # datetime and time series library

# read in solids retention time (SRT) data
data.dir <- "./data/"
raw.filename <- "Plant SRT 2013-2019.xlsx"
raw.path <- str_c(data.dir, raw.filename)
srt.raw <- read_excel(path = raw.path)

# clean data
## calculate 7-day moving average for SRT
df.srt <- srt.raw %>%
  mutate(SRT_7day_avg = rollmean(SRT_days, k = 7, na.pad=TRUE, align="right"))

# visualize SRT data
p1 <- ggplot(df.srt, aes(x=date, y=SRT_days)) +
  geom_point() +
  ylab("Solids Retention Time (Days)") +
  xlab("Date")

## zoom in on visualization
p2 <- p1 + ylim(0, 3)

## incorporate moving average
p2 +
  geom_line(aes(y=SRT_7day_avg, color="red"), size=0.5) +
  labs(color = "Moving Average") +
  scale_color_manual(labels = c("7-day"), values = c("red"))

# add SRT data to cleaned_all-filters
df.rds <- read_rds(str_c(data.dir, "cleaned_all-filters.rds"))
long.srt <- df.srt %>%
  rename(DateCollected = date) %>%  # match column names in "cleaned_all-filters.rds"
  gather(key="Parameter", value="Value", -DateCollected, -SRT_7day_avg) %>%
  mutate(Units = "days")
df.comb1 <- bind_rows(long.srt, df.rds)

# add SRT data to TSS and turbidity data
df.tssturb <- read_rds(str_c(data.dir, "TSS-turbidity.rds"))
df.comb2 <- inner_join(df.tssturb, select(df.srt, -SRT_7day_avg))

df.tssturbavg <- read_rds(str_c(data.dir, "TSS-turbidity_7day-avg.rds"))
df.comb3 <- inner_join(df.tssturbavg, select(df.srt, -SRT_days))

# export full datasets
write_rds(x=df.comb1, str_c(data.dir, "cleaned_all-filters+SRT.rds"))
write_rds(x=df.comb2, str_c(data.dir, "TSS-turbidity+SRT.rds"))
write_rds(x=df.comb3, str_c(data.dir, "TSS-turbidity_7day-avg+SRT.rds"))
