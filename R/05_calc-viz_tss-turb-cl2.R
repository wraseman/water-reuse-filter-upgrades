# Purpose: investigate impact of new filters TSS, turbidity, and chlorine
# Author: William Raseman

# clear environment
rm(list = ls())

# set working directory and turn off warnings for Knitting
setwd("C:/Users/wraseman/Hazen and Sawyer/Stanford, Benjamin - Loxahatchee DBF Evaluation/Data Analysis - Billy")
options(warn=-1)

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # import data from Excel

# get date ranges for Phase 1 (SMF and TBF) and Phase 2 (DBF)
data.dir <- "./data/"
filename1 <- "LRD all filters raw data.xlsx"
path1 <- str_c(data.dir, filename1)
df1 <- read_excel(path = path1, sheet = "Data") %>% 
  select(Phase, DateTimeCollected) %>%
  na.omit()

phase1.range <- filter(df1, Phase == "Phase 1")$DateTimeCollected %>%
  range() %>% as.Date()
phase2.range <- filter(df1, Phase == "Phase 2")$DateTimeCollected %>%
  range() %>% as.Date()

# read in chlorine data from Biowin dataset
biowin.dir <- str_c(data.dir, "Biowin Study Data/")
filename2 <- "3 Calibration Data and Plots - wjr edits.xlsx"
path2 <- str_c(biowin.dir, filename2)
df2 <- read_excel(path = path2, sheet = "Data", range = "A3:IV1402")

df2.cols <- c("Date",
              "PLANT INFLUENT pH - SCADA",
              "PLANT EFFLUENT pH - SCADA",
              "PLANT EFFLUENT pH 15 MIN - SCADA",
              "PLANT EFFLUENT pH 15 MAX - SCADA",
              "PLANT EFFLUENT FLOW - SCADA",
              "PLANT INFLUENT FLOW 15 MAX - SCADA",
              "PLANT INFLUENT FLOW 15 MIN - SCADA",
              "PLANT EFFLUENT FLOW 15 MAX - SCADA",
              "PLANT EFFLUENT FLOW 15 MIN - SCADA",
              "Raw Alkalinity",
              "CL2 Feed CCC",
              "CL2 FEED RATE PER MG",
              "CL2 Feed Filters",
              "CL2 SCALE 1 WEST",
              "CL2 SCALE 2 EAST",
              "CHLORINE RESIDUAL - SCADA",
              "CHLORINE RESIDUAL 15 MAX - SCADA",
              "CHLORINE RESIDUAL 15 MIN - SCADA",
              "Raw SS",
              "TSS in",
              "After Filter TSS Grab",
              "After Filter TSS Grab Meter Rdng",
              "TSS - SCADA",	
              "TSS 15 MAX - SCADA",
              "TSS 15 MIN - SCADA",
              "TURBIDITY - SCADA",
              "TURBIDITY 15 MAX - SCADA",
              "TURBIDITY 15 MIN - SCADA")

df2 <- select(df2, df2.cols)

new.colnames <- c("date", 
                  "inf_pH_SCADA", 
                  "eff_pH_SCADA", 
                  "eff_pH_SCADA_15min",
                  "eff_pH_SCADA_15max",
                  "eff_flow_SCADA_mgd",
                  "inf_flow_SCADA_15max_MGD",
                  "inf_flow_SCADA_15min_MGD",
                  "eff_flow_SCADA_15max_MGD",
                  "eff_flow_SCADA_15min_MGD",
                  "raw_alk_mg-L",
                  "cl2_feed_CCC_lbsperday",
                  "cl2_feed_rateperMG_lbsperday", 
                  "cl2_feed_filters_lbsperday",
                  "cl2_scale_1_west_lbsperday",
                  "cl2_scale_2_east_lbsperday",
                  "cl2_resid_SCADA_mgperL",
                  "cl2_resid_SCADA_15max_mgperL",
                  "cl2_resid_SCADA_15min_mgperL",
                  "raw_SS_mgperL",
                  "inf_TSS_unknown_units",
                  "postfilt_TSS_grab_mgperL",
                  "postfilt_TSS_grab_meter_reading_mgperL",
                  "postfilt_TSS_SCADA_mgperL",
                  "postfilt_TSS_SCADA_15max_mgperL",
                  "postfilt_TSS_SCADA_15min_mgperL",
                  "postfilt_turb_SCADA_ntu",
                  "postfilt_turb_SCADA_15max_ntu",
                  "postfilt_turb_SCADA_15min_ntu")

colnames(df2) <- new.colnames

df2 <- mutate(df2, date = as.Date(date),
              phase = if_else(between(date, min(phase1.range), max(phase1.range)), "Phase 1", 
                              if_else(between(date, min(phase2.range), max(phase2.range)), "Phase 2", NA_character_)),
              cl2_feed_to_residual = cl2_feed_rateperMG_lbsperday/cl2_resid_SCADA_mgperL)

# visualize data

## suspended solids analysis
ggplot(data = df2, aes(x=date, y=raw_SS_mgperL)) +
  geom_point(aes(color=phase)) +
  ylab("Raw Suspended Solids (mg/L)") +
  xlab("Date")

# ggplot(data = df2, aes(x=date, y=inf_TSS_unknown_units)) +
#   geom_point(aes(color=phase)) +
#   ylab("Influent TSS (units?)") +
#   xlab("Date")

ggplot(data = df2, aes(x=date, y=postfilt_TSS_grab_mgperL)) +
  geom_point(aes(color=phase)) +
  ylab("Post-filter TSS (mg/L)") +
  xlab("Date") + 
  ggtitle("Grab Sample")

ggplot(data = df2, aes(x=date, y=postfilt_TSS_SCADA_mgperL)) +
  geom_point(aes(color=phase)) +
  ylab("Post-filter TSS (mg/L)") +
  xlab("Date") + 
  ggtitle("SCADA")

## turbidity analysis
ggplot(data = df2, aes(x=date, y=postfilt_turb_SCADA_ntu)) +
  geom_point(aes(color=phase)) +
  ylab("Post-filter Turbidity (ntu)") +
  xlab("Date") + 
  ggtitle("SCADA")

## chlorine analysis
ggplot(data = df2, aes(x=date, y=cl2_feed_rateperMG_lbsperday)) +
  geom_point(aes(color=phase)) +
  ylab("Chlorine Feed Rate per Day (lbs/day)") +
  xlab("Date") 

ggplot(data = df2, aes(x=date, y=cl2_resid_SCADA_mgperL)) +
  geom_point(aes(color=phase)) +
  ylab("Chlorine Residual - SCADA (mg/L)") +
  xlab("Date") 

ggplot(data = df2, aes(x=date, y=cl2_feed_to_residual)) +
  geom_point(aes(color=phase)) +
  ylab("Chlorine Feed to Residual Ratio (lbs/day / mg/L)") +
  xlab("Date") 

# turn warnings back on
options(warn=0)
