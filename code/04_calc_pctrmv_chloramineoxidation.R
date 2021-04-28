# purpose: create dataset for percent removal of TrOCs due to Chloramine Oxidation
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in both of Eurofin's labs (California and South Bend)
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
df1 <- read_rds(clean.path) 

# calculate percent removal for each analyte due to filtration (assuming concentration of non-detects is the detection limit)
df2 <- df1 %>%
  filter(Duplicate == FALSE,
         Triplicate == FALSE) %>%  # remove extra values from triplicate analysis
  filter(Process != "lab_blank") %>%
  mutate(SampleDate = lubridate::floor_date(SampleDateTime, unit = "6 hours")) %>%  # round datetime to nearest 6 hours
  select(Analyte, SampleDate, Process, LimitAdjResult, ProcessInfEff, ChloramineOxidation)

## create influent and effluent columns for each process type
##  source: https://tidyr.tidyverse.org/dev/articles/pivot.html
df3 <- df2 %>%
  filter(ProcessInfEff == "Effluent") %>%
  unite(ProcessWithInfEff, Process, ProcessInfEff) %>%
  group_by(SampleDate, Analyte) %>%
  spread(key = ProcessWithInfEff, value = LimitAdjResult)

## create combined effluent for SMF and TBF (because these effluents mix before going into the clearwell)
Q.tbf <- 1.71  # based on average flow 1.81 and 1.60 MGD from "Pages from LRD SCADA SCREENS.pdf"
Q.smf <- 6.25  # based on average flow 6.34 and 6.15 MGD from "Pages from LRD SCADA SCREENS.pdf"
Q.total <- Q.tbf + Q.smf
Q.frc.tbf <- Q.tbf/Q.total  # flow fraction sent to traveling bed filters
Q.frc.smf <- Q.smf/Q.total  # flow fraction sent to synthetic media filters

df4 <- df3 %>%
  mutate(SMFTBF_Eff_Estimated = Q.frc.tbf*TBF_Effluent + Q.frc.smf*SMF_Effluent)

## calculate percent removal due to Chloramine Oxidation
df5 <- df4 %>%
  mutate(PctRmvChlormaineOx_SMFTBF = ((SMFTBF_Eff_Estimated - clearwell_Effluent)/SMFTBF_Eff_Estimated) * 100) %>%
  # PctRmvChlormaineOxSign_SMFTBF = if_else(sign(PctRmvChlormaineOx_SMFTBF) == 1, "Positive",
  #                           if_else(sign(PctRmvChlormaineOx_SMFTBF) == 0, "Zero",
  #                                   "Negative"))) %>%
  mutate(PctRmvChlormaineOx_DBF = ((DBF_Effluent - clearwell_Effluent)/DBF_Effluent) * 100)

## remove extra columns and tidy data
df6 <- df5 %>%
  # select(Analyte, SampleDate, ChloramineOx, PctRmvChlormaineOx_SMFTBF, PctRmvChlormaineOx_DBF) %>%
  mutate(Process = if_else(is.na(PctRmvChlormaineOx_SMFTBF), "DBF", "SMF+TBF")) %>% 
  mutate(PctRmvChlormaineOx = if_else(is.na(PctRmvChlormaineOx_SMFTBF), PctRmvChlormaineOx_DBF, PctRmvChlormaineOx_SMFTBF)) %>%
  select(-PctRmvChlormaineOx_SMFTBF, -PctRmvChlormaineOx_DBF)

# save percent removal data (categorized by )
write.df <- df6
ef.dir <- "./data/eurofins-data/"
clean.path <- str_c(ef.dir, "calculated/", "pctrmv-chloramineoxidation.rds")
write_rds(x=write.df, path=clean.path)
