# purpose: create dataset for percent removal of CECs due to chlorine oxidation
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in both of Eurofin's labs (California and South Bend)
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
df1 <- read_rds(path = clean.path) 

# calculate percent removal for each analyte due to filtration (assuming concentration of non-detects is the detection limit)
df2 <- df1 %>%
  filter(Duplicate == FALSE,
         Triplicate == FALSE) %>%  # remove extra values from triplicate analysis
  filter(Process != "lab_blank") %>%
  mutate(SampleDate = lubridate::floor_date(SampleDateTime, unit = "6 hours")) %>%  # round datetime to nearest 6 hours
  select(Analyte, SampleDate, Process, DilutAndLimAdjResult, ProcessInfEff, ClOxidation)

## create influent and effluent columns for each process type
##  source: https://tidyr.tidyverse.org/dev/articles/pivot.html
df3 <- df2 %>%
  filter(ProcessInfEff == "Effluent") %>%
  unite(ProcessWithInfEff, Process, ProcessInfEff) %>%
  group_by(SampleDate, Analyte) %>%
  spread(key = ProcessWithInfEff, value = DilutAndLimAdjResult)

## create combined effluent for SMF and TBF (because these effluents mix before going into the clearwell)
Q.tbf <- 1.71  # based on average flow 1.81 and 1.60 MGD from "Pages from LRD SCADA SCREENS.pdf"
Q.smf <- 6.25  # based on average flow 6.34 and 6.15 MGD from "Pages from LRD SCADA SCREENS.pdf"
Q.total <- Q.tbf + Q.smf
Q.frc.tbf <- Q.tbf/Q.total  # flow fraction sent to traveling bed filters
Q.frc.smf <- Q.smf/Q.total  # flow fraction sent to synthetic media filters

df4 <- df3 %>%
  mutate(SMFTBF_Eff_Estimated = Q.frc.tbf*TBF_Effluent + Q.frc.smf*SMF_Effluent)

## calculate percent removal due to chlorine oxidation
df5 <- df4 %>%
  mutate(PctRmvClOx_SMFTBF = ((SMFTBF_Eff_Estimated - clearwell_Effluent)/SMFTBF_Eff_Estimated) * 100) %>%
  # PctRmvClOxSign_SMFTBF = if_else(sign(PctRmvClOx_SMFTBF) == 1, "Positive",
  #                           if_else(sign(PctRmvClOx_SMFTBF) == 0, "Zero",
  #                                   "Negative"))) %>%
  mutate(PctRmvClOx_DBF = ((DBF_Effluent - clearwell_Effluent)/DBF_Effluent) * 100)

## remove extra columns and tidy data
df6 <- df5 %>%
  # select(Analyte, SampleDate, ClOxidation, PctRmvClOx_SMFTBF, PctRmvClOx_DBF) %>%
  mutate(Process = if_else(is.na(PctRmvClOx_SMFTBF), "DBF", "SMF+TBF")) %>% 
  mutate(PctRmvClOx = if_else(is.na(PctRmvClOx_SMFTBF), PctRmvClOx_DBF, PctRmvClOx_SMFTBF)) %>%
  select(-PctRmvClOx_SMFTBF, -PctRmvClOx_DBF)

# save percent removal data (categorized by )
write.df <- df6
ef.dir <- "./data/eurofins-data/"
clean.path <- str_c(ef.dir, "calculated/", "pctrmv-cloxidation.rds")
write_rds(x=write.df, path=clean.path)
