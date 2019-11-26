# Purpose: read in and clean data from California laboratory results
#   Data includes pathogen and contaminants of emerging concern (CEC) concentrations
# Output: ./data/cleaned_all-filters.rds (tidy, cleaned dataframe)
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in Eurofin's California lab
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "california-lab-results_clean.rds")
ca.df1 <- read_rds(path = clean.path)

# visulize a single parameter for a single process at the influent and effluent
analyte <- "Acetaminophen"
process <- "TBF"

pc.analyte.df <- filter(ca.df1, 
                   Analyte == analyte,
                   Process == process)
pc.analyte.units <- pc.analyte.df$Units %>% unique

ggplot(pc.analyte.df, aes(x = SampleDateTime, y = DilutAdjResult)) +
  geom_point(aes(color = ProcessInfEff)) +
  ylab(str_c("Concentration (", pc.analyte.units, ")")) +
  xlab("Time") + 
  ggtitle(str_c(analyte, ", ", process))

## visualize number of non-detects
pc.analyte.df %>% 
  ggplot(aes(ProcessInfEff)) +
  geom_bar(aes(fill=Detect))

# count the number of detects in influent and non-detects for each analyte for each filter type
# ca.df1 %>%
#   group_by(ProcessInfEff, Process1, Process2)