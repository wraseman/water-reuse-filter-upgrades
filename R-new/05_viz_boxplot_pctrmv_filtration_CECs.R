# purpose: create boxplots of percent removal of CECs due to filtration
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

analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")

## define figure properties
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
fig.resolution <- 300  # figure resolution (300 dpi)

## create output directory for plots for each filter type if it does not already exist
filters <- c("TBF", "SMF", "DBF")
for (filter in filters) {
  
  boxplt.dir <- str_c(fig.dir, filter, "/", "pctrmv-v-time/", "boxplots_CECs-only/")
  
  if (!dir.exists(boxplt.dir)){ 
    dir.create(boxplt.dir)
  } 
}



# plot percent removal by filter type 

## plots with full extent of removal data

## plots with zoomed in view of removal data


# plot percent removal for each filter by sorption category


# plot percent removal for each filter by biodegradation category


# plot percent removal for each filter by combined sorption/biodegradation category


