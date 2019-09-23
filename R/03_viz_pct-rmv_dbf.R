# Purpose: visualize percent removal for deep bed filters (DBF) and disinfection
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization

# read in percent removal data for DBF
data.dir <- "./data/"
filename <- "pct-rmv_dbf.rds"
data.path <- str_c(data.dir, filename)
dbf.df <- read_rds(path = data.path) 

# visualize boxplots of percent removal for DBF
## Giardia and Cryptosporidium
ggplot(data = filter(dbf.df, Parameter %in% c("Giardia", "Cryptosporidium")), 
              aes(x=Parameter, y=PctRmvFilter)) +
  geom_boxplot() +
  ggtitle("Deep Bed Filter") +
  ylab("Percent Removal")

## Turbidity
ggplot(data = filter(dbf.df, Parameter == "Turbidity"), 
       aes(x=Parameter, y=PctRmvFilter)) +
  geom_boxplot() +
  ggtitle("Deep Bed Filter") +
  ylab("Percent Removal")

## Contaminants of emerging concern (CEC)
### plot by biodegration and sorption categories
ggplot(data = filter(dbf.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))), 
       aes(x=Biodeg, y=PctRmvFilter, color=Sorption)) +
  geom_boxplot() +
  ggtitle("Deep Bed Filter") +
  ylab("Percent Removal")

# visualize filter removal data without negative outliers
nonneg.filt <- filter(dbf.df, PctRmvFilter >= 0)

## Giardia and Cryptosporidium
ggplot(data = filter(nonneg.filt, Parameter %in% c("Giardia", "Cryptosporidium")), 
       aes(x=Parameter, y=PctRmvFilter)) +
  geom_boxplot() +
  ggtitle("Deep Bed Filter", "(Negative values removed)") +
  ylab("Percent Removal")

## Turbidity
ggplot(data = filter(nonneg.filt, Parameter == "Turbidity"), 
                 aes(x=Parameter, y=PctRmvFilter)) +
  geom_boxplot() +
  ggtitle("Deep Bed Filter", "(Negative values removed)") +
  ylab("Percent Removal")

## Contaminants of emerging concern (CEC)
### plot by biodegration and sorption categories
ggplot(data = filter(nonneg.filt, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))), 
       aes(x=Biodeg, y=PctRmvFilter, color=Sorption)) +
  geom_boxplot() +
  ggtitle("Deep Bed Filter") +
  ylab("Percent Removal")

# visualize boxplots of percent removal for chlorination
## plot by chlorine oxidation category
ggplot(data = filter(dbf.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))), 
       aes(x=ClOxidation, y=PctRmvChlorination, color=ClOxidation)) +
  geom_boxplot() +
  ggtitle("Post-Chlorination after Deep Bed Filter") +
  ylab("Percent Removal")

## plot by chlorine oxidation category for only nonnegative values
nonneg.clox <- filter(dbf.df, PctRmvChlorination >= 0)
ggplot(data = filter(nonneg.clox, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))), 
       aes(x=ClOxidation, y=PctRmvChlorination, color=ClOxidation)) +
  geom_boxplot() +
  ggtitle("Post-Chlorination after Deep Bed Filter", "(Negative values removed)") +
  ylab("Percent Removal")




