# Purpose: visualize percent removal for each filter and disinfection point
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization

# read in percent removal data for TBF and SMF
data.dir <- "./data/"
filename <- "pct-rmv.rds"
data.path <- str_c(data.dir, filename)
all.df <- read_rds(path = data.path) 

# visualize boxplots of percent removal for TBF and SMF
filter.names <- c("Traveling Bed Filter", "Synthetic Media Filter", "Deep Bed Filter")
pctrmv.string <- c("PctRmvTBFilt", "PctRmvSMFilt", "PctRmvDBFilt")

for (i in 1:length(filter.names)) {

  ## Giardia and Cryptosporidium
  p1.gc <- ggplot(data = filter(all.df, Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity")),
                  aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal") +
    ylim(-50, 100)
  print(p1.gc)
  
  ## Contaminants of emerging concern (CEC)
  ### plot by biodegration and sorption categories
  p1.cec <- ggplot(data = filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                   aes_string(x="Biodeg", y=pctrmv.string[i], color="Sorption")) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal") +
    ylim(-50, 100)
  print(p1.cec) 
    
  
  # visualize filter removal data without negative outliers
  nonneg.filt <- filter(all.df, get(pctrmv.string[i]) >= 0)
  
  ## Giardia and Cryptosporidium
  p2.gc <- ggplot(filter(nonneg.filt, Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity")),
                  aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i], "(Negative values removed)") +
    ylab("Percent Removal") +
    ylim(0, 100)
  print(p2.gc)
  
  ## Contaminants of emerging concern (CEC)
  ### plot by biodegration and sorption categories
  p2.cec <- ggplot(data = filter(nonneg.filt, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                   aes_string(x="Biodeg", y=pctrmv.string[i], color="Sorption")) +
    geom_boxplot() +
    ggtitle(filter.names[i], "(Negative values removed)") +
    ylab("Percent Removal") +
    ylim(0, 100)
  print(p2.cec)
}

# visualize boxplots of percent removal for chlorination
pctrmvchlor.string <- c("PctRmvChlor1", "PctRmvChlor2")

for (i in 1:length(pctrmvchlor.string)) {
  ## plot by chlorine oxidation category
  filt.df <- filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity")))
  p.chlor <- ggplot(data = filt.df,
         aes_string(x="ClOxidation", y=pctrmvchlor.string[i], color="ClOxidation")) +
    geom_boxplot() +
    ggtitle(str_c("Post-Chlorination Phase", i)) +
    ylab("Percent Removal")
  print(p.chlor)
  
  ## plot by chlorine oxidation category for only nonnegative values
  nonneg.clox <- filter(all.df, get(pctrmvchlor.string[i]) >= 0)
  p.chlornon <- ggplot(data = filter(nonneg.clox, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
         aes_string(x="ClOxidation", y=pctrmvchlor.string[i], color="ClOxidation")) +
    geom_boxplot() +
    ggtitle(str_c("Post-Chlorination Phase", i), "(Negative values removed)") +
    ylab("Percent Removal") +
    ylim(0, 100)
  print(p.chlornon)
}

# visualize number of detects and non-detects
## dataset contains information on pathogen, contaminants of emerging concern (CEC), 
##  turbidity, and chlorine concentrations and flow rates
data.dir <- "./data/"
filename <- "cleaned_all-filters.rds"
data.path <- str_c(data.dir, filename)
tidy.df <- read_rds(path = data.path) 

## visualize detects for CECs
detect1.df <- filter(tidy.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))) %>%
  group_by(Location, Detect) %>% count() %>% na.omit

ggplot(data = filter(detect1.df, Detect == TRUE, 
                     Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 1") +
  ylim(0, 200)

ggplot(data = filter(detect1.df, Detect == TRUE, 
                     Location %in% c("DBF-E", "DBF-I", "Final Ph2")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 2") +
  ylim(0, 200)

## visualize detects for pathogens
detect2.df <- filter(tidy.df, Parameter %in% c("Giardia", "Cryptosporidium")) %>%
  group_by(Location, Detect) %>% count() %>% na.omit

ggplot(data = filter(detect2.df, Detect == TRUE, 
                     Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 1 Pathogen Samples")

ggplot(data = filter(detect2.df, Detect == TRUE, 
                     Location %in% c("DBF-E", "DBF-I", "Final Ph2")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 2 Pathogen Samples")
