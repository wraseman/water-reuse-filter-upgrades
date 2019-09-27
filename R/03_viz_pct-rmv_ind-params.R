# Purpose: visualize percent removal for individual species
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


## plot settings
figure.dir <- "./figures/"
fig.resolution <- 300
# mytheme = theme(
#   axis.title.x = element_text(size = 16),
#   axis.text.x = element_text(size = 16),
#   axis.title.y = element_text(size = 16),
#   axis.text.y = element_text(size = 16),
#   title = element_text(size = 18),
#   legend.text = element_text(size = 16))

### Analytes of Interest (from Eric Stanley)
# Dilantin (poor biodegradation / poor sorption / poor cl oxidation)
# Tris(2-carboxyethyl)phosphine hydrochloride(poor biodegradation / good sorption / poor cl oxidation)
# Carbamazepine (poor biodegradation / good sorption / poor cl oxidation)
# Gemfibrozil (good biodegradation / poor sorption / moderate cl oxidation)
# Ibuprofen (good biodegradation / poor sorption / poor cl oxidation)
# Bisphenol A (good biodegradation / good sorption)
# Triclosan (goodbiodegradation / good sorption / good cl oxidation)

## keep columns and analytes necessary for analysis
analytes <- c("Dilantin", "Sulfamethoxazole", 
              "Tris(2-carboxyethyl)phosphine hydrochloride",
              "Carbamazepine", "Gemfibrozil", "Ibuprofen", 
              "Triclosan") # note: not enough data on BPA (Bisphenol A) to do comparison
analytes.df <- filter(all.df, Parameter %in% analytes) %>% ungroup %>%
  mutate(Parameter = str_trunc(Parameter, 15))

### Filter removal 
pctrmv.df <- select(analytes.df, PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt, Parameter) %>%
  gather(key = "FilterType", value = "PercentRemoval", -Parameter) %>%
  mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
                             ifelse(FilterType == "PctRmvSMFilt", "SMF", 
                                    ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))


### DBF Analysis
ggplot(data = filter(pctrmv.df, FilterType=="DBF"), aes(x=Parameter, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Deep Bed Filter") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

### TBF Analysis
ggplot(data = filter(pctrmv.df, FilterType=="TBF"), aes(x=Parameter, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Traveling Bed Filter") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

### SMF Analysis
ggplot(data = filter(pctrmv.df, FilterType=="SMF"), aes(x=Parameter, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Synthetic Media Filter") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

