# Purpose: visualize percent removal for traveling bed and synthetic media filters and disinfection
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization

# read in percent removal data for TBF and SMF
data.dir <- "./data/"
filename <- "pct-rmv_tbf-smf.rds"
data.path <- str_c(data.dir, filename)
tsf.df <- read_rds(path = data.path) 

# visualize boxplots of percent removal for TBF and SMF
filter.names <- c("Traveling Bed Filter", "Synthetic Media Filter")
pctrmv.string <- c("PctRmvTBFilt", "PctRmvSMFilt")

for (i in 1:length(filter.names)) {
  
  ## Giardia and Cryptosporidium
  p1.gc <- ggplot(data = filter(tsf.df, Parameter %in% c("Giardia", "Cryptosporidium")),
         aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal")
  print(p1.gc)

  ## Turbidity
  p1.turb <- ggplot(data = filter(tsf.df, Parameter == "Turbidity"),
         aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal")
  print(p1.turb)

  ## Contaminants of emerging concern (CEC)
  ### plot by biodegration and sorption categories
  p1.cec <- ggplot(data = filter(tsf.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
         aes_string(x="Biodeg", y=pctrmv.string[i], color="Sorption")) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal")
  print(p1.cec)

  # visualize filter removal data without negative outliers
  nonneg.filt <- filter(tsf.df, get(pctrmv.string[i]) >= 0)

  ## Giardia and Cryptosporidium
  p2.gc <- ggplot(data = filter(nonneg.filt, Parameter %in% c("Giardia", "Cryptosporidium")),
         aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i], "(Negative values removed)") +
    ylab("Percent Removal")
  print(p2.gc)

  ## Turbidity
  p2.turb <- ggplot(data = filter(nonneg.filt, Parameter == "Turbidity"),
         aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i], "(Negative values removed)") +
    ylab("Percent Removal")
  print(p2.turb)

  ## Contaminants of emerging concern (CEC)
  ### plot by biodegration and sorption categories
  p2.cec <- ggplot(data = filter(nonneg.filt, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
         aes_string(x="Biodeg", y=pctrmv.string[i], color="Sorption")) +
    geom_boxplot() +
    ggtitle(filter.names[i], "(Negative values removed)") +
    ylab("Percent Removal")
  print(p2.cec)
}

# visualize boxplots of percent removal for chlorination
## plot by chlorine oxidation category
ggplot(data = filter(tsf.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
       aes(x=ClOxidation, y=PctRmvChlorination, color=ClOxidation)) +
  geom_boxplot() +
  ggtitle("Post-Chlorination after TBF And SMF") +
  ylab("Percent Removal") %>% print

## plot by chlorine oxidation category for only nonnegative values
nonneg.clox <- filter(tsf.df, PctRmvChlorination >= 0)
ggplot(data = filter(nonneg.clox, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
       aes(x=ClOxidation, y=PctRmvChlorination, color=ClOxidation)) +
  geom_boxplot() +
  ggtitle("Post-Chlorination after TBF And SMF") +
  ylab("Percent Removal") %>% print




