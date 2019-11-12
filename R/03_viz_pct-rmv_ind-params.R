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
# 
# ## keep columns and analytes necessary for analysis
# analytes <- c("Dilantin", "Sulfamethoxazole", 
#               "Tris(2-carboxyethyl)phosphine hydrochloride",
#               "Carbamazepine", "Gemfibrozil", "Ibuprofen", 
#               "Triclosan") # note: not enough data on BPA (Bisphenol A) to do comparison
# analytes.df <- filter(all.df, Parameter %in% analytes) %>% ungroup %>%
#   mutate(Parameter = str_trunc(Parameter, 15))

analytes <- c("Cryptosporidium", "Giardia", "Turbidity")
analytes.df <- filter(all.df, !(Parameter %in% analytes)) %>% ungroup %>%
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
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Deep Bed Filter") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

### TBF Analysis
ggplot(data = filter(pctrmv.df, FilterType=="TBF"), aes(x=Parameter, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Traveling Bed Filter") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

### SMF Analysis
ggplot(data = filter(pctrmv.df, FilterType=="SMF"), aes(x=Parameter, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Synthetic Media Filter") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# investigate DBF negative values
anom.df1 <- filter(analytes.df, PctRmvDBFilt < -5000)

## visualize non-detects and detects
# https://stackoverflow.com/questions/2190756/how-to-count-true-values-in-a-logical-vector
sum(anom.df1$DBFiltEffDetect, na.rm = TRUE)  # 3 detects
sum(anom.df1$DBFiltInfDetect, na.rm = TRUE)  # 0 non-detects
### all extreme values are from non-detects

## DBF: create dataframe for analyzing only detected values in filter effluent and influent
detect.df1 <- filter(analytes.df, DBFiltEffDetect == TRUE, DBFiltInfDetect == TRUE)

ggplot(detect.df1, aes(x=Parameter, y=PctRmvDBFilt)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Deep Bed Filter (only analyzing detectable samples for influent and effluent)") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# investigate TBF and SMF negative values

## TBF and SMF: create dataframe for analyzing only detected values in filter effluent and influent
detect.df2 <- filter(analytes.df, TBFiltEffDetect == TRUE, TBFiltInfDetect == TRUE)
detect.df3 <- filter(analytes.df, SMFiltEffDetect == TRUE, TBFiltInfDetect == TRUE)

## visualize TBF detectable samples
ggplot(detect.df2, aes(x=Parameter, y=PctRmvTBFilt)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Traveling Bed Filter (only analyzing detectable samples for influent and effluent)") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

## visualize SMF detectable samples
ggplot(detect.df3, aes(x=Parameter, y=PctRmvSMFilt)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Parameter") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  ggtitle("Synthetic Media Filter (only analyzing detectable samples for influent and effluent)") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# look at time series of percent removal
## only detectable samples
ggplot(detect.df1, aes(x=DateCollected, y = PctRmvDBFilt)) +
  geom_point() +
  ylab("Percent Removal") +
  ggtitle("Deep Bed Filter (only analyzing detectable samples for influent and effluent)") 

ggplot(detect.df2, aes(x=DateCollected, y = PctRmvTBFilt)) +
  geom_point() +
  ylab("Percent Removal") +
  ggtitle("Traveling Bed Filter (only analyzing detectable samples for influent and effluent)") 

ggplot(detect.df3, aes(x=DateCollected, y = PctRmvSMFilt)) +
  geom_point() + 
  ylab("Percent Removal") +
  ggtitle("Synthetic Media Filter (only analyzing detectable samples for influent and effluent)") 

# all samples
ggplot(analytes.df, aes(x=DateCollected, y = PctRmvDBFilt)) +
  geom_point() +
  ylab("Percent Removal") +
  ggtitle("Deep Bed Filter") 

ggplot(analytes.df, aes(x=DateCollected, y = PctRmvTBFilt)) +
  geom_point() +
  ylab("Percent Removal") +
  ggtitle("Traveling Bed Filter") 

ggplot(analytes.df, aes(x=DateCollected, y = PctRmvSMFilt)) +
  geom_point() + 
  ylab("Percent Removal") +
  ggtitle("Synthetic Media Filter") 
