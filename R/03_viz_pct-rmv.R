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
all.df <- read_rds(path = data.path) %>%
  # create combined biodegradation and sorption category
  mutate(BiodegSorption = if_else(Biodeg == "Good" & Sorption == "Good", "Good-Biodeg, Good-Sorption", 
                                  if_else(Biodeg == "Good" & Sorption == "Poor", "Good-Biodeg, Poor-Sorption", 
                                          if_else(Biodeg == "Poor" & Sorption == "Good", "Poor-Biodeg, Good-Sorption", 
                                                  if_else(Biodeg == "Poor" & Sorption == "Poor", "Poor-Biodeg, Poor-Sorption", NA_character_)))))

all.df$BiodegSorption <- factor(all.df$BiodegSorption,
                                levels = c("Poor-Biodeg, Poor-Sorption", "Poor-Biodeg, Good-Sorption", "Good-Biodeg, Poor-Sorption", "Good-Biodeg, Good-Sorption"), ordered = TRUE)

# visualize boxplots of percent removal for TBF and SMF
filter.names <- c("Traveling Bed Filter", "Synthetic Media Filter", "Deep Bed Filter")
pctrmv.string <- c("PctRmvTBFilt", "PctRmvSMFilt", "PctRmvDBFilt")

## plot settings
figure.dir <- "./figures/"
fig.resolution <- 300
mytheme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  title = element_text(size = 18),
  legend.text = element_text(size = 16))

samplesize <- function(y) 
  c(label=length(y), y=median(y))

for (i in 1:length(filter.names)) {
  # i <- 1
  ## Giardia and Cryptosporidium
  p1.gc <- ggplot(data = filter(all.df, Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity")),
                  aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal") +
    ylim(-50, 100) +
    stat_summary(fun.data=samplesize, geom="text", vjust=1, col="black", size=6) + # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
    mytheme 
  tiff(filename = str_c(figure.dir, "pathogens_", pctrmv.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p1.gc)
  dev.off()
  
  ## Contaminants of emerging concern (CEC)
  ### plot by biodegration and sorption categories
  p1.cec <- ggplot(data = filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                   aes_string(x="Biodeg", y=pctrmv.string[i], color="Sorption")) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal") +
    xlab("Biodegradability") +
    ylim(-50, 100) +
    mytheme 
  tiff(filename = str_c(figure.dir, "CECs_", pctrmv.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p1.cec) 
  dev.off()
  
  ### plot by combined biodegration and sorption categories
  p1b.cec <- ggplot(data = filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                    aes_string(x="BiodegSorption", y=pctrmv.string[i], fill="BiodegSorption")) +
    geom_boxplot(alpha= 0.7) +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal") +
    xlab("Biodegradability and Sorption Categories") +
    ylim(-50, 100) +
    mytheme +
    theme(legend.position="none")
  tiff(filename = str_c(figure.dir, "CECs_BiodegSorption", pctrmv.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p1b.cec) 
  dev.off()
  
  p1c.cec <- ggplot(data = filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                    aes_string(x="Sorption", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal") +
    xlab("Sorption") +
    ylim(-50, 100) +
    mytheme 
  tiff(filename = str_c(figure.dir, "CECs_Sorption", pctrmv.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p1c.cec) 
  dev.off()
  
  p1d.cec <- ggplot(data = filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                    aes_string(x="Biodeg", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i]) +
    ylab("Percent Removal") +
    xlab("Biodegradability") +
    ylim(-50, 100) +
    mytheme 
  tiff(filename = str_c(figure.dir, "CECs_Biodeg", pctrmv.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p1d.cec) 
  dev.off()
  
  # visualize filter removal data without negative outliers
  nonneg.filt <- filter(all.df, get(pctrmv.string[i]) >= 0)
  
  ## Giardia and Cryptosporidium
  p2.gc <- ggplot(filter(nonneg.filt, Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity")),
                  aes_string(x="Parameter", y=pctrmv.string[i])) +
    geom_boxplot() +
    ggtitle(filter.names[i], "(Negative values removed)") +
    ylab("Percent Removal") +
    ylim(0, 100) +
    mytheme
  tiff(filename = str_c(figure.dir, "pathogens-nonneg_", pctrmv.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p2.gc)
  dev.off()
  
  ## Contaminants of emerging concern (CEC)
  ### plot by biodegration and sorption categories
  p2.cec <- ggplot(data = filter(nonneg.filt, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                   aes_string(x="Biodeg", y=pctrmv.string[i], color="Sorption")) +
    geom_boxplot() +
    ggtitle(filter.names[i], "(Negative values removed)") +
    ylab("Percent Removal") +
    xlab("Biodegradability") +
    ylim(0, 100) +
    mytheme 
  # stat_summary(fun.data=samplesize, geom="text", vjust=-2, col="black")  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  tiff(filename = str_c(figure.dir, "CECs-nonneg_", pctrmv.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p2.cec) 
  dev.off()
}

# visualize boxplots of percent removal for chlorination
pctrmvchlor.string <- c("PctRmvChlor1", "PctRmvChlor2")

for (i in 1:length(pctrmvchlor.string)) {
  ## plot by chlorine oxidation category
  filt.df <- filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity")))
  
  filt.df$ClOxidation <- factor(filt.df$ClOxidation, levels = 
                                  c("Poor", "Moderate", "Good"))
  
  p.chlor <- ggplot(data = filt.df,
                    aes_string(x="ClOxidation", y=pctrmvchlor.string[i], fill="ClOxidation")) +
    geom_boxplot(alpha= 0.7) +
    ggtitle(str_c("Post-Chlorination Phase ", i)) +
    ylab("Percent Removal") +
    xlab("Chlorine Oxidation") +
    mytheme +
    ylim(-50, 100)
  tiff(filename = str_c(figure.dir, "CECs_", pctrmvchlor.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p.chlor)
  dev.off()
  
  ## plot by chlorine oxidation category for only nonnegative values
  nonneg.clox <- filter(all.df, get(pctrmvchlor.string[i]) >= 0)
  p.chlornon <- ggplot(data = filter(nonneg.clox, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))),
                       aes_string(x="ClOxidation", y=pctrmvchlor.string[i], fill="ClOxidation")) +
    geom_boxplot(alpha= 0.7) +
    geom_boxplot() +
    ggtitle(str_c("Post-Chlorination Phase", i), "(Negative values removed)") +
    ylab("Percent Removal") +
    xlab("Chlorine Oxidation") +
    ylim(0, 100) +
    mytheme
  tiff(filename = str_c(figure.dir, "CECs-nonneg_", pctrmvchlor.string[i], ".tiff"),
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = fig.resolution)
  print(p.chlornon)
  dev.off()
}

# visualize number of detects and non-detects
## dataset contains information on pathogen, contaminants of emerging concern (CEC), 
##  turbidity, and chlorine concentrations and flow rates
data.dir <- "./data/"
filename <- "cleaned_all-filters.rds"
data.path <- str_c(data.dir, filename)
tidy.df <- read_rds(path = data.path) 

### order bar graph to match order of processes in plant
### source: https://sebastiansauer.github.io/ordering-bars/
tidy.df$Location <- factor(tidy.df$Location, levels = 
                             c("TBF-I", "TBF-E", "SMF-E", "Final Ph1", 
                               "DBF-I", "DBF-E", "Final Ph2"))

## visualize detects for CECs
detect1.df <- filter(tidy.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))) %>%
  group_by(Location, Detect) %>% count() %>% na.omit


p.dcec1 <- ggplot(data = filter(detect1.df, Detect == TRUE, 
                                Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 1") +
  ylim(0, 200) +
  mytheme
tiff(filename = str_c(figure.dir, "CECs-detect-Phs1_", pctrmvchlor.string[i], ".tiff"),
     height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = fig.resolution)
print(p.dcec1)
dev.off()

p.dcec2 <- ggplot(data = filter(detect1.df, Detect == TRUE, 
                                Location %in% c("DBF-E", "DBF-I", "Final Ph2")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 2") +
  ylim(0, 200) +
  mytheme
tiff(filename = str_c(figure.dir, "CECs-detect-Phs2", pctrmvchlor.string[i], ".tiff"),
     height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = fig.resolution)
print(p.dcec2)
dev.off()

## visualize detects for pathogens
detect2.df <- filter(tidy.df, Parameter %in% c("Giardia", "Cryptosporidium")) %>%
  group_by(Location, Detect) %>% count() %>% na.omit

p.dpath1 <- ggplot(data = filter(detect2.df, Detect == TRUE, 
                                 Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 1 Pathogen Samples") +
  mytheme
tiff(filename = str_c(figure.dir, "pathogens-detect-Phs1", pctrmvchlor.string[i], ".tiff"),
     height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = fig.resolution)
print(p.dpath1)
dev.off()


p.dpath2 <- ggplot(data = filter(detect2.df, Detect == TRUE, 
                                 Location %in% c("DBF-E", "DBF-I", "Final Ph2")), aes(x=Location, y=n)) +
  geom_bar(stat="identity") +
  ylab("Detectable Samples") +
  ggtitle("Phase 2 Pathogen Samples") +
  mytheme
tiff(filename = str_c(figure.dir, "pathogens-detect-Phs2", pctrmvchlor.string[i], ".tiff"),
     height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = fig.resolution)
print(p.dpath2)
dev.off()

