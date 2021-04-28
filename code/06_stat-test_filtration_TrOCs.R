# purpose: calculate statistical test (Wilcoxon signed rank test) to determine if there is a significant difference
#   between filter influent and effluent TrOC data 
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in percent removal data
calc.dir <- "./data/eurofins-data/calculated/"
pctrmv.path <- str_c(calc.dir, "pctrmv-filtration.rds")
df1 <- read_rds(pctrmv.path) %>%
  filter(!(Analyte %in% c("Cryptosporidium", "Giardia")))

analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")

### specify order of filter types for boxplots
df2 <- df1
df2$Process <- factor(df1$Process,
                      levels = c("SMF", "TBF", "DBF"), ordered = TRUE)

### create an array that acts like a dictionary for filter names 
###   source: https://stackoverflow.com/questions/2858014/working-with-dictionaries-lists-in-r
filter.fullnames <- c("Synthetic Media Filter", "Traveling Bed Filter", "Deep Bed Filter")
names(filter.fullnames) <- c("SMF", "TBF", "DBF")  # abbreviations

# plot percent removal for each filter by combined sorption/biodegradation category
df3 <- df2 %>%
  # create combined biodegradation and sorption category
  mutate(BiodegSorption = if_else(Biodegradation == "Good" & Sorption == "Good", "Good-Biodeg, Good-Sorption", 
                                  if_else(Biodegradation == "Good" & Sorption == "Poor", "Good-Biodeg, Poor-Sorption", 
                                          if_else(Biodegradation == "Poor" & Sorption == "Good", "Poor-Biodeg, Good-Sorption", 
                                                  if_else(Biodegradation == "Poor" & Sorption == "Poor", "Poor-Biodeg, Poor-Sorption", NA_character_)))))
df3$BiodegSorption <- factor(df3$BiodegSorption,
                             levels = c("Poor-Biodeg, Poor-Sorption", "Poor-Biodeg, Good-Sorption", "Good-Biodeg, Poor-Sorption", "Good-Biodeg, Good-Sorption"), ordered = TRUE)


# Wilcoxon Rank Sum test (http://biostat.mc.vanderbilt.edu/wiki/pub/Main/AnesShortCourse/NonParametrics.pdf)
## choose non-parametric test because the data is non-normal
## to answer the following question: is the difference between influent and effluent significantly different from zero?
bdsrp.categories <- unique(df3$BiodegSorption)
# biodeg.category <- "Poor"
# process <- "DBF"

for (process in filters) {  # filter type
  for (category in bdsrp.categories) {  # biodegradation and sorption category
    
    test.df <- filter(df3, Process == process) %>%
      filter(BiodegSorption == category) %>%
      # filter(Biodegradation == biodeg.category) %>% 
      ungroup()
    
    # summary(test.df) %>% print()
    
    influent <- test.df$Influent
    effluent <- test.df$Effluent
    
    # t.test(influent, effluent, mu = 0) %>% print()
    if (length(influent) == length(effluent)) {
      n.samples <- length(influent)
    } else {
      print("Length of influent and effluent data not equal")
    }
    p.value <- wilcox.test(influent, effluent, paired = TRUE, mu = 0)$p.value 
    
    str_c(process, category, p.value %>% round(4), n.samples, sep = " ") %>%
      print()
    
  }
}

