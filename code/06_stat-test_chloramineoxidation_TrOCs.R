# purpose: calculate statistical test (Wilcoxon signed rank test) to determine if there is a significant difference
#   between pre- and post-chloramine oxidation
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

filters <- c("SMF+TBF", "DBF")

# read in percent removal data
calc.dir <- "./data/eurofins-data/calculated/"
pctrmv.path <- str_c(calc.dir, "pctrmv-chloramineoxidation.rds")
df1 <- read_rds(pctrmv.path) %>%
  filter(!(Analyte %in% c("Cryptosporidium", "Giardia"))) %>%
  filter(Analyte != "BPA")  # no Chloramine Oxidation category for BPA

# rearrange data for statistical test
df2 <- df1 %>%
  select(-SMF_Effluent, -TBF_Effluent, -PctRmvChlormaineOx) %>%  # no longer need these columns
  # DBF effluent is the "Influent" to the clearwell for the DBF data
  # SMF+TBF effluent is the "Influent" to the clearwell for the SMF+TBF data
  mutate(Influent = if_else(Process == "DBF", DBF_Effluent, SMFTBF_Eff_Estimated)) %>%
  rename(Effluent = clearwell_Effluent) %>%
  select(-DBF_Effluent, -SMFTBF_Eff_Estimated)  # no longer need these columns



# Wilcoxon Rank Sum test (http://biostat.mc.vanderbilt.edu/wiki/pub/Main/AnesShortCourse/NonParametrics.pdf)
## choose non-parametric test because the data is non-normal
## to answer the following question: is the difference between influent and effluent significantly different from zero?
chlox.categories <- unique(df2$ChloramineOxidation)

for (process in filters) {  # filter type
  for (category in chlox.categories) {  # biodegradation and sorption category
    
    test.df <- filter(df2, Process == process) %>%
      filter(ChloramineOxidation == category) %>%
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
    p.value <- wilcox.test(influent, effluent, paired = TRUE)$p.value 
    
    str_c(process, category, p.value %>% round(4), n.samples, sep = " ") %>%
      print()
  }
}

# look at all analytes
analytes <- unique(df2$Analyte)

for (process in filters) {  # filter type
  for (analyte in analytes) {
  
  test.df <- filter(df2, Process == process) %>%
    filter(Analyte == analyte) %>%
    ungroup()
  
  influent <- test.df$Influent
  effluent <- test.df$Effluent
  
  # t.test(influent, effluent, mu = 0) %>% print()
  if (length(influent) == length(effluent)) {
    n.samples <- length(influent)
  } else {
    print("Length of influent and effluent data not equal")
  }
  p.value <- wilcox.test(influent, effluent, paired = TRUE)$p.value 
  
  str_c(process, analyte, p.value %>% round(4), n.samples, sep = " ") %>%
    print()
  }
}


