# purpose: create boxplots of percent removal of pathogens due to filtration
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# user-defined functions
# medianboxplt <- function(y) 
#   c(label=median(y) %>% round(1), y=median(y))

# read in percent removal data
calc.dir <- "./data/eurofins-data/calculated/"
pctrmv.path <- str_c(calc.dir, "pctrmv-filtration.rds")
df1 <- read_rds(path = pctrmv.path) %>%
  filter(Analyte %in% c("Cryptosporidium", "Giardia"))

analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")

### specify order of filter types for boxplots
df2 <- df1
df2$Process <- factor(df1$Process,
                      levels = c("SMF", "TBF", "DBF"), ordered = TRUE)

# Wilcoxon Rank Sum test (http://biostat.mc.vanderbilt.edu/wiki/pub/Main/AnesShortCourse/NonParametrics.pdf)
## choose non-parametric test because the data is non-normal
## to answer the following question: is the difference between influent and effluent significantly different from zero?
for (process in filters) {  # filter type

    test.df <- filter(df2, Process == process) %>%
      ungroup()
    
    influent <- test.df$Influent
    effluent <- test.df$Effluent
    
    # t.test(influent, effluent, mu = 0) %>% print()
    if (length(influent) == length(effluent)) {
      n.samples <- length(influent)
    } else {
      print("Length of influent and effluent data not equal")
    }
    p.value <- wilcox.test(influent, effluent)$p.value 
    
    str_c("For ", process, 
          " with ", category, 
          ", p-value = ", p.value %>% round(2), 
          " (n = ", n.samples, ")") %>%
      print()
    
  }
}
