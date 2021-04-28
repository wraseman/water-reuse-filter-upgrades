# purpose: calculate statistical tests to determine the impact of filtration type of pathogen removal
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
df1 <- read_rds(pctrmv.path) %>%
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
process <- filters[2]
analyte <- analytes[2]

for (process in filters) {  # filter type
  for (analyte in analytes) {  # pathogen type
    
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
    p.value <- wilcox.test(influent, effluent, paired = TRUE, mu = 0)$p.value
    
    # wilcox.test(influent, effluent, paired = TRUE) %>% print()
    
    str_c(process, analyte, p.value %>% round(4), n.samples, sep = " ") %>%
      print()
    
    # print(test.df$PctRmvFilt)
  }
}


# purpose: statistical test on pathogen removal due to filtration
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
df1 <- read_rds(pctrmv.path) %>%
  filter(Analyte %in% c("Cryptosporidium", "Giardia"))

# calculate log removal
df1 <- df1 %>%
  mutate(LogRmvFilt = log10(Influent/Effluent))

# run statistical tests
# DBF vs SMF; DBF vs TBF; and/or DBF vs SMF+TBF

## For Cryptosporidium

df.crypto <- df1 %>% filter(Analyte == "Cryptosporidium") %>%
  ungroup()

crypto.smf <- df.crypto %>% 
  filter(Process == "SMF") %>%
  select(PctRmvFilt) %>%
  unlist()

crypto.tbf <- df.crypto %>% 
  filter(Process == "TBF") %>%
  select(PctRmvFilt) %>%
  unlist()

crypto.dbf <- df.crypto %>% 
  filter(Process == "DBF") %>%
  select(PctRmvFilt) %>%
  unlist()

crypto.smf.tbf <- c(crypto.smf, crypto.tbf)

### Mann-Whitney U Test
### Nonparametric measure of whether they came from the same distribution
pc.smf.dbf <- wilcox.test(crypto.smf, crypto.dbf, paired = FALSE)$p.value
pc.tbf.dbf <- wilcox.test(crypto.tbf, crypto.dbf, paired = FALSE)$p.value
pc.smf.tbf.dbf <- wilcox.test(crypto.smf.tbf, crypto.dbf, paired = FALSE)$p.value

## For Giardia

df.giardia <- df1 %>% filter(Analyte == "Giardia") %>%
  ungroup()

giardia.smf <- df.giardia %>% 
  filter(Process == "SMF") %>%
  select(PctRmvFilt) %>%
  unlist()

giardia.tbf <- df.giardia %>% 
  filter(Process == "TBF") %>%
  select(PctRmvFilt) %>%
  unlist()

giardia.dbf <- df.giardia %>% 
  filter(Process == "DBF") %>%
  select(PctRmvFilt) %>%
  unlist()

giardia.smf.tbf <- c(giardia.smf, giardia.tbf)

### Mann-Whitney U Test
### Nonparametric measure of whether they came from the same distribution
pg.smf.dbf <- wilcox.test(giardia.smf, giardia.dbf, paired = FALSE)$p.value
pg.tbf.dbf <- wilcox.test(giardia.tbf, giardia.dbf, paired = FALSE)$p.value
pg.smf.tbf.dbf <- wilcox.test(giardia.smf.tbf, giardia.dbf, paired = FALSE)$p.value



