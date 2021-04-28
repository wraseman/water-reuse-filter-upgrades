# Purpose: visualize TrOC concentrations
# Author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in both of Eurofin's labs (California and South Bend)
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
df1 <- read_rds(clean.path) 

# visulize a single parameter for a single process at the influent and effluent
# with for-loop, we can look at each individual parameters
analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")
detect.bool.fill <- c("FALSE" = "#99d8c9", "TRUE" = "#2ca25f") ## assign color scale for detects/non-detects. source: # source: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
fig.resolution <- 300  # figure resolution (300 dpi)
# date.range <- range(df1$SampleDateTime)

for (analyte in analytes) {
  for (process in filters) {
# analyte <- analytes[6]
# process <- filters[1]

  process.dir <- str_c(process, "/")  # directory based on treatment process type
  
  pc.analyte.df <- filter(df1, 
                          Analyte == analyte,
                          Process == process)
  pc.analyte.units <- pc.analyte.df$Units %>% unique
  
  ## plot concentration vs. time for each TrOC for each type of filter
  p1 <- ggplot(pc.analyte.df, aes(x = SampleDateTime, y = LimitAdjResult)) +
    geom_point(aes(color = ProcessInfEff)) +
    ylab(str_c("Concentration (", pc.analyte.units, ")")) +
    xlab("Time") + 
    # xlim(date.range) +
    ggtitle(str_c(analyte, ", ", process)) +
    theme_bw()
  
  ## save plot as .tiff
  plot.dir1 <- "concentration-v-time/"
  tiff.name1 <- str_c(analyte, "_", "concentration-v-time.tiff")
  tiff.path1 <- str_c(fig.dir, process.dir, plot.dir1, tiff.name1)
  tiff(filename = tiff.path1,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p1)
  dev.off()
  
  ## plot detects vs. non-detects for each TrOC for each type of filter
  p2 <- pc.analyte.df %>% 
    ggplot(aes(ProcessInfEff)) +
    geom_bar(aes(fill=Detect)) +
    # scale_fill_brewer(palette = 4) +
    scale_fill_manual(values = detect.bool.fill) + 
    ggtitle(str_c(analyte, ", ", process)) +
    xlab("") +
    theme_bw()
  
  ## save plot as .tiff
  plot.dir2 <- "detect-nondetect_bar-plots/"
  tiff.name2 <- str_c(analyte, "_", "detect-nondetect_bar-plots.tiff")
  tiff.path2 <- str_c(fig.dir, process.dir, plot.dir2, tiff.name2)
  tiff(filename = tiff.path2,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p2)
  dev.off()
  }
}

# count the number of detects in influent and non-detects each process
df1 %>%
  group_by(ProcessInfEff, Process, Detect) %>%
  count()

p3 <- df1 %>%
  filter(Process %in% c("TBF", "SMF", "DBF")) %>%
  ggplot(aes(x=ProcessInfEff, fill=Detect)) +
  geom_bar(stat="count", position="dodge") +
  facet_wrap(Process ~ ., scales="free_y") +
  scale_fill_manual(values = detect.bool.fill) +
  theme_bw() +
  xlab("")

## save plot as .tiff
tiff.name3 <- "summary_detect-nondetect_bar-plots.tiff"
tiff.path3 <- str_c(fig.dir, tiff.name3)
tiff(filename = tiff.path3,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p3)
dev.off()

