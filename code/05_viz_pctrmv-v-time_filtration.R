# purpose: visualize percent removal of compounds vs. time due to filtration
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in both of Eurofin's labs (California and South Bend)
calc.dir <- "./data/eurofins-data/calculated/"
pctrmv.path <- str_c(calc.dir, "pctrmv-filtration.rds")
df1 <- read_rds(path = pctrmv.path) 

# plot percent removal over time for each Analyte for each Filter type
analytes <- unique(df1$Analyte)  # get array of different analytes
filters <- c("TBF", "SMF", "DBF")
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
fig.resolution <- 300  # figure resolution (300 dpi)
sign.fill <- c("Negative" = "red", "Positive" = "darkgreen", "Zero" = "black")  # specify fill colors

for (analyte in analytes) {
  for (process in filters) {
    # analyte <- analytes[1]
    # process <- filters[1]
    
    process.dir <- str_c(process, "/")  # directory based on treatment process type
    
    temp.df1 <- filter(df1,
                       Analyte == analyte,
                       Process == process)
    
    p1 <- ggplot(temp.df1, aes(x = SampleDate, y = PctRmvFilt)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_col(aes(fill = as.factor(PctRmvFilt_Sign)), position = "dodge") +
      scale_fill_manual(values = sign.fill, name = "Removal") +
      geom_label(aes(label = round(PctRmvFilt, digits = 1))) +
      ylab("Removal (%)") +
      xlab("Date") +
      ggtitle(analyte)
    
    ## save plot as .tiff
    plot.dir1 <- "pctrmv-v-time/"
    tiff.name1 <- str_c(analyte, "_", "pctrmv-v-time_filtration.tiff")
    tiff.path1 <- str_c(fig.dir, process.dir, plot.dir1, tiff.name1)
    tiff(filename = tiff.path1,
         height = 12, width = 17, units = 'cm',
         compression = "lzw", res = fig.resolution)
    print(p1)
    dev.off()
  }
}