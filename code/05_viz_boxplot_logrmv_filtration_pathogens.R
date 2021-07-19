# purpose: create boxplots of log removal of pathogens due to filtration
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
  filter(Analyte %in% c("Cryptosporidium", "Giardia")) %>%
  mutate(LogRmvFilt = log10(Influent/Effluent))  # calculate log removal 

analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")

## define figure properties
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
pctrmv.dir <- "pctrmv-v-time/"
fig.resolution <- 300  # figure resolution (300 dpi)
subtitle.zoom <- "(extreme negative values removed)"
mytheme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  title = element_text(size = 18),
  plot.subtitle = element_text(size = 12),
  legend.text = element_text(size = 16))

### specify order of filter types for boxplots
df2 <- df1
df2$Process <- factor(df1$Process,
                      levels = c("SMF", "TBF", "DBF"), ordered = TRUE)

### create an array that acts like a dictionary for filter names 
###   source: https://stackoverflow.com/questions/2858014/working-with-dictionaries-lists-in-r
filter.fullnames <- c("Synthetic Media Filter", "Traveling Bed Filter", "Deep Bed Filter")
names(filter.fullnames) <- c("SMF", "TBF", "DBF")  # abbreviations

# plot percent removal by filter type 
for (analyte in analytes) {
  
  temp.df2 <- filter(df2, Analyte == analyte)
  
  ## plots with full extent of removal data
  p1 <- ggplot(temp.df2, aes(x=Process, y=LogRmvFilt)) +
    geom_boxplot() +
    ylab("Log Removal") +
    xlab("Filter Type") +
    geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    ggtitle(analyte) +
    mytheme +
    labs(subtitle = "sample sizes: SMF (n=7), TBF (n=7), DBF (n=6)")
  
  ### save plot as .tiff
  tiff.name1 <- str_c(analyte, "_summary_boxplot_logrmv-v-time_filtration_pathogens.tiff")
  tiff.path1 <- str_c(fig.dir, tiff.name1)
  tiff(filename = tiff.path1,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p1)
  dev.off()
  
  # ## plots with zoomed in view of removal data
  # p2 <- p1 + 
  #   ylim(-50, 100) +
  #   labs(subtitle = subtitle.zoom)
  # 
  # ### save plot as .tiff
  # tiff.name2 <- str_c(analyte, "_summary_boxplot_logrmv-v-time_filtration_pathogens_zoomed.tiff")
  # tiff.path2 <- str_c(fig.dir, tiff.name2)
  # tiff(filename = tiff.path2,
  #      height = 12, width = 17, units = 'cm',
  #      compression = "lzw", res = fig.resolution)
  # print(p2)
  # dev.off()
}

