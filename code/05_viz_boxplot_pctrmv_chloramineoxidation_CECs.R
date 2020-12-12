# purpose: create boxplots of percent removal of TrOCs due to Chloramine Oxidation
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# # user-defined functions
# medianboxplt <- function(y) 
#   c(label=median(y) %>% round(1), y=median(y))

# read in percent removal data
calc.dir <- "./data/eurofins-data/calculated/"
pctrmv.path <- str_c(calc.dir, "pctrmv-chloramineoxidation.rds")
df1 <- read_rds(path = pctrmv.path) %>%
  filter(!(Analyte %in% c("Cryptosporidium", "Giardia"))) %>%
  filter(Analyte != "BPA")  # no Chloramine Oxidation category for BPA

analytes <- df1$Analyte %>% unique
filters <- c("SMF+TBF", "DBF")

## reverse order of factors for Chloramine Oxidation
df1$ChloramineOxidation <- factor(df1$ChloramineOxidation, 
                                  levels=levels(df1$ChloramineOxidation)[order(levels(df1$ChloramineOxidation), decreasing = TRUE)])

## define figure properties
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
pctrmv.dir <- "pctrmv_boxplots/"
fig.resolution <- 300  # figure resolution (300 dpi)
subtitle.zoom <- "(negative outliers removed for readability)"
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
                      levels = filters, ordered = TRUE)

### create an array that acts like a dictionary for filter names 
###   source: https://stackoverflow.com/questions/2858014/working-with-dictionaries-lists-in-r
filter.fullnames <- c("Synthetic Media Filter and Traveling Bed Filter", "Deep Bed Filter")
names(filter.fullnames) <- filters  # abbreviations

# plot percent removal by filter type 
## plots with full extent of removal data
p1 <- ggplot(df2, aes(x=Process, y=PctRmvChlormaineOx)) +
  geom_boxplot() +
  ylab("Removal by Chloramine Oxidation (%)") +
  xlab("Filter Type") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme 

### save plot as .tiff
tiff.name1 <- str_c("summary_pctrmv_boxplots_ChlormaineOx_TrOCs.tiff")
tiff.path1 <- str_c(fig.dir, tiff.name1)
tiff(filename = tiff.path1,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p1)
dev.off()

## plots with zoomed in view of removal data
p2 <- p1 + 
  ylim(-50, 100) +
  labs(subtitle = subtitle.zoom)

### save plot as .tiff
tiff.name2 <- str_c("summary_boxplot_pctrmv-v-time_ChlormaineOx_TrOCs_zoomed.tiff")
tiff.path2 <- str_c(fig.dir, tiff.name2)
tiff(filename = tiff.path2,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p2)
dev.off()

# plot percent removal for each filter by Chloramine Oxidation category
for (process in filters) {
  
  temp.df2 <- filter(df2, Process == process)  # create temporary dataframe that only includes a single filter type
  
  ## plot with full extent of removal data
  p3 <- ggplot(temp.df2, aes(x=ChloramineOxidation, y=PctRmvChlormaineOx)) +
    geom_boxplot(aes(fill = ChloramineOxidation), alpha = 0.7) +
    ylab("Removal by Chloramine Oxidation (%)") +
    xlab("Chloramine Oxidation Category") +
    geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    ggtitle(process) +
    mytheme 
  
  ### save plot as .tiff
  tiff.name3 <- str_c("pctrmv_boxplots_ChlormaineOx_by_ChlormaineOxcategory.tiff")
  tiff.path3 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name3)
  tiff(filename = tiff.path3,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p3)
  dev.off()
  
  ## plots with zoomed in view of removal data
  p4 <- p3 + 
    ylim(-50, 100) +
    labs(subtitle = subtitle.zoom)
  
  tiff.name4 <- str_c("pctrmv_boxplots_ChlormaineOx_by_ChlormaineOxcategory_zoomed.tiff")
  tiff.path4 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name4)
  tiff(filename = tiff.path4,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p4)
  dev.off()
}

# plot "Good Chloramine Oxidation": triclosan (it seems to have high removal) and acetaminophen
detect.bool.fill <- c("SMF+TBF" = "#bcbddc", "DBF" = "#756bb1") ## assign color scale for detects/non-detects. source: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
goodox.analytes <- c("Acetaminophen", "Triclosan")
for (analyte in goodox.analytes) {
  df3 <- filter(df2, Analyte == analyte) %>%
    ungroup
  
  p5 <- ggplot(df3, aes(x = PctRmvChlormaineOx)) +
    geom_histogram(binwidth = 5, aes(fill = Process)) +
    scale_fill_manual(values = detect.bool.fill) + 
    xlab("Removal by Chloramine Oxidation (%)") +
    ylab("Number of Samples") +
    ggtitle(analyte) +
    mytheme
  
  tiff.name5 <- str_c(analyte, "_hist_pctrmv_by_ChlormaineOx.tiff")
  tiff.path5 <- str_c(fig.dir, tiff.name5)
  tiff(filename = tiff.path5,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p5)
  dev.off()
}
