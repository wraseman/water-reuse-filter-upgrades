# purpose: create boxplots of percent removal of CECs due to chlorine oxidation
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
pctrmv.path <- str_c(calc.dir, "pctrmv-cloxidation.rds")
df1 <- read_rds(path = pctrmv.path) %>%
  filter(!(Analyte %in% c("Cryptosporidium", "Giardia"))) %>%
  filter(Analyte != "BPA")  # no Chlorine Oxidation category for BPA

analytes <- df1$Analyte %>% unique
filters <- c("SMF+TBF", "DBF")

## reverse order of factors for Chlorine Oxidation
df1$ClOxidation <- factor(df1$ClOxidation, 
                   levels=levels(df1$ClOxidation)[order(levels(df1$ClOxidation), decreasing = TRUE)])

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
p1 <- ggplot(df2, aes(x=Process, y=PctRmvClOx)) +
  geom_boxplot() +
  ylab("Removal by Chlorine Oxidation (%)") +
  xlab("Filter Type") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme 

### save plot as .tiff
tiff.name1 <- str_c("summary_pctrmv_boxplots_clox_CECs.tiff")
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
tiff.name2 <- str_c("summary_boxplot_pctrmv-v-time_clox_CECs_zoomed.tiff")
tiff.path2 <- str_c(fig.dir, tiff.name2)
tiff(filename = tiff.path2,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p2)
dev.off()

# plot percent removal for each filter by chlorine oxidation category
for (process in filters) {

  temp.df2 <- filter(df2, Process == process)  # create temporary dataframe that only includes a single filter type

  ## plot with full extent of removal data
  p3 <- ggplot(temp.df2, aes(x=ClOxidation, y=PctRmvClOx)) +
    geom_boxplot(aes(fill = ClOxidation), alpha = 0.7) +
    ylab("Removal by Chlorine Oxidation (%)") +
    xlab("Chlorine Oxidation Category") +
    geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    ggtitle(process) +
    mytheme 
  
  ### save plot as .tiff
  tiff.name3 <- str_c("pctrmv_boxplots_clox_by_cloxcategory.tiff")
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
  
  tiff.name4 <- str_c("pctrmv_boxplots_clox_by_cloxcategory_zoomed.tiff")
  tiff.path4 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name4)
  tiff(filename = tiff.path4,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p4)
  dev.off()
}

# plot triclosan (it seems to have high removal)
detect.bool.fill <- c("SMF+TBF" = "#bcbddc", "DBF" = "#756bb1") ## assign color scale for detects/non-detects. source: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
df3 <- filter(df2, Analyte == "Triclosan") %>%
  ungroup

p5 <- ggplot(df3, aes(x = PctRmvClOx)) +
  geom_histogram(binwidth = 5, aes(fill = Process)) +
  scale_fill_manual(values = detect.bool.fill) + 
  xlab("Removal by Chlorine Oxidation (%)") +
  ylab("Number of Samples") +
  ggtitle("Triclosan") +
  mytheme

tiff.name5 <- str_c("Triclosan_hist_pctrmv_by_clox.tiff")
tiff.path5 <- str_c(fig.dir, tiff.name5)
tiff(filename = tiff.path5,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p5)
dev.off()
  
