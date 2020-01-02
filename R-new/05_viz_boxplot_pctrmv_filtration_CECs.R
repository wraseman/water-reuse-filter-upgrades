# purpose: create boxplots of percent removal of CECs due to filtration
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# user-defined functions
medianboxplt <- function(y) 
  c(label=median(y) %>% round(1), y=median(y))

# read in percent removal data
calc.dir <- "./data/eurofins-data/calculated/"
pctrmv.path <- str_c(calc.dir, "pctrmv-filtration.rds")
df1 <- read_rds(path = pctrmv.path) %>%
  filter(!(Analyte %in% c("Cryptosporidium", "Giardia")))

analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")

## define figure properties
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
pctrmv.dir <- "pctrmv-v-time/"
fig.resolution <- 300  # figure resolution (300 dpi)
subtitle.zoom <- "(negative outliers removed for readability)"
mytheme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  title = element_text(size = 18),
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
## plots with full extent of removal data
p1 <- ggplot(df2, aes(x=Process, y=PctRmvFilt)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Filter Type") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme 

### save plot as .tiff
tiff.name1 <- str_c("summary_boxplot_pctrmv-v-time_filtration_CECs.tiff")
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
tiff.name2 <- str_c("summary_boxplot_pctrmv-v-time_filtration_CECs_zoomed.tiff")
tiff.path2 <- str_c(fig.dir, tiff.name2)
tiff(filename = tiff.path2,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p2)
dev.off()

# plot percent removal for each filter by sorption category
for (process in filters) {
  
  temp.df2 <- filter(df2, Process == process)  # create temporary dataframe that only includes a single filter type
  
  ## plot with full extent of removal data
  p3 <- ggplot(temp.df2, aes(x=Sorption, y=PctRmvFilt)) +
    geom_boxplot() +
    ylab("Percent Removal") +
    xlab("Sorption") +
    geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    ggtitle(filter.fullnames[[process]]) +
    mytheme 
  
  ### save plot as .tiff
  tiff.name3 <- str_c("boxplot_pctrmv-v-time_filtration_by_sorption.tiff")
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
  
  tiff.name4 <- str_c("boxplot_pctrmv-v-time_filtration_by_sorption_zoomed.tiff")
  tiff.path4 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name4)
  tiff(filename = tiff.path4,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p4)
  dev.off()
}

# plot percent removal for each filter by biodegradation category
for (process in filters) {
  
  temp.df2 <- filter(df2, Process == process)  # create temporary dataframe that only includes a single filter type
  
  ## plot with full extent of removal data
  p5 <- ggplot(temp.df2, aes(x=Biodegradation, y=PctRmvFilt)) +
    geom_boxplot() +
    ylab("Percent Removal") +
    xlab("Biodegradability") +
    geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    ggtitle(filter.fullnames[[process]]) +
    mytheme 
  
  ### save plot as .tiff
  tiff.name5 <- str_c("boxplot_pctrmv-v-time_filtration_by_biodeg.tiff")
  tiff.path5 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name5)
  tiff(filename = tiff.path5,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p5)
  dev.off()
  
  ## plots with zoomed in view of removal data
  p6 <- p5 + 
    ylim(-50, 100) +
    labs(subtitle = subtitle.zoom)
  
  tiff.name6 <- str_c("boxplot_pctrmv-v-time_filtration_by_biodeg_zoomed.tiff")
  tiff.path6 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name6)
  tiff(filename = tiff.path6,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p6)
  dev.off()
}

# plot percent removal for each filter by combined sorption/biodegradation category
df3 <- df2 %>%
  # create combined biodegradation and sorption category
  mutate(BiodegSorption = if_else(Biodegradation == "Good" & Sorption == "Good", "Good-Biodeg, Good-Sorption", 
                                  if_else(Biodegradation == "Good" & Sorption == "Poor", "Good-Biodeg, Poor-Sorption", 
                                          if_else(Biodegradation == "Poor" & Sorption == "Good", "Poor-Biodeg, Good-Sorption", 
                                                  if_else(Biodegradation == "Poor" & Sorption == "Poor", "Poor-Biodeg, Poor-Sorption", NA_character_)))))
df3$BiodegSorption <- factor(df3$BiodegSorption,
                             levels = c("Poor-Biodeg, Poor-Sorption", "Poor-Biodeg, Good-Sorption", "Good-Biodeg, Poor-Sorption", "Good-Biodeg, Good-Sorption"), ordered = TRUE)

# plot by combined biodegration and sorption categories
for (process in filters) {
  
  temp.df3 <- filter(df3, Process == process)  # create temporary dataframe that only includes a single filter type
  
  ## plot with full extent of removal data
  p7 <- ggplot(data = temp.df3,
               aes_string(x="BiodegSorption", y="PctRmvFilt", fill="BiodegSorption")) +
    geom_boxplot(alpha= 0.7) +
    ggtitle(filter.fullnames[[process]]) +
    ylab("Percent Removal") +
    xlab("Biodegradability and Sorption Categories") +
    mytheme +
    theme(legend.position="none")
  
  ### save plot as .tiff
  tiff.name7 <- str_c("boxplot_pctrmv-v-time_filtration_by_biodeg-sorption.tiff")
  tiff.path7 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name7)
  tiff(filename = tiff.path7,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p7)
  dev.off()
  
  ## plots with zoomed in view of removal data
  p8 <- p7 + 
    ylim(-50, 100) +
    labs(subtitle = subtitle.zoom)
  
  ### save plot as .tiff
  tiff.name8 <- str_c("boxplot_pctrmv-v-time_filtration_by_biodeg-sorption_zoomed.tiff")
  tiff.path8 <- str_c(fig.dir, process, "/", pctrmv.dir, tiff.name8)
  tiff(filename = tiff.path8,
       height = 12, width = 17, units = 'cm',
       compression = "lzw", res = fig.resolution)
  print(p8)
  dev.off()
}



# ### Disinfection removal 
# pctrmv.cl <- select(filt.cec, PctRmvChlor1, PctRmvChlor2) %>%
#   gather(key = "Phase", value = "PercentRemoval") %>%
#   mutate(Phase = ifelse(Phase == "PctRmvChlor1", "1", 
#                         ifelse(Phase == "PctRmvChlor2", "2", NA)))
# 
# p2 <- ggplot(data = filter(pctrmv.cl), aes(x=Phase, y=PercentRemoval)) +
#   geom_boxplot() +
#   ylab("Percent Removal") +
#   ylim(-50, 100) +
#   geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
#   # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.5, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
#   mytheme 
# tiff(filename = str_c(figure.dir, "CECs_pctrmv_cl", ".tiff"),
#      height = 12, width = 17, units = 'cm',
#      compression = "lzw", res = fig.resolution)
# print(p2)
# dev.off()
# 
# ### Total removal 
# pctrmv.tot <- select(filt.cec, PctRmvTotal1, PctRmvTotal2) %>%
#   gather(key = "Phase", value = "PercentRemoval") %>%
#   mutate(Phase = ifelse(Phase == "PctRmvTotal1", "1", 
#                         ifelse(Phase == "PctRmvTotal2", "2", NA)))
# 
# p3 <- ggplot(data = filter(pctrmv.tot), aes(x=Phase, y=PercentRemoval)) +
#   geom_boxplot() +
#   ylab("Percent Removal") +
#   ylim(-50, 100) +
#   geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
#   # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.5, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
#   mytheme 
# tiff(filename = str_c(figure.dir, "CECs_pctrmv_total", ".tiff"),
#      height = 12, width = 17, units = 'cm',
#      compression = "lzw", res = fig.resolution)
# print(p3)
# dev.off()
# 
# ## Pathogens and turbidity
# filt.cryp <- filter(all.df, Parameter == "Cryptosporidium") %>% ungroup()
# filt.giar <- filter(all.df, Parameter == "Giardia") %>% ungroup()
# filt.turb <- filter(all.df, Parameter == "Turbidity") %>% ungroup()
# 
# ### Filter removal
# #### Cryptosporidium
# pctrmv.cryp <- select(filt.cryp, PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt) %>%
#   gather(key = "FilterType", value = "PercentRemoval") %>%
#   mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
#                              ifelse(FilterType == "PctRmvSMFilt", "SMF", 
#                                     ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))
# 
# #### specify order of filter types for boxplots
# pctrmv.cryp$FilterType <- factor(pctrmv.cryp$FilterType,
#                                  levels = c("SMF", "TBF", "DBF"), ordered = TRUE)
# 
# 
# 
# p4 <- ggplot(data = filter(pctrmv.cryp), aes(x=FilterType, y=PercentRemoval)) +
#   geom_boxplot() +
#   ylab("Percent Removal") +
#   xlab("Filter Type") +
#   ylim(-50, 100) +
#   geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
#   # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
#   mytheme +
#   ggtitle("Cryptosporidium")
# tiff(filename = str_c(figure.dir, "crypto_filter", ".tiff"),
#      height = 12, width = 17, units = 'cm',
#      compression = "lzw", res = fig.resolution)
# print(p4)
# dev.off()
# 
# #### Giardia
# pctrmv.giar <- select(filt.giar, PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt) %>%
#   gather(key = "FilterType", value = "PercentRemoval") %>%
#   mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
#                              ifelse(FilterType == "PctRmvSMFilt", "SMF", 
#                                     ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))
# 
# #### specify order of filter types for boxplots
# pctrmv.giar$FilterType <- factor(pctrmv.giar$FilterType,
#                                  levels = c("SMF", "TBF", "DBF"), ordered = TRUE)
# 
# p5 <- ggplot(data = filter(pctrmv.giar), aes(x=FilterType, y=PercentRemoval)) +
#   geom_boxplot() +
#   ylab("Percent Removal") +
#   xlab("Filter Type") +
#   ylim(-50, 100) +
#   geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
#   # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
#   mytheme +
#   ggtitle("Giardia")
# tiff(filename = str_c(figure.dir, "giar_filter", ".tiff"),
#      height = 12, width = 17, units = 'cm',
#      compression = "lzw", res = fig.resolution)
# print(p5)
# dev.off()
# 
# #### Turbidity
# pctrmv.turb <- select(filt.turb, PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt) %>%
#   gather(key = "FilterType", value = "PercentRemoval") %>%
#   mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
#                              ifelse(FilterType == "PctRmvSMFilt", "SMF", 
#                                     ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))
# 
# #### specify order of filter types for boxplots
# pctrmv.turb$FilterType <- factor(pctrmv.turb$FilterType,
#                                  levels = c("SMF", "TBF", "DBF"), ordered = TRUE)
# 
# p6 <- ggplot(data = filter(pctrmv.turb), aes(x=FilterType, y=PercentRemoval)) +
#   geom_boxplot() +
#   ylab("Percent Removal") +
#   xlab("Filter Type") +
#   ylim(-50, 100) +
#   geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
#   # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
#   mytheme +
#   ggtitle("Turbidity")
# tiff(filename = str_c(figure.dir, "turb_filter", ".tiff"),
#      height = 12, width = 17, units = 'cm',
#      compression = "lzw", res = fig.resolution)
# print(p6)
# dev.off()
# 
# ## empty plot
# 
# p7 <- ggplot(data = filter(pctrmv.df), aes(x=FilterType, y=PercentRemoval)) +
#   # geom_boxplot() +
#   ylab("Percent Removal") +
#   xlab("Filter Type") +
#   ylim(-50, 100) +
#   # geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
#   # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
#   mytheme 
# tiff(filename = str_c(figure.dir, "empty_CECs_pctrmv", ".tiff"),
#      height = 12, width = 17, units = 'cm',
#      compression = "lzw", res = fig.resolution)
# print(p7)
# dev.off()


