# purpose: create dataset for percent removal of CECs and pathogens due to filtration
# author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in cleaned data from lab samples tested in both of Eurofin's labs (California and South Bend)
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
df1 <- read_rds(path = clean.path) 
  # filter(!(Analyte %in% c("Cryptosporidium", "Giardia")))

# visulize a single parameter for a single process at the influent and effluent
# with for-loop, we can look at each individual parameters
analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")
# detect.bool.fill <- c("FALSE" = "#99d8c9", "TRUE" = "#2ca25f") ## assign color scale for detects/non-detects. source: # source: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
fig.resolution <- 300  # figure resolution (300 dpi)
# date.range <- range(df1$SampleDateTime)

# calculate percent removal for each analyte due to filtration (assuming concentration of non-detects is the detection limit)
df2 <- df1 %>% 
  filter(Duplicate == FALSE, 
         Triplicate == FALSE) %>%  # remove extra values from triplicate analysis
  filter(Process != "lab_blank") %>% 
  mutate(SampleDate = lubridate::floor_date(SampleDateTime, unit = "6 hours")) %>%  # round datetime to nearest 6 hours
  mutate(DilutAdjResult_NDetEqualDetLim = 
           if_else(Detect == FALSE, DetectionLimit, DilutAdjResult)) %>%
  select(Analyte, SampleDate, Process, DilutAdjResult_NDetEqualDetLim, ProcessInfEff)

## create influent and effluent columns for each filter
df3 <- df2 %>%
  group_by(SampleDate, Analyte, ProcessInfEff) %>%
  spread(key = ProcessInfEff, value = DilutAdjResult_NDetEqualDetLim) %>%
  filter(Process != "clearwell")  # just look at removal due to filtration for now 

## calculate percent removal
df4 <- df3 %>%
  mutate(PctRmvFilt = ((Influent - Effluent)/Influent) * 100, 
         PctRmvFilt_Sign = if_else(sign(PctRmvFilt) == 1, "Positive",
                                   if_else(sign(PctRmvFilt) == 0, "Zero",
                                           "Negative")))

# plot percent removal over time
cols <- c("Negative" = "red", "Positive" = "darkgreen", "Zero" = "black")

for (analyte in analytes) {
  for (process in filters) {
    # analyte <- analytes[1]
    # process <- filters[1]
    
    process.dir <- str_c(process, "/")  # directory based on treatment process type
    
    temp.df1 <- filter(df4, 
                       Analyte == analyte, 
                       Process == process)
    
    p1 <- ggplot(temp.df1, aes(x = SampleDate, y = PctRmvFilt)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_col(aes(fill = as.factor(PctRmvFilt_Sign)), position = "dodge") +
      scale_fill_manual(values = cols, name = "Removal") +
      geom_label(aes(label = round(PctRmvFilt, digits = 1))) +
      ylab("Removal (%)") +
      xlab("Date") +
      
      ggtitle(analyte)
    
    
    ## save plot as .tiff
    plot.dir1 <- "pctrmv-v-time/"
    tiff.name1 <- str_c(analyte, "_", "pctrmv-v-time.tiff")
    tiff.path1 <- str_c(fig.dir, process.dir, plot.dir1, tiff.name1)
    tiff(filename = tiff.path1,
         height = 12, width = 17, units = 'cm',
         compression = "lzw", res = fig.resolution)
    print(p1)
    dev.off()
  }
}

# ## plot concentration vs. time for each CEC for each type of filter
# p4 <- ggplot(pc.analyte.df, aes(x = SampleDateTime, y = DilutAdjResult)) +
#   geom_point(aes(color = ProcessInfEff)) +
#   ylab(str_c("Concentration (", pc.analyte.units2, ")")) +
#   xlab("Time") + 
#   # xlim(date.range) +
#   ggtitle(str_c(analyte, ", ", process)) +
#   theme_bw()
# 
# ## save plot as .tiff
# plot.dir4 <- "pctrmv-v-time/"
# tiff.name1 <- str_c(analyte, "_", "pctrmv-v-time.tiff")
# tiff.path1 <- str_c(fig.dir, process.dir, plot.dir1, tiff.name1)
# tiff(filename = tiff.path1,
#      height = 12, width = 17, units = 'cm',
#      compression = "lzw", res = fig.resolution)
# print(p1)
# dev.off()
