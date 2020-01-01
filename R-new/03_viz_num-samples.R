# Purpose: visualize sampling counts for pathogens and CECs
# Author: Billy Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# function purpose: convert numeric (less than 100) into two character string
## input: num (numeric)
## return: two_char_string (string)
## example: num = 1 would return "01"
## example: num = 12 would return "12"
num_to_two_char_string <- function (num) {
  if (num < 10) {
    two_char_string <- str_c("0", num)  # if less than 10, add a 0 in front of the value
  } else {
    two_char_string <- str_c(num)
  }
  return(two_char_string)
}

# read in cleaned data from lab samples tested in Eurofin's California lab
clean.dir <- "./data/eurofins-data/clean/"
clean.path <- str_c(clean.dir, "combined-lab-results_clean.rds")
df1 <- read_rds(path = clean.path) 

# visulize a single parameter for a single process at the influent and effluent
# with for-loop, we can look at each individual parameters
analytes <- df1$Analyte %>% unique
filters <- c("TBF", "SMF", "DBF")
# detect.bool.fill <- c("FALSE" = "#99d8c9", "TRUE" = "#2ca25f") ## assign color scale for detects/non-detects. source: # source: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
fig.dir <- "./figures/combined-lab-results/"  # figure directory for California laboratory results
fig.resolution <- 300  # figure resolution (in dpi)

# visualize barplot of pathogen samples over time (by month) 
pathogens <- c("Cryptosporidium", "Giardia")
pth.df1 <- df1 %>%
  filter(Analyte %in% pathogens)

pth.count1 <- pth.df1 %>%
  mutate(month = lubridate::month(SampleDateTime),
         year = lubridate::year(SampleDateTime)) %>%
  group_by(month, year) %>%
    count() %>%
  mutate(day = 1) %>%  # approximate day to the 1st for every month for ease in plotting
  mutate(Date = lubridate::ymd(str_c(year, num_to_two_char_string(month), 
                         num_to_two_char_string(day))))

p1 <- ggplot(data = pth.count1, aes(x = Date, y = n)) +
  geom_col() + 
  xlab("Date") +
  ylab("Number of Samples") +
  ggtitle("Pathogen Sampling") +
  theme_bw()

## save plot as .tiff
tiff.name1 <- "summary_sampling-counts_pathogens.tiff"
tiff.path1 <- str_c(fig.dir, tiff.name1)
tiff(filename = tiff.path1,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p1)
dev.off()

# visualize barplot of CECs samples over time (by month)
cec.df1 <- df1 %>%
  filter(!(Analyte %in% pathogens))

cec.count1 <- cec.df1 %>%
  mutate(month = lubridate::month(SampleDateTime),
         year = lubridate::year(SampleDateTime)) %>%
  group_by(month, year) %>%
  count() %>%
  mutate(day = 1) %>%  # approximate day to the 1st for every month for ease in plotting
  mutate(Date = lubridate::ymd(str_c(year, num_to_two_char_string(month),
                                     num_to_two_char_string(day))))

p2 <- ggplot(data = cec.count1, aes(x = Date, y = n)) +
  geom_col() +
  xlab("Date") +
  ylab("Number of Samples") +
  ggtitle("Contaminants of Emerging Concern Sampling") +
  theme_bw()

## save plot as .tiff
tiff.name2 <- "summary_sampling-counts_CECs.tiff"
tiff.path2 <- str_c(fig.dir, tiff.name2)
tiff(filename = tiff.path2,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution) 
print(p2)
dev.off()

# plot counts of individual CEC samples
cec.count2 <- cec.df1 %>%
  group_by(Analyte) %>%
  count() 

p3 <- ggplot(cec.count2, aes(x = Analyte, y = n)) + 
  geom_col() + 
  xlab("Analyte") +
  ylab("Number of Samples") +
  ggtitle("Contaminants of Emerging Concern Sampling") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

## save plot as .tiff
tiff.name3 <- "summary_sampling-counts_indiv-CECs.tiff"
tiff.path3 <- str_c(fig.dir, tiff.name3)
tiff(filename = tiff.path3,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution) 
print(p3)
dev.off()

# plot counts of individual pathogen samples
pth.count2 <- pth.df1 %>%
  group_by(Analyte) %>%
  count() 

p4 <- ggplot(pth.count2, aes(x = Analyte, y = n)) + 
  geom_col() + 
  xlab("Analyte") +
  ylab("Number of Samples") +
  ggtitle("Pathogen Sampling") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

## save plot as .tiff
tiff.name4 <- "summary_sampling-counts_indiv-pathogens.tiff"
tiff.path4 <- str_c(fig.dir, tiff.name4)
tiff(filename = tiff.path4,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution) 
print(p4)
dev.off()

# figure out why there are some CECs with 127 samples and others with 130

## Q: is it because of triplicate testing?
## A: no, it is not because of triplicate testing
cec.count3 <- cec.df1 %>%
  filter(Duplicate == FALSE, 
         Triplicate == FALSE) %>%
  group_by(Analyte) %>%
  count() 

## Q: is it because of "Lab Test Blanks"?
## A: no, it is not because of triplicate testing
cec.count4 <- cec.df1 %>%
  filter(Process != "lab_blank") %>%
  group_by(Analyte) %>%
  count() 

## Q: is it a difference between California and South Bend tests?
## A: yes!! The three extra test are those performed South Bend. The 
##    remaining 127 are California-based tests. 
cec.count5 <- cec.df1 %>%
  filter(Lab == "California") %>%
  group_by(Analyte) %>%
  count()

# visualize South Bend and California tests for CECs
cec.count6 <- cec.df1 %>%
  group_by(Analyte, Lab) %>%
  count() 

p5 <- ggplot(cec.count6, aes(x = Analyte, y = n)) + 
  geom_col(aes(fill = Lab)) + 
  xlab("Analyte") +
  ylab("Number of Samples") +
  ggtitle("Contaminants of Emerging Concern Sampling") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

## save plot as .tiff
tiff.name5 <- "summary_sampling-counts_indiv-CECs_by-lab.tiff"
tiff.path5 <- str_c(fig.dir, tiff.name5)
tiff(filename = tiff.path5,
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution) 
print(p5)
dev.off()

