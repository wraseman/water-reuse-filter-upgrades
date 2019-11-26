# Purpose: read in and clean data from California laboratory results
#   Data includes pathogen and contaminants of emerging concern (CEC) concentrations
# Output: ./data/cleaned_all-filters.rds (tidy, cleaned dataframe)
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in data from lab samples tested in Eurofin's California lab
ef.dir <- "./data/eurofins-data/"
ca.dir <- str_c(ef.dir, "california-lab-results/")

## read in all California spreadsheets at once
## source: https://rpubs.com/LMunyan/363306
file.array <- list.files(path=ca.dir)  # create an array of the files from your target directory
id.table <- "Sample_ID_key.xlsx"  # Sample ID table for location IDs
excel.array <- str_subset(file.array, "xlsx") %>%  # only include Excel spreadsheets
  str_subset("Sample_ID_key.xlsx", negate=TRUE)  # don't keep Sample ID key file

ca.df1 <- data.frame()  # empty dataframe for California data
# for (i in 1:length(file.array )){
#   temp.df <- read_excel(str_c(ca.dir1, excel.array[i]), range = cell_cols("A:AI"))
#   ca.df1 <- rbind(ca.df1, temp.df)  # for each iteration, bind the new data to the building dataset
# }

## read in data with specified columns
col.types <- c("numeric", "text", "text", "numeric", "numeric", "text", "text", 
              "numeric", "numeric", "date", "date", "date",	"date",	
              "text",	"text",	"text",	"text",	"text",	"text",	
              "numeric", 	"text",	"text",	"numeric",	"numeric",	
              "numeric",	"numeric",	"numeric",	"numeric",	"numeric", 	
              "date",	"date",	"date",	"date",	"text",	"text")  # specify column types
ca.df1 <- read_excel(str_c(ca.dir, excel.array[1]), range = cell_cols("A:AI"),
                      col_types = col.types)

## select only the columns that are needed and remove whitespace from column names
selected.cols <- c("Sample ID", "Sample Date", "Sample Time", 
                      "Analyte", "Result", "Detection Limit", 
                      "Units", "Dilution")
ca.df2 <- select(ca.df1, selected.cols)
cols.no.spaces <- str_replace_all(selected.cols, " ", "")
colnames(ca.df2) <- cols.no.spaces

## reformat non-detects: replace "ND" with NAs in Result column and make column of type numeric
ca.df3 <- mutate(ca.df2, Detect = if_else(Result=="ND", FALSE, TRUE))  # create column to record non-detects
nondetect.index <- ca.df3$Result=="ND"
ca.df3$Result[nondetect.index] <- NA

ca.df3 <- mutate(ca.df3, Result = as.numeric(Result))  # convert Result to numeric

## combine date and time information into a single datetime column
ca.df4 <- mutate(ca.df3, SampleTime = hms::as_hms(SampleTime)) %>%
  mutate(SampleDateTime = as.POSIXct(str_c(SampleDate, SampleTime), format="%Y-%m-%d %H:%M:%S")) %>%
  select(-SampleDate, -SampleTime)  # remove sample date and time columns

# read in Sample ID key
id.df <- read_excel(str_c(ca.dir, id.table))
colnames(id.df) <- c("SampleID", "ClientID")
id.df$SampleID <- as.character(id.df$SampleID)

## join data with sample ID and clean data
ca.df5 <- left_join(ca.df4, id.df)  # add sample location (raw data) using Sample ID key

## identify duplicate and triplicate samples and extract location
duplicates <- c("Final-2", "DBF-E-2", "DBF-I-2", 
                "TBF-I Dup Week 2",
                "TBF-E Dup Week 2",
                "SMF-E Week 2 Dup",
                "Final Week 2 Dup")
triplicates <- c("Final-3", "DBF-E-3", "DBF-I-3",
                 "TBF-I Trip Week 2",
                 "TBF-E Trip Week 2",
                 "SMF-E Week 2 Trip",
                 "Final Week 2 Trip")
ca.df6 <- ca.df5 %>%
  mutate(Duplicate = if_else(ClientID %in% duplicates, TRUE, FALSE),
         Triplicate = if_else(ClientID %in% triplicates, TRUE, FALSE)) %>%  # identify duplicate and triplicate samples
  mutate(Location = str_sub(ClientID, 1, 5)) %>%  # extract location from Client ID
  filter(Location != "LTB")  # remove "LTB" samples (I do not know what this abbreviation stands for)

## 

