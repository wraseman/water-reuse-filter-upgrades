# Purpose: read in and clean data from California laboratory results
#   Data includes trace organic contaminants (TrOCs)
# Output: ./data/eurofins-data/clean/california-lab-results_clean.rds (tidy, cleaned dataframe)
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in data from lab samples tested in Eurofin's California lab
ef.dir <- "./data/eurofins-data/"
ca.dir <- str_c(ef.dir, "california-lab-results/")
rmvcat.dir <- "./removal categories/"

## read in all California spreadsheets at once
## source: https://rpubs.com/LMunyan/363306
file.array <- list.files(path=ca.dir)  # create an array of the files from your target directory
id.table <- "Sample_ID_key.xlsx"  # Sample ID table for location IDs
excel.array <- str_subset(file.array, "xlsx") %>%  # only include Excel spreadsheets
  str_subset("Sample_ID_key.xlsx", negate=TRUE)  # don't keep Sample ID key file

ca.df1 <- data.frame()  # empty dataframe for California data

## read in data with specified columns
col.types <- c("numeric", "text", "text", "numeric", "numeric", "text", "text",
               "numeric", "numeric", "date", "date", "date",	"date",
               "text",	"text",	"text",	"text",	"text",	"text",
               "numeric", 	"text",	"text",	"numeric",	"numeric",
               "numeric",	"numeric",	"numeric",	"numeric",	"numeric",
               "text",	"text",	"text",	"text",	"text",	"text")  # specify column types

# ca.df1 <- read_excel(str_c(ca.dir, excel.array[9]), range = cell_cols("A:AI"),
#                      col_types = col.types)

for (i in 1:length(excel.array)){
  temp.df <- read_excel(str_c(ca.dir, excel.array[i]), range = cell_cols("A:AI"),
                        col_types = col.types)
  ca.df1 <- rbind(ca.df1, temp.df)  # for each iteration, bind the new data to the building dataset
}

## select only the columns that are needed and remove whitespace from column names
selected.cols <- c("Sample ID", "Sample Date", "Sample Time",
                   "Analyte", "Result", "Detection Limit",
                   "Units", "Dilution", "QC Name")
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

## join data with Sample ID to get the "client ID" which contains location information and filter type
### account for errors in Eurofins data entry for Sample ID
id.errors <- c("2885588", "3124086", "3124087")  # errors identified manually by author of this code
id.corrections <- c("3885588", "3421086", "3421087")

ca.df5 <- mutate(ca.df4, SampleID = if_else(SampleID==id.errors[1], id.corrections[1],
                                            if_else(SampleID==id.errors[2], id.corrections[2],
                                                    if_else(SampleID==id.errors[3], id.corrections[3],
                                                            SampleID))))

### check that Sample IDs are correct
#### for all 7 digit Sample IDs, check that they match the Sample ID key options
ids.7dig <- mutate(ca.df5, SampleIDLength = str_length(SampleID)) %>%
  filter(SampleIDLength == 7) %>%
  select(SampleID) %>% unique %>% unlist

check.ids <- (ids.7dig %in% id.df$SampleID) %>%
  all(TRUE)  # confirmed that all 7 digit Sample ID now match those in the key table

### add sample location (raw data) using Sample ID key
ca.df5 <- left_join(ca.df5, id.df)

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
  mutate(LocationProcessType = str_sub(ClientID, 1, 5))  %>%  # extract location from Client ID
  filter(is.na(QCName))  %>% # remove quality control data except for laboratory trip blanks (LTB)
  select(-QCName)

## get location and filter type
influents <- c("TBF-I", "DBF-I")
effluents <- c("TBF-E", "SMF-E", "Final", "DBF-E")
clearwell <- "Final"
blank <- "LTB"
tbf <- c("TBF-I", "TBF-E")  # traveling bridge filters
smf <- "SMF-E"  # synthetic media filters
dbf <- c("DBF-I", "DBF-E")  # deep bed filtration

## unpack LocationProcessType to extract whether location is influent/effluent and what the unit process is
ca.df7 <- ca.df6 %>%
  ### specify whether it is a process influent or effluent and the process type
  mutate(ProcessInfEff =
                   if_else(LocationProcessType %in% influents, "Influent",
                           if_else(LocationProcessType %in% effluents, "Effluent",
                                   NA_character_))) %>%
  ### determine the process type: filter (TBF, SMF, DBF), clearwell, or laboratory blank (technically not a process)
  mutate(Process = if_else(LocationProcessType %in% tbf, "TBF",
                            if_else(LocationProcessType %in% smf, "SMF",
                                    if_else(LocationProcessType %in% dbf, "DBF",
                                            if_else(LocationProcessType %in% clearwell, "clearwell",
                                                    if_else(LocationProcessType %in% blank, "lab_blank",
                                                            NA_character_))))))
## in the plant, the stream is bifurcated between TBF and SMF. This stream is
##   called "TBF-I" but it is actually the infuent for both TBF and SMF.
##   For this reason, duplicate all TBF-I to specify Process = "SMF" and ProcessInfEff = "Influent"
smf.inf <- filter(ca.df7, Process == "TBF", ProcessInfEff == "Influent") %>%
  mutate(Process = "SMF")

## add duplicated observations for TBF-I
ca.df7 <- bind_rows(ca.df7, smf.inf)

### LocationProcessType matrix
## TBF-I - ProcessInfEff = Influent, Process = TBF
## TBF-I - ProcessInfEff = Influent, Process = SMF (note that this is a special case)
## TBF-E - ProcessInfEff = Effluent, Process = TBF
## SMF-E - ProcessInfEff = Effluent, Process = SMF
## Final - ProcessInfEff = Effluent, Process = clearwell
## LTB   - ProcessInfEff - NA      , Process = lab_blank
## DBF-I - ProcessInfEff = Influent, Process = DBF
## DBF-E - ProcessInfEff = Effluent, Process = DBF

## account for dilution of samples
ca.df8 <- mutate(ca.df7, DilutAdjResult = Result * Dilution)  # calculate concentration after adjusting for lab dilution

### make Process influent and effluent factors so when plotted as boxplot or barplot, influent comes before effluent
ca.df8$ProcessInfEff <- factor(ca.df8$ProcessInfEff,
                               levels = c("Influent", "Effluent"))

## filter data for compounds of interest
## keep columns and analytes necessary for analysis
analytes <- c("Meprobamate", "Dilantin", "Sulfamethoxazole",
               "TCEP", "Carbamazepine",
               "Gemfibrozil", "Ibuprofen", "Acetaminophen",
               "Triclosan", "Caffeine", "Fluoxetine")
# analytes <- c("Meprobamate", "Dilantin", "Sulfamethoxazole",
#               "BPA", "TCEP", "TCPP",
#               "Carbamazepine", "Salicylic Acid",
#               "Gemfibrozil", "Ibuprofen", "Acetaminophen",
#               "Triclosan", "Caffeine", "Fluoxetine")
ca.df9 <- filter(ca.df8, Analyte %in% analytes)

### compare analyte names in lab data and analytes array
matching.analytes <- (analytes %in% ca.df8$Analyte)
mismatched <- analytes[matching.analytes==FALSE]
check.analytes <- all(matching.analytes, TRUE)

## assign biodegradation, sorption, and chloramine oxidation categories for each analyte
bsc.table <- read_excel(str_c(rmvcat.dir, "Biodeg-Sorp-ChloramineOx_key.xlsx")) %>%
  ### convert columns to factors
  mutate(Biodegradation = as.factor(Biodegradation),
         Sorption = as.factor(Sorption),
         ChloramineOxidation = as.factor(ChloramineOxidation))

ca.df10 <- left_join(ca.df9, bsc.table)  # add biodegration, sorption, and Chloramine Oxidation categories to dataframe

## removed redundant rows in the data
### note: this is NOT removing data from the triplicate analysis, it is removing
###   samples that appear twice in the dataset
ca.df11 <- unique(ca.df10)

# save cleaned data
clean.path <- str_c(ef.dir, "clean/", "california-lab-results_clean.rds")
write.df <- ca.df11
write_rds(x=write.df, path=clean.path)
