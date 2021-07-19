# Purpose: read in and clean data from South Bend laboratory results
#   Data includes TrOCs and pathogens (Giardia and Cryptosporidium)
# Output: ./data/eurofins-data/clean/southbend-lab-results_clean.rds (tidy, cleaned dataframe)
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # read Excel spreadsheets

# read in data from lab samples tested in Eurofin's South Bend lab
ef.dir <- "./data/eurofins-data/"
sb.dir <- str_c(ef.dir, "southbend-lab-results/")
config.dir <- "./config/"

## read in all South Bend spreadsheets at once
## source: https://rpubs.com/LMunyan/363306
file.array <- list.files(path=sb.dir)  # create an array of the files from your target directory
excel.array <- str_subset(file.array, "xlsx") %>%  # only include Excel spreadsheets
  str_subset("Sample_ID_key.xlsx", negate=TRUE)  # don't keep Sample ID key file

sb.df1 <- data.frame()  # empty dataframe for California data

# read in data
for (i in 1:length(excel.array)){
  temp.df <- read_excel(str_c(sb.dir, excel.array[i]), range = cell_cols("A:P"))
  sb.df1 <- rbind(sb.df1, temp.df)  # for each iteration, bind the new data to the building dataset
}

# clean and reformat data
## select only the columns that are needed and rename those columns for consistency with California lab analysis
selected.cols <- c("EEA ID#", "Client ID", "Sample Type", "Collected",
                   "Analyte", "Result Flag", "Result",
                   "Units", "MRL")
new.colnames <- c("SampleID", "ClientID", "SampleType", "SampleDateTime",
                  "Analyte", "ResultFlag", "Result",
                  "Units", "DetectionLimit")
sb.df2 <- select(sb.df1, selected.cols)
colnames(sb.df2) <- new.colnames

## filter out quality control data--just keep "Field Samples"
sb.df2 <- sb.df2 %>%
  ## filter out quality control data
  filter(SampleType == "Field Sample") %>%
  select(-SampleType)  # drop "SampleType" column

## create non-detect column and set Result to "NA" if there is a non-detect for consistency with California analysis
sb.df3 <- sb.df2 %>%
  mutate(Detect = if_else(ResultFlag == "=", TRUE, FALSE)) %>%  # ResultFlag of "=" means detect, "<" means non-detect
  mutate(Result = if_else(Detect == FALSE, NA_real_, Result)) %>%  # If non-detect, set Result to "NA"
  select(-ResultFlag)  # drop ResultFlag column

## get location and unit process type based on the ClientID
sb.df4 <- sb.df3 %>%
  mutate(LocationProcessType = str_sub(ClientID, 1, 5)) %>%
  ### specify whether it is a process influent or effluent and the process type
  mutate(ProcessInfEff =
           if_else(str_sub(LocationProcessType, -2, -1) == "-I", "Influent",  # check last two characters to determine if influent data
                   if_else(str_sub(LocationProcessType, -2, -1) == "-E", "Effluent",  # check last two characters to determine if effluent data
                           if_else(LocationProcessType == "Final", "Effluent",
                                   NA_character_)))) %>%
  ### determine the process type: filter (TBF, SMF, DBF), clearwell, or laboratory blank (technically not a process)
  mutate(Process = if_else(str_sub(LocationProcessType, 1, 3) == "TBF", "TBF",  # check first three characters to get unit process
                           if_else(str_sub(LocationProcessType, 1, 3) == "SMF", "SMF",
                                   if_else(str_sub(LocationProcessType, 1, 3) == "DBF", "DBF",
                                           if_else(LocationProcessType=="Final", "clearwell",
                                                   NA_character_)))))

## in the plant, the stream is bifurcated between TBF and SMF. This stream is
##   called "TBF-I" but it is actually the infuent for both TBF and SMF.
##   For this reason, duplicate all TBF-I to specify Process = "SMF" and ProcessInfEff = "Influent"
smf.inf <- filter(sb.df4, Process == "TBF", ProcessInfEff == "Influent") %>%
  mutate(Process = "SMF")

## specify in the dataset that TBF-I is the influent to both TBF and SMF
sb.df5 <- bind_rows(sb.df4, smf.inf)

## no triplicate samples were performed at the South Bend (unlike California);
##  however, we need to add "Duplicate" and "Triplicate" columns in this dataframe
##  to match the structure of the California dataframe
sb.df6 <- sb.df5 %>%
  mutate(Duplicate = FALSE,
         Triplicate = FALSE)

## account for dilution factor and inconsistency in units in data
### multiply all "/100L" samples by 100 and convert to units of "/L"
sb.df7 <- sb.df6 %>%
  mutate(Result = if_else(str_sub(Units, -5, -1) == "/100L", Result/100, Result)) %>%  # convert Result from /100L units to /L
  mutate(DetectionLimit = if_else(str_sub(Units, -5, -1) == "/100L", DetectionLimit/100, DetectionLimit)) %>%  # convert Dilution from /100L units to /L
  mutate(Units = if_else(str_sub(Units, -5, -1) == "/100L", str_replace(Units, "/100L", "/L"), Units))  # convert units from /100L to /L

### make Process influent and effluent factors so when plotted as boxplot or barplot, influent comes before effluent
sb.df7$ProcessInfEff <- factor(sb.df7$ProcessInfEff,
                               levels = c("Influent", "Effluent"))

# filter data for compounds of interest
# keep columns and analytes necessary for analysis
analytes <- c("Cryptosporidium", "Giardia",  # pathogens
               "Meprobamate", "Dilantin", "Sulfamethoxazole",
               "TCEP", "Carbamazepine",
               "Gemfibrozil", "Ibuprofen", "Acetaminophen",
               "Triclosan", "Caffeine", "Fluoxetine")
# analytes <- c("Cryptosporidium", "Giardia",  # pathogens
#                "Meprobamate", "Dilantin", "Sulfamethoxazole",  # CECs (all remaining strings)
#                "BPA", "TCEP", "TCPP",
#                "Carbamazepine", "Salicylic Acid",
#                "Gemfibrozil", "Ibuprofen", "Acetaminophen",
#                "Triclosan", "Caffeine", "Fluoxetine")
sb.df8 <- filter(sb.df7, Analyte %in% analytes)

## assign biodegradation, sorption, and chloramine oxidation categories for each analyte
bsc.table <- read_excel(str_c(config.dir, "Biodeg-Sorp-ChloramineOx_key.xlsx")) %>%
  ### convert columns to factors
  mutate(Biodegradation = as.factor(Biodegradation),
         Sorption = as.factor(Sorption),
         ChloramineOxidation = as.factor(ChloramineOxidation))

sb.df9 <- left_join(sb.df8, bsc.table)  # add biodegration, sorption, and Chloramine Oxidation categories to dataframe

# save cleaned data
clean.path <- str_c(ef.dir, "clean/", "southbend-lab-results_clean.rds")
write.df <- sb.df9
write_rds(x=write.df, file=clean.path)
