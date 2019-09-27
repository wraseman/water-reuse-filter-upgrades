# Purpose: calculate percent removal for filters and disinfection
# Output: ./data/pct-rmv_dbf.rds
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization

# read in cleaned data for all filters
## dataset contains information on pathogen, contaminants of emerging concern (CEC), 
##  turbidity, and chlorine concentrations and flow rates
data.dir <- "./data/"
filename <- "cleaned_all-filters.rds"
data.path <- str_c(data.dir, filename)
all.df <- read_rds(path = data.path) 

# create dataframe for TBF and SMF analysis (i.e., Phase 1)
tsf.df <- filter(all.df, Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1"), 
                 Parameter != "Flow") %>%
select(-DateTimeCollected, -Detect)

## get mean value for all triplicates
## source: https://stackoverflow.com/questions/31215795/removing-duplicate-rows-and-calculate-the-average-in-a-dataframe-in-r
tsf.df <- group_by(tsf.df, DateCollected, Location, Parameter) %>%
  mutate_each(list(mean), -(c(Units, Biodeg, Sorption, ClOxidation))) %>%
  distinct

## spread the data to wide format to calculate percent removals
tsf.wide <- spread(tsf.df, Location, Value)
colnames(tsf.wide)[8:11] <- c("ChlorEffluent1", "SMFiltEffluent", "TBFiltEffluent", "TBFiltInfluent")

tsf.wide <- select(tsf.wide, DateCollected, Parameter, TBFiltInfluent,
                   TBFiltEffluent, SMFiltEffluent, ChlorEffluent1,
                   Biodeg, Sorption, ClOxidation, Units, MRL)

## add whether or not a parameter was detected
tsf.wide <- mutate(tsf.wide, ChlorEff1Detect = ifelse(MRL < ChlorEffluent1, TRUE, FALSE),
                   SMFiltEffDetect = ifelse(MRL < SMFiltEffluent, TRUE, FALSE), 
                   TBFiltEffDetect = ifelse(MRL < TBFiltEffluent, TRUE, FALSE),
                   TBFiltInfDetect = ifelse(MRL < TBFiltInfluent, TRUE, FALSE))

# tsf.wide <- full_join(tsf1.wide, tsf2.wide) 
# create column for chlorination influent
flw.tbf <- 1.71  # based on average flow 1.81 and 1.60 MGD from "Pages from LRD SCADA SCREENS.pdf" 
flw.smf <- 6.25  # based on average flow 6.34 and 6.15 MGD from "Pages from LRD SCADA SCREENS.pdf"
flw.total <- flw.tbf + flw.smf
flwfrc.tbf <- flw.tbf/flw.total  # flow fraction sent to traveling bed filters
flwfrc.smf <- flw.smf/flw.total  # flow fraction sent to synthetic media filters
tsf.wide <- mutate(tsf.wide,
                   ChlorInfluent1 = flwfrc.tbf*TBFiltEffluent + flwfrc.smf*SMFiltEffluent)

# calculate % removal due to filtration and chlorination
## note: assume that TBF influent concentration is the same as SMF influent
tsf.wide <- mutate(tsf.wide, 
                   PctRmvSMFilt = (TBFiltInfluent - SMFiltEffluent) / TBFiltInfluent * 100)
tsf.wide <- mutate(tsf.wide, 
                   PctRmvTBFilt = (TBFiltInfluent - TBFiltEffluent) / TBFiltInfluent * 100)
tsf.wide <- mutate(tsf.wide, 
                   PctRmvChlor1 = (ChlorInfluent1 - ChlorEffluent1) / ChlorInfluent1 * 100)
tsf.wide <- mutate(tsf.wide, 
                   PctRmvTotal1 = (TBFiltInfluent - ChlorEffluent1) / TBFiltInfluent * 100)

# create dataframe for deep bed filter (DBF) analysis (i.e., Phase 2)
dbf.df <- filter(all.df, Location %in% c("DBF-E", "DBF-I", "Final Ph2"), 
                 Parameter != "Flow") %>%
  select(-DateTimeCollected, -Detect)

## match units for Giardia and Cryptosporidium for all locations.
##  some measures are oocysts (or cysts) per Liter and others per 100 Liters
dbf.units <- mutate(dbf.df, 
                    Value = ifelse(Units == "oocysts/L", 
                                   Value*100,
                                   Value), 
                    Units = ifelse(Units == "oocysts/L",
                                   "oocysts/100L", 
                                   Units)) %>%
  mutate(Value = ifelse(Units == "cysts/L", 
                        Value*100, 
                        Value), 
         Units = ifelse(Units == "cysts/L", 
                        "cysts/100L",
                        Units)
  )

## get mean value for all triplicates
## source: https://stackoverflow.com/questions/31215795/removing-duplicate-rows-and-calculate-the-average-in-a-dataframe-in-r
dbf.units <- group_by(dbf.units, DateCollected, Location, Parameter) %>%
  mutate_each(list(mean), -(c(Units, Biodeg, Sorption, ClOxidation))) %>%
  distinct

## spread the data to wide format to calculate percent removals
dbf.wide <- spread(dbf.units, Location, Value)
colnames(dbf.wide)[8:10] <- c("DBFiltEffluent", "DBFiltInfluent", "ChlorEffluent2")
dbf.wide <- select(dbf.wide, DateCollected, Parameter, DBFiltInfluent, 
                   DBFiltEffluent, ChlorEffluent2,
                   Biodeg, Sorption, ClOxidation, Units, MRL)

## correct for issue with inconsistent MRL for Pathogen tests
## source: https://stackoverflow.com/questions/40820120/merging-two-rows-with-some-having-missing-values-in-r
dbf.wide <- dbf.wide %>% group_by(DateCollected, Parameter)  %>% 
  summarise_each(funs(max(., na.rm = TRUE))) 

## source: https://stackoverflow.com/questions/30990961/replace-inf-nan-and-na-values-with-zero-in-a-dataset-in-r
is.na(dbf.wide)<-sapply(dbf.wide, is.infinite)
dbf.wide[is.na(dbf.wide)] <- NA

## add whether or not a parameter was detected
dbf.wide <- mutate(dbf.wide, ChlorEff2Detect = ifelse(MRL < ChlorEffluent2, TRUE, FALSE),
                   DBFiltEffDetect = ifelse(MRL < DBFiltEffluent, TRUE, FALSE), 
                   DBFiltInfDetect = ifelse(MRL < DBFiltInfluent, TRUE, FALSE))


# calculate % removal due to filtration and chlorination
dbf.wide <- mutate(dbf.wide, 
                   PctRmvDBFilt = (DBFiltInfluent - DBFiltEffluent) / DBFiltInfluent * 100)
dbf.wide <- mutate(dbf.wide, 
                   PctRmvChlor2 = (DBFiltEffluent - ChlorEffluent2) / DBFiltEffluent * 100)
dbf.wide <- mutate(dbf.wide, 
                   PctRmvTotal2 = (DBFiltInfluent - ChlorEffluent2) / DBFiltInfluent * 100)

# combine both dataframes 
all.wide <- bind_rows(tsf.wide, dbf.wide)



# save cleaned, combined dataframe
data.path <- str_c(data.dir, "pct-rmv.rds")
write_rds(x=all.wide, path=data.path)



