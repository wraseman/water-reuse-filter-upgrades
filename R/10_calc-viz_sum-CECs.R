# Purpose: calculate and visualize the sum of effluent CECs for each day
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization

# read in percent removal data for TBF and SMF
data.dir <- "./data/"
filename <- "pct-rmv.rds"
data.path <- str_c(data.dir, filename)
all.df <- read_rds(path = data.path) 


## plot settings
figure.dir <- "./figures/"
fig.resolution <- 300
# mytheme = theme(
#   axis.title.x = element_text(size = 16),
#   axis.text.x = element_text(size = 16),
#   axis.title.y = element_text(size = 16),
#   axis.text.y = element_text(size = 16),
#   title = element_text(size = 18),
#   legend.text = element_text(size = 16))

analytes <- c("Cryptosporidium", "Giardia", "Turbidity")
analytes.df <- filter(all.df, !(Parameter %in% analytes)) %>% ungroup %>%
  mutate(Parameter = str_trunc(Parameter, 15))

# visualize the sum of all CECs in the clearwell effluent
## first visualize all CECs as a timeseries
chloreff.df <- select(analytes.df, DateCollected, Parameter, Units, MRL, ChlorEffluent1, ChlorEffluent2) %>%  # get just the chlorinated effluent data
  gather(key = Phase, value = ChlorEffluent, -DateCollected, -Parameter, -Units, -MRL) %>%  # arrange in a way that is easier to plot by phase
  mutate(Phase = str_sub(Phase, -1)) %>%
  na.omit  # remove NAs

ggplot(chloreff.df, aes(x=DateCollected, y=ChlorEffluent)) +
  geom_point(aes(color=Phase)) +
  ylab("Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")

ggplot(chloreff.df, aes(x=DateCollected, y=ChlorEffluent)) +
  geom_point(aes(color=Phase)) +
  ylab("Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent") +
  facet_wrap(Phase ~ ., scales = "free_x")

## calculate the sum and mean of all CECs per day and calculate how many 

eff_by_date <- group_by(chloreff.df, DateCollected, Phase) %>%
  summarize(SumCEC = sum(ChlorEffluent, na.rm = TRUE), MeanCEC = mean(ChlorEffluent, na.rm = TRUE), 
            CountCEC = n())

ggplot(eff_by_date, aes(x=DateCollected, y=SumCEC)) +
  geom_point(aes(color=Phase)) +
  ylab("Sum Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")

ggplot(eff_by_date, aes(x=DateCollected, y=MeanCEC)) +
  geom_point(aes(color=Phase)) +
  ylab("Mean Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")

ggplot(eff_by_date, aes(x=DateCollected, y=CountCEC)) +
  geom_bar(aes(color=Phase), stat="identity") +
  ylab("Sample Count") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")

# do same analysis but only for analytes in common for both Phase 1 and 2
analytes2 <- c("Acetaminophen", "Caffeine",
               "Carbamazepine", "Dilantin",
               "Gemfibrozil", "Ibuprofen",
               "Meprobamate", "Salicylic Acid",
               "Sulfamethoxazole", "Triclosan")
analytes.df2 <- filter(all.df, Parameter %in% analytes2) %>% ungroup %>%
  mutate(Parameter = str_trunc(Parameter, 15))

# visualize the sum of all CECs in the clearwell effluent
## first visualize all CECs as a timeseries
chloreff.df2 <- select(analytes.df2, DateCollected, Parameter, Units, MRL, ChlorEffluent1, ChlorEffluent2) %>%  # get just the chlorinated effluent data
  gather(key = Phase, value = ChlorEffluent, -DateCollected, -Parameter, -Units, -MRL) %>%  # arrange in a way that is easier to plot by phase
  mutate(Phase = str_sub(Phase, -1)) %>%
  na.omit  # remove NAs

ggplot(chloreff.df2, aes(x=DateCollected, y=ChlorEffluent)) +
  geom_point(aes(color=Phase)) +
  ylab("Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")

ggplot(chloreff.df2, aes(x=DateCollected, y=ChlorEffluent)) +
  geom_point(aes(color=Phase)) +
  ylab("Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent") +
  facet_wrap(Phase ~ ., scales = "free_x")

## calculate the sum and mean of all CECs per day and calculate how many 

eff_by_date2 <- group_by(chloreff.df2, DateCollected, Phase) %>%
  summarize(SumCEC = sum(ChlorEffluent, na.rm = TRUE), MeanCEC = mean(ChlorEffluent, na.rm = TRUE), 
            CountCEC = n())

ggplot(eff_by_date2, aes(x=DateCollected, y=SumCEC)) +
  geom_point(aes(color=Phase)) +
  ylab("Daily Sum Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")

ggplot(eff_by_date2, aes(x=DateCollected, y=MeanCEC)) +
  geom_point(aes(color=Phase)) +
  ylab("Mean Concentration (ug/L)") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")

ggplot(eff_by_date2, aes(x=DateCollected, y=CountCEC)) +
  geom_bar(aes(color=Phase), stat="identity") +
  ylab("Sample Count") +
  xlab("Date") +
  ggtitle("CEC Samples at Chlorinated Effluent")
