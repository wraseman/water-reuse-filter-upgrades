# Purpose: visualize percent removal for each filter and disinfection point
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
mytheme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  title = element_text(size = 18),
  legend.text = element_text(size = 16))

medianboxplt <- function(y) 
  c(label=median(y) %>% round(1), y=median(y))

# visualize boxplots of percent removal for all filter types
filter.names <- c("Traveling Bed Filter", "Synthetic Media Filter", "Deep Bed Filter")
pctrmv.string <- c("PctRmvTBFilt", "PctRmvSMFilt", "PctRmvDBFilt")

## Contaminants of emerging concern (CEC)
filt.cec <- filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))) %>% 
  ungroup

### Filter removal 
pctrmv.df <- ungroup(filt.cec) %>%
  select(PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt) %>%
  gather(key = "FilterType", value = "PercentRemoval") %>%
  mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
                             ifelse(FilterType == "PctRmvSMFilt", "SMF", 
                                    ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))

#### specify order of filter types for boxplots
pctrmv.df$FilterType <- factor(pctrmv.df$FilterType,
                       levels = c("SMF", "TBF", "DBF"), ordered = TRUE)


p1 <- ggplot(data = filter(pctrmv.df), aes(x=FilterType, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Filter Type") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme 
tiff(filename = str_c(figure.dir, "CECs_pctrmv", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p1)
dev.off()

### Disinfection removal 
pctrmv.cl <- select(filt.cec, PctRmvChlor1, PctRmvChlor2) %>%
  gather(key = "Phase", value = "PercentRemoval") %>%
  mutate(Phase = ifelse(Phase == "PctRmvChlor1", "1", 
                             ifelse(Phase == "PctRmvChlor2", "2", NA)))

p2 <- ggplot(data = filter(pctrmv.cl), aes(x=Phase, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.5, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme 
tiff(filename = str_c(figure.dir, "CECs_pctrmv_cl", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p2)
dev.off()

### Total removal 
pctrmv.tot <- select(filt.cec, PctRmvTotal1, PctRmvTotal2) %>%
  gather(key = "Phase", value = "PercentRemoval") %>%
  mutate(Phase = ifelse(Phase == "PctRmvTotal1", "1", 
                        ifelse(Phase == "PctRmvTotal2", "2", NA)))

p3 <- ggplot(data = filter(pctrmv.tot), aes(x=Phase, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.5, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme 
tiff(filename = str_c(figure.dir, "CECs_pctrmv_total", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p3)
dev.off()

## Pathogens and turbidity
filt.cryp <- filter(all.df, Parameter == "Cryptosporidium") %>% ungroup()
filt.giar <- filter(all.df, Parameter == "Giardia") %>% ungroup()
filt.turb <- filter(all.df, Parameter == "Turbidity") %>% ungroup()
  
### Filter removal
#### Cryptosporidium
pctrmv.cryp <- select(filt.cryp, PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt) %>%
  gather(key = "FilterType", value = "PercentRemoval") %>%
  mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
                             ifelse(FilterType == "PctRmvSMFilt", "SMF", 
                                    ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))

#### specify order of filter types for boxplots
pctrmv.cryp$FilterType <- factor(pctrmv.cryp$FilterType,
                               levels = c("SMF", "TBF", "DBF"), ordered = TRUE)



p4 <- ggplot(data = filter(pctrmv.cryp), aes(x=FilterType, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Filter Type") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme +
  ggtitle("Cryptosporidium")
tiff(filename = str_c(figure.dir, "crypto_filter", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p4)
dev.off()

#### Giardia
pctrmv.giar <- select(filt.giar, PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt) %>%
  gather(key = "FilterType", value = "PercentRemoval") %>%
  mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
                             ifelse(FilterType == "PctRmvSMFilt", "SMF", 
                                    ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))

#### specify order of filter types for boxplots
pctrmv.giar$FilterType <- factor(pctrmv.giar$FilterType,
                                 levels = c("SMF", "TBF", "DBF"), ordered = TRUE)

p5 <- ggplot(data = filter(pctrmv.giar), aes(x=FilterType, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Filter Type") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme +
  ggtitle("Giardia")
tiff(filename = str_c(figure.dir, "giar_filter", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p5)
dev.off()

#### Turbidity
pctrmv.turb <- select(filt.turb, PctRmvTBFilt, PctRmvSMFilt, PctRmvDBFilt) %>%
  gather(key = "FilterType", value = "PercentRemoval") %>%
  mutate(FilterType = ifelse(FilterType == "PctRmvTBFilt", "TBF", 
                             ifelse(FilterType == "PctRmvSMFilt", "SMF", 
                                    ifelse(FilterType == "PctRmvDBFilt", "DBF", NA))))

#### specify order of filter types for boxplots
pctrmv.turb$FilterType <- factor(pctrmv.turb$FilterType,
                                 levels = c("SMF", "TBF", "DBF"), ordered = TRUE)

p6 <- ggplot(data = filter(pctrmv.turb), aes(x=FilterType, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  xlab("Filter Type") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.25, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme +
  ggtitle("Turbidity")
tiff(filename = str_c(figure.dir, "turb_filter", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p6)
dev.off()


