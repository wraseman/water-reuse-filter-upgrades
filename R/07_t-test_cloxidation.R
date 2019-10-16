# Purpose: perform t-tests for chlorine oxidation
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # import data from Excel

# read in percent removal data for TBF and SMF
data.dir <- "./data/"
filename <- "pct-rmv.rds"
data.path <- str_c(data.dir, filename)
all.df <- read_rds(path = data.path) 

# visualize boxplots of percent removal for TBF and SMF
filter.names <- c("Traveling Bed Filter", "Synthetic Media Filter", "Deep Bed Filter")
pctrmv.string <- c("PctRmvTBFilt", "PctRmvSMFilt", "PctRmvDBFilt")

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

samplesize <- function(y) 
  c(label=length(y), y=median(y))

# investigate percent removal for chlorination
pctrmvchlor.string <- c("PctRmvChlor1", "PctRmvChlor2")

filt.cec <- filter(all.df, !(Parameter %in% c("Giardia", "Cryptosporidium", "Turbidity"))) %>% 
  ungroup

filt.cec$ClOxidation <- factor(filt.cec$ClOxidation, levels = 
                                c("Poor", "Moderate", "Good"))

pctrmv.cl <- select(filt.cec, PctRmvChlor1, PctRmvChlor2, ClOxidation) %>%
  gather(key = "Phase", value = "PercentRemoval", -ClOxidation) %>%
  mutate(Phase = ifelse(Phase == "PctRmvChlor1", "1",
                        ifelse(Phase == "PctRmvChlor2", "2", NA)))

## one-sample t Test where null hypothesis is the distribution has mean zero

### Is Phase 1 or Phase 2 removal due to chlorination significantly different than zero?
mu.hyp <- 0
phase1.pctrmv <- filter(pctrmv.cl, Phase == 1)$PercentRemoval %>% na.omit %>% unlist
onesamp.ttest.phase1 <- t.test(x=phase1.pctrmv, mu=mu.hyp)
onesamp.phase1.p <- onesamp.ttest.phase1$p.value
# p = 0.24

phase2.pctrmv <- filter(pctrmv.cl, Phase == 2)$PercentRemoval %>% na.omit %>% unlist
onesamp.ttest.phase2 <- t.test(x=phase2.pctrmv, mu=mu.hyp)
onesamp.phase2.p <- onesamp.ttest.phase2$p.value
# p = 0.01

### For Phase 1, are different chlorination oxidation categories significantly different than zero?
#### Phase 1 - "Poor", "Moderate", "Good"
poor.ph1.pctrmv <- filter(pctrmv.cl, Phase == 1, ClOxidation == "Poor")$PercentRemoval %>% na.omit %>% 
  unlist
onesamp.p.poor.ph1 <- t.test(x=poor.ph1.pctrmv, mu=mu.hyp)$p.value
  
mod.ph1.pctrmv <- filter(pctrmv.cl, Phase == 1, ClOxidation == "Moderate")$PercentRemoval %>% na.omit %>% 
  unlist
onesamp.p.mod.ph1 <- t.test(x=mod.ph1.pctrmv, mu=mu.hyp)$p.value  

good.ph1.pctrmv <- filter(pctrmv.cl, Phase == 1, ClOxidation == "Good")$PercentRemoval %>% na.omit %>% 
  unlist
onesamp.p.good.ph1 <- t.test(x=good.ph1.pctrmv, mu=mu.hyp)$p.value  

#### Phase 2 - "Poor", "Moderate", "Good"
poor.ph2.pctrmv <- filter(pctrmv.cl, Phase == 2, ClOxidation == "Poor")$PercentRemoval %>% na.omit %>% 
  unlist
onesamp.p.poor.ph2 <- t.test(x=poor.ph2.pctrmv, mu=mu.hyp)$p.value

mod.ph2.pctrmv <- filter(pctrmv.cl, Phase == 2, ClOxidation == "Moderate")$PercentRemoval %>% na.omit %>% 
  unlist
onesamp.p.mod.ph2 <- t.test(x=mod.ph2.pctrmv, mu=mu.hyp)$p.value  

good.ph2.pctrmv <- filter(pctrmv.cl, Phase == 2, ClOxidation == "Good")$PercentRemoval %>% na.omit %>% 
  unlist
onesamp.p.good.ph2 <- t.test(x=good.ph2.pctrmv, mu=mu.hyp)$p.value  


# visualize phase 1 vs. phase 2 chlorine oxidation
## unbounded
p1 <- ggplot(data = filter(pctrmv.cl), aes(x=Phase, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.5, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme
tiff(filename = str_c(figure.dir, "ttest_clox1", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p1)
dev.off()

## bounded version
p2 <- ggplot(data = filter(pctrmv.cl), aes(x=Phase, y=PercentRemoval)) +
  geom_boxplot() +
  ylab("Percent Removal") +
  ylim(-50, 100) +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  # stat_summary(fun.data=medianboxplt, geom="text", vjust=-0.5, col="black", size=6) +  # source: https://stackoverflow.com/questions/31138970/plot-number-of-data-points-in-r
  mytheme
tiff(filename = str_c(figure.dir, "ttest_clox2", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p2)
dev.off()

## plot by chlorine oxidation category
p3 <- ggplot(data = pctrmv.cl,
             aes(x=ClOxidation, y=PercentRemoval, fill=ClOxidation)) +
  geom_boxplot(alpha= 0.7) +
  facet_wrap(Phase ~ .) +
  ylab("Percent Removal") +
  xlab("Chlorine Oxidation") +
  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
  mytheme
tiff(filename = str_c(figure.dir, "ttest_clox3", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p3)
dev.off()
