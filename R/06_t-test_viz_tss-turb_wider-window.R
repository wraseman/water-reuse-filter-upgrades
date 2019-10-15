# Purpose: investigate impact of new filters on chlorine demand
# Author: William Raseman

# clear environment
rm(list = ls())

# set working directory and turn off warnings for Knitting
setwd("C:/Users/wraseman/Hazen and Sawyer/Stanford, Benjamin - Loxahatchee DBF Evaluation/Data Analysis - Billy")
options(warn=-1)

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # import data from Excel

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

# get date ranges for Phase 1 (SMF and TBF) and Phase 2 (DBF)
data.dir <- "./data/"
filename1 <- "LRD all filters raw data.xlsx"
path1 <- str_c(data.dir, filename1)
df1 <- read_excel(path = path1, sheet = "Data") %>% 
  select(Phase, DateTimeCollected) %>%
  na.omit()

# phase1.range <- filter(df1, Phase == "Phase 1")$DateTimeCollected %>%
#   range() %>% as.Date()
phase1.range <- c(as.Date("2015-01-01"), as.Date("2017-01-01"))
phase2.range <- filter(df1, Phase == "Phase 2")$DateTimeCollected %>%
  range() %>% as.Date()

# read in chlorine data from Biowin dataset
biowin.dir <- str_c(data.dir, "Biowin Study Data/")
filename2 <- "3 Calibration Data and Plots - wjr edits.xlsx"
path2 <- str_c(biowin.dir, filename2)
df2 <- read_excel(path = path2, sheet = "Data", range = "A3:IV1402")

df2.cols <- c("Date",
              "Raw SS",
              "TSS in",
              "After Filter TSS Grab",
              "After Filter TSS Grab Meter Rdng",
              "TSS - SCADA",	
              "TSS 15 MAX - SCADA",
              "TSS 15 MIN - SCADA",
              "TURBIDITY - SCADA",
              "TURBIDITY 15 MAX - SCADA",
              "TURBIDITY 15 MIN - SCADA")

df2 <- select(df2, df2.cols)  # subset the data to look at just TSS and turbidity

new.colnames <- c("date", 
                  "raw_SS_mgperL",
                  "inf_TSS_unknown_units",
                  "postfilt_TSS_grab_mgperL",
                  "postfilt_TSS_grab_meter_reading_mgperL",
                  "postfilt_TSS_SCADA_mgperL",
                  "postfilt_TSS_SCADA_15max_mgperL",
                  "postfilt_TSS_SCADA_15min_mgperL",
                  "postfilt_turb_SCADA_ntu",
                  "postfilt_turb_SCADA_15max_ntu",
                  "postfilt_turb_SCADA_15min_ntu")

colnames(df2) <- new.colnames

df2 <- mutate(df2, date = as.Date(date),
              phase = if_else(between(date, min(phase1.range), max(phase1.range)), "Phase 1", 
                              if_else(between(date, min(phase2.range), max(phase2.range)), "Phase 2", NA_character_)))


# visualize TSS and turbidity
## calculate average TSS and turbidity for Phases 1 and 2
df.phase1 <- filter(df2, phase == "Phase 1")
means.phase1 <- df.phase1 %>%
  summarize(mean_tss = mean(postfilt_TSS_SCADA_mgperL),
            mean_turb = mean(postfilt_turb_SCADA_ntu))

df.phase2 <- filter(df2, phase == "Phase 2") 
means.phase2 <- df.phase2 %>%
  na.omit() %>%  
  summarize(mean_tss = mean(postfilt_TSS_SCADA_mgperL),
            mean_turb = mean(postfilt_turb_SCADA_ntu))


## function for determining colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

p.inftss <- ggplot(data = df2, aes(x=date, y=raw_SS_mgperL)) +
  geom_point(aes(fill=phase), color="white", alpha = 0.7, shape = 21, size=2) +
  ylab("Raw TSS (mg/L)") +
  xlab("Date") + 
  guides(fill = guide_legend(title = "")) +
  mytheme
tiff(filename = str_c(figure.dir, "raw_TSS", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p.inftss)
dev.off()

p.tss <- ggplot(data = df2, aes(x=date, y=postfilt_TSS_SCADA_mgperL)) +
  geom_point(aes(fill=phase), fill="black", color="white", alpha = 0.7, shape = 21, size=2) +
  ylab("Post-filter TSS (mg/L)") +
  xlab("Date") + 
  # geom_segment(aes(x=min(phase1.range), y=means.phase1$mean_tss, xend=max(phase1.range), yend=means.phase1$mean_tss), color=gg_color_hue(n=2)[1], size=1.6) +
  # geom_segment(aes(x=min(phase2.range), y=means.phase2$mean_tss, xend=max(df2$date), yend=means.phase2$mean_tss), color=gg_color_hue(n=2)[2], size=1.6) +
  guides(fill = guide_legend(title = "")) +
  mytheme
tiff(filename = str_c(figure.dir, "post-filt_TSS_nocolor", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p.tss)
dev.off()

p.tss.bounded <- p.tss + ylim(0,4.5)
tiff(filename = str_c(figure.dir, "post-filt_TSS_nocolor_bounded", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p.tss.bounded)
dev.off()

p.tss1 <- ggplot(data = df2, aes(x=date, y=postfilt_TSS_SCADA_mgperL)) +
  geom_point(aes(fill=phase), color="white", alpha = 0.7, shape = 21, size=2) +
  ylab("Post-filter TSS (mg/L)") +
  xlab("Date") + 
  geom_segment(aes(x=min(phase1.range), y=means.phase1$mean_tss, xend=max(phase1.range), yend=means.phase1$mean_tss), color=gg_color_hue(n=2)[1], size=1.6) +
  geom_segment(aes(x=min(phase2.range), y=means.phase2$mean_tss, xend=max(df2$date), yend=means.phase2$mean_tss), color=gg_color_hue(n=2)[2], size=1.6) +
  guides(fill = guide_legend(title = "")) +
  mytheme
tiff(filename = str_c(figure.dir, "post-filt_TSS", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p.tss1)
dev.off()

p.tss2 <- p.tss1 + ylim(0, 4.5)
tiff(filename = str_c(figure.dir, "post-filt_TSS_bounded", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p.tss2)
dev.off()


p.turb1 <- ggplot(data = df2, aes(x=date, y=postfilt_turb_SCADA_ntu)) +
  geom_point(aes(fill=phase), color="white", alpha = 0.7, shape = 21, size=2) +
  ylab("Post-filter Turbidity (mg/L)") +
  xlab("Date") + 
  geom_segment(aes(x=min(phase1.range), y=means.phase1$mean_turb, xend=max(phase1.range), yend=means.phase1$mean_turb), color=gg_color_hue(n=2)[1], size=1.6) +
  geom_segment(aes(x=min(phase2.range), y=means.phase2$mean_turb, xend=max(df2$date), yend=means.phase2$mean_turb), color=gg_color_hue(n=2)[2], size=1.6) +
  # ylim(0, 5.7) +
  guides(fill = guide_legend(title = "")) +
  mytheme
tiff(filename = str_c(figure.dir, "post-filt_turb", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p.turb1)
dev.off()

p.turb2 <- p.turb1 + ylim(0, 5.7)
tiff(filename = str_c(figure.dir, "post-filt_turb_bounded", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p.turb2)
dev.off()


# TSS and turbidity two-sample t-Test
tss.ttest <- t.test(x=df.phase1$postfilt_TSS_SCADA_mgperL, y=df.phase2$postfilt_TSS_SCADA_mgperL)
turb.ttest <- t.test(x=df.phase1$postfilt_turb_SCADA_ntu, y=df.phase2$postfilt_turb_SCADA_ntu)


