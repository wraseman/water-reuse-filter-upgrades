# Purpose: analyze operational data (TSS and turbidity) that Albrey sent
# Output: TSS-turbidity.rds
# Author: William Raseman 

# Data: 
# TSS at Chlorine contact chamber (DEP monitoring point – daily grab)
# TSS at Chlorine contact chamber (TSS meter – daily average)
# Turbidity at Chlorine contact chamber (turbidity meter – daily average)
# Turbidity at Chlorine contact chamber (turbidity meter – daily grab)
# Clarifier 1 TSS 
# Clarifier 2 TSS 
# Clarifier 3 TSS 
# Clarifier 4 TSS 
# Filter Pump Station 1 TSS (mix of all 4 clarifiers before filters)

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization
library(readxl)  # import data from Excel
library(zoo)  # calculting rolling average

# user defined functions
## function for determining colors in ggplot2 style
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


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

# read in data
data.dir <- "./data/tss-turbidity/"
filename1 <- "TSS and Turb 10152019.xlsx"
path1 <- str_c(data.dir, filename1)
df1 <- read_excel(path = path1, skip = 1, 
                  col_types = c("date", rep("numeric", 9))) 

## rename data
new.colnames <- c("date",
                  "TSS_postfilt_daily_grab",
                  "TSS_postfilt_SCADA_daily_avg",
                  "turb_postfilt_SCADA_daily_avg",
                  "turb_postfilt_daily_grab",
                  "TSS_prefilt_clar1_daily_avg",
                  "TSS_prefilt_clar2_daily_avg",
                  "TSS_prefilt_clar3_daily_avg",
                  "TSS_prefilt_clar4_daily_avg",
                  "TSS_prefilt_combo_daily_avg")
colnames(df1) <- new.colnames


### add rolling averages for visualization
week <- 7
df2 <- mutate(df1, weekly_rollmean_TSS_effluent = rollmean(TSS_postfilt_SCADA_daily_avg, k = week, na.pad=TRUE, align="right"), 
              weekly_rollmean_turb_effluent = rollmean(turb_postfilt_SCADA_daily_avg, k = week, na.pad=TRUE, align="right")) %>%
                select(date, weekly_rollmean_TSS_effluent, weekly_rollmean_turb_effluent)

# perform t-test about percent removal and visualize

## delineate new and old filter dates. DBF online: "2018-03-06"
start.ph1 <- min(df2$date) %>% as.Date()
end.ph1 <- as.Date("2018-03-05")
start.ph2 <- as.Date("2018-03-06")
end.ph2 <- max(df2$date) %>% as.Date()
df2 <- mutate(df2, phase = if_else(date >= start.ph2, 2, 1) %>% as.factor)

## calculate average TSS and turbidity for Phases 1 and 2
df2.ph1 <- filter(df2, phase == 1)
means.ph1 <- df2.ph1 %>%
  summarize(mean_tss = mean(weekly_rollmean_TSS_effluent, na.rm = TRUE),
            mean_turb = mean(weekly_rollmean_turb_effluent, na.rm = TRUE))

df2.ph2 <- filter(df2, phase == 2)
means.ph2 <- df2.ph2 %>%
  summarize(mean_tss = mean(weekly_rollmean_TSS_effluent, na.rm = TRUE),
            mean_turb = mean(weekly_rollmean_turb_effluent, na.rm = TRUE))

### visualize TSS and turbidity
#### TSS
p1 <- ggplot(df2, aes(x=as.Date(date), y=weekly_rollmean_TSS_effluent)) +
  geom_point(aes(fill=phase), color="white", alpha = 0.7, shape = 21, size=2) +
  geom_segment(aes(x=start.ph1, y=means.ph1$mean_tss, xend=end.ph1, yend=means.ph1$mean_tss), color=gg_color_hue(n=2)[1], size=1.6) +
  geom_segment(aes(x=start.ph2, y=means.ph2$mean_tss, xend=end.ph2, yend=means.ph2$mean_tss), color=gg_color_hue(n=2)[2], size=1.6) +
  guides(fill = guide_legend(title = "")) +
  xlab("Date") +
  ylab("Filter Effluent TSS (mg/L)") +
  mytheme
tiff(filename = str_c(figure.dir, "t-test_means_tss_filteff", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p1)
dev.off()

#### turbidity
p2 <- ggplot(df2, aes(x=as.Date(date), y=weekly_rollmean_turb_effluent)) +
  geom_point(aes(fill=phase), color="white", alpha = 0.7, shape = 21, size=2) +
  geom_segment(aes(x=as.Date("2014-01-01"), y=means.ph1$mean_turb, xend=end.ph1, yend=means.ph1$mean_turb), color=gg_color_hue(n=2)[1], size=1.6) +
  geom_segment(aes(x=start.ph2, y=means.ph2$mean_turb, xend=end.ph2, yend=means.ph2$mean_turb), color=gg_color_hue(n=2)[2], size=1.6) +
  guides(fill = guide_legend(title = "")) +
  xlab("Date") +
  ylab("Filter Effluent Turbidity (ntu)") +
  xlim(as.Date("2014-01-01"), end.ph2) +
  mytheme
tiff(filename = str_c(figure.dir, "t-test_means_turb_filteff", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p2)
dev.off()

## result of t Test
tss.ttest <- t.test(x=df2.ph1$weekly_rollmean_TSS_effluent, y=df2.ph2$weekly_rollmean_TSS_effluent)$p.value
turb.ttest <- t.test(x=df2.ph1$weekly_rollmean_turb_effluent, y=df2.ph2$weekly_rollmean_turb_effluent)$p.value

library(ggpubr)
ggqqplot(df2.ph1$weekly_rollmean_TSS_effluent)
ggqqplot(df2.ph2$weekly_rollmean_TSS_effluent)
ggqqplot(df2.ph1$weekly_rollmean_turb_effluent)
ggqqplot(df2.ph2$weekly_rollmean_turb_effluent)

# write data 
write_rds(df1, str_c(data.dir, "TSS-turbidity.rds"))
write_rds(df2, str_c(data.dir, "TSS-turbidity_7day-avg.rds"))
