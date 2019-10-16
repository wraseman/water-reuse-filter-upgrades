# Purpose: analyze operational data (TSS and turbidity) that Albrey sent
# Author: William Raseman 

# Note from Albrey: 
# Ben, 
# We discussed TSS data regarding the operational robustness of the filters. The attached spreadsheet contains daily TSS and Turbidity data at the following locations: 
#   
#   TSS at Chlorine contact chamber (DEP monitoring point – daily grab)
# TSS at Chlorine contact chamber (TSS meter – daily average)
# Turbidity at Chlorine contact chamber (turbidity meter – daily average)
# Turbidity at Chlorine contact chamber (turbidity meter – daily grab)
# Clarifier 1 TSS 
# Clarifier 2 TSS 
# Clarifier 3 TSS 
# Clarifier 4 TSS 
# Filter Pump Station 1 TSS (mix of all 4 clarifiers before filters)
# 
# Albrey 

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
data.dir <- "./data/"
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


# visualize TSS and turbidity data
transparency = 0.15

## TSS - combined prefilter
ggplot(filter(df1, date >= as.Date("2019-04-17")), aes(x=date, y=TSS_prefilt_combo_daily_avg)) +
  geom_point(aes(alpha = transparency)) +
  mytheme  
### this dataset is very short!

## TSS - SCADA (postfilt)
ggplot(df1, aes(x=date, y=TSS_postfilt_SCADA_daily_avg)) +
  geom_point(aes(alpha = transparency)) +
  mytheme

## turbidity - SCADA
ggplot(df1, aes(x=date, y=turb_postfilt_SCADA_daily_avg)) +
  geom_point(aes(alpha = transparency)) +
  mytheme

## turbidity - daily grab
ggplot(df1 %>% filter(turb_postfilt_daily_grab < 300), aes(x=date, y=turb_postfilt_daily_grab)) +
  geom_point(aes(alpha = transparency)) +
  mytheme


# estimate and visualize TSS removal

## combined TSS from each clarifier (prefilter) and each clarifier alone
df2 <- filter(df1, date >= as.Date("2019-04-17")) %>% 
  select(date, contains("prefilt"))
colnames(df2) <- c("date", "clarifier1", "clarifier2", "clarifier3", "clarifier4", "clarifiers_combined")
df2$mean_clarifiers <- rowMeans(df2[,2:5], na.rm=TRUE)  # calculate average to see if that is representative of combined value

### add rolling averages for visualization
week <- 7
df2 <- mutate(df2, weekly_rollmean_clar1 = rollmean(clarifier1, k = week, na.pad=TRUE, align="right"), 
              weekly_rollmean_clar2 = rollmean(clarifier2, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_clar3 = rollmean(clarifier3, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_clar4 = rollmean(clarifier4, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_combo = rollmean(clarifiers_combined, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_mean = rollmean(mean_clarifiers, k = week, na.pad=TRUE, align="right"))

### gather data for visualization and plot
df2.rollmean <- select(df2, date, contains("rollmean")) %>% 
  gather(key = "location", value = "tss_mgperL", -date)
ggplot(df2.rollmean, aes(x=date, y=tss_mgperL, color=location)) +
  geom_line() +
  ylab("TSS (mg/L) - Weekly Rolling Mean") +
  xlab("Date") +
  mytheme

### figure out multiplication factors which map the mean to the combined TSS
mult.factor <- mean(df2$weekly_rollmean_combo, na.rm = TRUE)/
  mean(df2$weekly_rollmean_mean, na.rm = TRUE)
# 1.545

df2.rollmean2 <- mutate(df2, adj_weekly_rollmean_mean = weekly_rollmean_mean * mult.factor) %>%
  select(date, weekly_rollmean_combo, adj_weekly_rollmean_mean) %>% 
  gather(key = "location", value = "tss_mgperL", -date)

ggplot(df2.rollmean2, aes(x=date, y=tss_mgperL, color=location)) +
  geom_line() +
  ylab("TSS (mg/L) - Weekly Rolling Mean") +
  xlab("Date") +
  mytheme

## equiped with multiplication factor, determine estimated pre-filter TSS for entire dataset and estimate removal
df3 <- df1 %>%
  select(date, contains("prefilt"))
colnames(df3) <- c("date", "clarifier1", "clarifier2", "clarifier3", "clarifier4", "clarifiers_combined")
df3$mean_clarifiers <- rowMeans(df3[,2:5], na.rm=TRUE)  # calculate average to see if that is representative of combined value
df3 <- mutate(df3, weekly_rollmean_clar1 = rollmean(clarifier1, k = week, na.pad=TRUE, align="right"), 
              weekly_rollmean_clar2 = rollmean(clarifier2, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_clar3 = rollmean(clarifier3, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_clar4 = rollmean(clarifier4, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_combo = rollmean(clarifiers_combined, k = week, na.pad=TRUE, align="right"),
              weekly_rollmean_mean = rollmean(mean_clarifiers, k = week, na.pad=TRUE, align="right")) %>%
  mutate(adj_weekly_rollmean_mean = weekly_rollmean_mean * mult.factor)

### visualize adjusted rolling mean
ggplot(df3, aes(x=date, y=adj_weekly_rollmean_mean)) +
  geom_line() +
  mytheme

## estimate removal

### calculate weekly rolling mean post filter TSS
df3.postfilt <- select(df1, date, TSS_postfilt_SCADA_daily_avg) %>%
  mutate(weekly_rollmean_postfilt = rollmean(TSS_postfilt_SCADA_daily_avg, k = week, na.pad=TRUE, align="right"))

df4 <- inner_join(df3.postfilt, df3) %>%
  select(date, weekly_rollmean_postfilt, adj_weekly_rollmean_mean) %>%
  na.omit()
colnames(df4) <- c("date", "tss_effluent", "tss_influent")
df4 <- mutate(df4, 
              est_pctrmv_tss = (tss_influent - tss_effluent) / tss_influent * 100)

ggplot(df4, aes(x=date, y=est_pctrmv_tss)) +
  geom_point() +
  xlab("Date") + 
  mytheme

# perform t-test about percent removal and visualize

## delineate new and old filter dates. DBF online: "2018-03-06"
start.ph1 <- min(df4$date) %>% as.Date()
end.ph1 <- as.Date("2018-03-05")
start.ph2 <- as.Date("2018-03-06")
end.ph2 <- max(df4$date) %>% as.Date()
df4 <- mutate(df4, phase = if_else(date >= start.ph2, 2, 1) %>% as.factor)

## calculate average TSS and turbidity for Phases 1 and 2
df4.ph1 <- filter(df4, phase == 1)
mean.tss.ph1 <- df4.ph1 %>%
  summarize(mean_tss = mean(est_pctrmv_tss)) %>% unlist()

df4.ph2 <- filter(df4, phase == 2)
mean.tss.ph2 <- df4.ph2 %>%
  summarize(mean_tss = mean(est_pctrmv_tss)) %>% unlist()


## visualize
p <- ggplot(df4, aes(x=as.Date(date), y=est_pctrmv_tss/100)) +
  geom_point(aes(fill=phase), color="white", alpha = 0.7, shape = 21, size=2) +
  geom_segment(aes(x=start.ph1, y=mean.tss.ph1/100, xend=end.ph1, yend=mean.tss.ph1/100), color=gg_color_hue(n=2)[1], size=1.6) +
  geom_segment(aes(x=start.ph2, y=mean.tss.ph2/100, xend=end.ph2, yend=mean.tss.ph2/100), color=gg_color_hue(n=2)[2], size=1.6) +
  guides(fill = guide_legend(title = "")) +
  xlab("Date") +
  ylab("TSS Removal (Estimated)") +
  scale_y_continuous(labels=scales::percent) +
  mytheme
tiff(filename = str_c(figure.dir, "est-tss-pctrmv", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p)
dev.off()

## result of t Test
tss.ttest <- t.test(x=df4.ph1$est_pctrmv_tss, y=df4.ph2$est_pctrmv_tss)$p.value

## visualize
p1 <- ggplot(df4, aes(x=as.Date(date), y=est_pctrmv_tss/100)) +
  # geom_point(aes(fill=phase), color="white", alpha = 0.7, shape = 21, size=2) +
  # geom_segment(aes(x=start.ph1, y=mean.tss.ph1/100, xend=end.ph1, yend=mean.tss.ph1/100), color=gg_color_hue(n=2)[1], size=1.6) +
  # geom_segment(aes(x=start.ph2, y=mean.tss.ph2/100, xend=end.ph2, yend=mean.tss.ph2/100), color=gg_color_hue(n=2)[2], size=1.6) +
  # guides(fill = guide_legend(title = "")) +
  xlab("Date") +
  ylab("TSS Removal (Estimated)") +
  scale_y_continuous(labels=scales::percent) +
  mytheme
tiff(filename = str_c(figure.dir, "empty_est-tss-pctrmv", ".tiff"),
     height = 12, width = 17, units = 'cm',
     compression = "lzw", res = fig.resolution)
print(p1)
dev.off()
