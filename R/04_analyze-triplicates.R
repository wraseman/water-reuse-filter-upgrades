# Purpose: visualize variability of triplicate samples
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

## plot settings
figure.dir <- "./figures/"
fig.resolution <- 300
mytheme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  title = element_text(size = 18))

# create dataframe for TBF and SMF analysis (i.e., Phase 1)
tsf.df <- filter(all.df, Location %in% c("TBF-I", "TBF-E", "SMF-E", "Final Ph1"), 
                 Parameter != "Flow") %>%
  # select(-Detect)
  select(-DateTimeCollected, -Detect)

# list of triplicates (identified by trying to do "tsf.wide <- spread(tsf.df, Location, Value)")
trip.list <- list(c(26, 27, 28),
                  c(102, 104, 106),
                  c(232, 237, 242),
                  c(234, 239, 244),
                  c(233, 238, 243),
                  c(101, 103, 105),
                  c(208, 211, 214),
                  c(235, 240, 245),
                  c(210, 213, 216),
                  c(236, 241, 246),
                  c(209, 212, 215),
                  c(29, 31, 32),
                  c(96, 98, 100),
                  c(247, 251, 256),
                  c(249, 253, 258),
                  c(248, 252, 257),
                  c(95, 97, 99),
                  c(199, 202, 205),
                  c(30, 254, 259),
                  c(201, 204, 207),
                  c(250, 255, 260),
                  c(200, 203, 206),
                  c(263, 264, 265, 266),
                  c(267, 268, 269, 270),
                  c(271, 272, 273, 274),
                  c(275, 276, 277),
                  c(278, 279, 280),
                  c(281, 282, 283, 284),
                  c(285, 286, 287, 288),
                  c(289, 290, 291, 292),
                  c(293, 294, 295, 296),
                  c(297, 298, 299, 300),
                  c(301, 302, 303, 304),
                  c(305, 306, 307, 308),
                  c(309, 310, 311, 312),
                  c(313, 314, 315, 316),
                  c(317, 318, 319, 320),
                  c(321, 322, 323, 324),
                  c(325, 326, 327, 328),
                  c(329, 330, 331, 332),
                  c(333, 334, 335, 336),
                  c(337, 338, 339, 340),
                  c(341, 342, 343, 344),
                  c(345, 346, 347),
                  c(348, 349, 350, 351),
                  c(352, 353, 354),
                  c(355, 356, 357),
                  c(358, 359, 360, 361),
                  c(362, 363, 364),
                  c(19, 21, 23))

sd.array1 <- mean.array1 <- vector(mode = "numeric", length = length(trip.list))
for (i in 1:length(trip.list)) {
  # print(tsf.df$Value[trip.list[[i]]])
  sd.array1[i] <- sd(tsf.df$Value[trip.list[[i]]], na.rm = TRUE)
  mean.array1[i] <- mean(tsf.df$Value[trip.list[[i]]], na.rm = TRUE)
} 
cov.array1 <- sd.array1/mean.array1
cov.array1[cov.array1==0] <- NA

## visualize distribution of standard deviations/mean for triplicates (phase 1)
p1 <- qplot(cov.array1, geom="histogram") +
  xlab("Coeff. of Variation\n (i.e., Standard Deviation/Mean)") +
  ggtitle("Phase 1 Triplicates (No Non-Detects)") +
  mytheme
tiff(filename = str_c(figure.dir, "triplicates-phase1", ".tiff"),
     height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = fig.resolution)
print(p1) 
dev.off()

# create dataframe for deep bed filter (DBF) analysis (i.e., Phase 2)
dbf.df <- filter(all.df, Location %in% c("DBF-E", "DBF-I", "Final Ph2"),
                 Parameter != "Flow") %>%
  select(-DateTimeCollected, -Detect)

# list of triplicates (identified by trying to do "dbf.wide <- spread(dbf.units, Location, Value)")
trip.list2 <- list(c(50, 51, 52),
                   c(140, 151, 162),
                   c(141, 152, 163),
                   c(142, 153, 164),
                   c(143, 154, 165),
                   c(144, 155, 166),
                   c(145, 156, 167),
                   c(146, 157, 168),
                   c(147, 158, 169),
                   c(148, 159, 170),
                   c(150, 161, 172),
                   c(149, 160, 171),
                   c(44, 45, 46),
                   c(173, 184, 195),
                   c(174, 185, 196),
                   c(175, 186, 197),
                   c(176, 187, 198),
                   c(177, 188, 199),
                   c(178, 189, 200),
                   c(179, 190, 201),
                   c(180, 191, 202),
                   c(181, 192, 203),
                   c(183, 194, 205),
                   c(182, 193, 204),
                   c(47, 48, 49),
                   c(107, 118, 129),
                   c(108, 119, 130),
                   c(109, 120, 131),
                   c(110, 121, 132),
                   c(111, 122, 133),
                   c(112, 123, 134),
                   c(113, 124, 135),
                   c(114, 125, 136),
                   c(115, 126, 137),
                   c(117, 128, 139),
                   c(116, 127, 138))

sd.array2 <- mean.array2 <- vector(mode = "numeric", length = length(trip.list2))
for (i in 1:length(trip.list2)) {
  # print(dbf.df$Value[trip.list2[[i]]])
  sd.array2[i] <- sd(dbf.df$Value[trip.list2[[i]]], na.rm = TRUE)
  mean.array2[i] <- mean(dbf.df$Value[trip.list2[[i]]], na.rm = TRUE)
} 
cov.array2 <- sd.array2/mean.array2
cov.array2[cov.array2==0] <- NA

## visualize distribution of standard deviations/mean for triplicates (phase 2)
p2 <- qplot(cov.array2, geom="histogram") +
  xlab("Coeff. of Variation\n (i.e., Standard Deviation/Mean)") +
  ggtitle("Phase 2 Triplicates (No Non-Detects)") + 
  mytheme
tiff(filename = str_c(figure.dir, "triplicates-phase2", ".tiff"),
     height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = fig.resolution)
print(p2) 
dev.off()
