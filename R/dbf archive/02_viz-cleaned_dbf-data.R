# Purpose: Visualize cleaned deep bed filtration (DBF) data from Loxahatchee plant. 
# Output: None
# Author: William Raseman

# clear environment
rm(list = ls())

# load packages
library(tidyverse)  # ggplot2, dplyr for data wrangling and visualization

# read in cleaned DBF data
data.dir <- "./data/"
filename <- "cleaned_dbf-data.rds"
data.path <- str_c(data.dir, filename)
dbf.df <- read_rds(path = data.path) 
analytes <- unique(dbf.df$analyte)

# visualize time series data for each analyte 
for (i in 1:length(analytes)) {
# i <- 10
  filt.df <- filter(dbf.df, analyte == analytes[i])
  
  ## plot each location on same plot (same y-axis scale)
  p.same <- ggplot(filt.df, aes(x=collect_dttm, y=result)) +
    geom_point(aes(color=location)) +
    geom_line(aes(linetype=location, color=location)) +
    xlab("Datetime") + 
    ylab("Result") + 
    ggtitle(analytes[i])
  
  print(p.same)
  
  # ## plot each location on different plot (different y-axis scale)
  # p.diff <- ggplot(filt.df, aes(x=collect_dttm, y=result)) +
  #   geom_point() + 
  #   geom_line() +
  #   xlab("Datetime") + 
  #   ylab("Result") + 
  #   facet_grid(location ~ ., scales = "free") + 
  #   ggtitle(analytes[i])
  # 
  # print(p.diff)
}
