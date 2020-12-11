# purpose: run all data analysis and visualization scripts 
# author: Billy Raseman

# clear environment
rm(list = ls())

# remove figures from previous runs
fig.dir <- "./figures/combined-lab-results/"
unlink(str_c(fig.dir, "*"))

filters <- c("DBF", "SMF", "SMF+TBF", "TBF")
for (filter in filters) {
  
  # remove figures from filter directories
  filt.dir <- str_c(fig.dir, filter, "/")
  unlink(str_c(filt.dir, "*"))
  
  # remove figures from filter subdirectories
  unlink(str_c(filt.dir, "concentration-v-time/*"))
  unlink(str_c(filt.dir, "detect-nondetect_bar-plots/*"))
  unlink(str_c(filt.dir, "pctrmv_boxplots/*"))
  unlink(str_c(filt.dir, "pctrmv-v-time/*"))
}

# step 1 - read in laboratory data
source("./code/01_read-clean_ca-labs.R")
source("./code/01_read-clean_sb-labs.R")

# step 2 - combine all laboratory data
source("./code/02_combine_all-labs.R")

# step 3 - visualize the number of samples, concentration over time, and detects vs. non-detects
source("./code/03_table_stats_inf-eff.R")  
## results: Table - statistics for TrOCs and pathogens in treatment plant influent for data associated with TBF and SMF
source("./code/03_viz_num-samples.R") 
## results: see ./figures/combined-lab-results/ for visualizations
source("./code/03_viz_conc-v-time_detect-v-nondetect.R") 
## results: see ./figures/combined-lab-results/*/detect-nondetect_bar-plots and
##  ./figures/combined-lab-results/*/concentration-v-time/ for visualizations 

# step 4 - calculate percent removal of analytes due to filtration and Chloramine Oxidation
source("./code/04_calc_pctrmv_chloramineoxidation.R")
source("./code/04_calc_pctrmv_filtration.R")

# step 5 - visualize percent removal data
source("./code/05_viz_boxplot_pctrmv_chloramineoxidation_TrOCs.R")
source("./code/05_viz_boxplot_pctrmv_filtration_TrOCs.R")
## results: see ./figures/combined-lab-results/*/pctrmv-
source("./code/05_viz_boxplot_pctrmv_filtration_pathogens.R")
## results: see ./figures/combined-lab-results/
source("./code/05_viz_pctrmv-v-time_filtration.R")

# step 6 - perform statistical tests on data (e.g., Wilcoxon Rank Sum test, Welch's t-test, Mann-Whitney U test)
source("./code/06_stat-test_chloramineoxidation_TrOCs.R")  # TrOC Removal
source("./code/06_stat-test_filtration_pathogens.R")  # Pathogen Removal
source("./code/06_stat-test_filtration_TrOCs.R")  # TrOC Removal
source("./code/06_stat-test_viz_tss-turb_operational-impacts.R")  # Operational Impacts