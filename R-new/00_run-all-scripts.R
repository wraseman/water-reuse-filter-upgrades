# purpose: run all data analysis and visualization scripts 
# author: Billy Raseman

# clear environment
rm(list = ls())

# step 1 - read in laboratory data
source("./R-new/01_read-clean_ca-labs.R")
source("./R-new/01_read-clean_sb-labs.R")

# step 2 - combine all laboratory data
source("./R-new/02_combine_all-labs.R")

# step 3 - visualize the number of samples, concentration over time, and detects vs. non-detects
source("./R-new/03_viz_num-samples.R")  
## results: see ./figures/combined-lab-results/ for visualizations
source("./R-new/03_viz_conc-v-time_detect-v-nondetect.R") 
## results: see ./figures/combined-lab-results/*/detect-nondetect_bar-plots and
##  ./figures/combined-lab-results/*/concentration-v-time/ for visualizations 

# step 4 - calculate percent removal of analytes due to filtration and chlorine oxidation
source("./R-new/04_calc_pctrmv_cloxidation.R")
source("./R-new/04_calc_pctrmv_filtration.R")

# step 5 - visualize percent removal data
source("./R-new/05_viz_boxplot_pctrmv_cloxidation_CECs.R")
source("./R-new/05_viz_boxplot_pctrmv_filtration_CECs.R")
## results: see ./figures/combined-lab-results/*/pctrmv-v-time/boxplots_CECs-only
source("./R-new/05_viz_pctrmv-v-time_filtration.R")