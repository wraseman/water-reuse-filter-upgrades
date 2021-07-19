# purpose: run all data analysis and visualization scripts
# author: Billy Raseman

# clear environment
rm(list = ls())

library(tidyverse)

# clean up "figures" directory and its contents, if it does not already exist
# source: https://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
fig.dir <- "./figures/"

if (!dir.exists(fig.dir))
{
  # if "figures" directory doesn't exist, create it
  dir.create(fig.dir)
} else {
  # if "figures" does exist, remove all files so outdated figures don't linger
  # before deleting these files, get the user's permission:
  if (interactive())
  {
    rmv.figures.bool <-
      askYesNo(
        msg = "Running all scripts! It is recommended that figures from previous runs are deleted.
                               \nIs this okay?\n\nYes will run scripts and delete outdated figures.\nNo or Cancel will run scripts but will not delete outdated figures."
      )
  }
  
  # if user responded "Yes", delete contents of "figures" directory
  if (rmv.figures.bool == TRUE)
  {
    # delete contents of "figure" directory
    fig.dir.allcontents <- str_c(fig.dir, "*")
    unlink(x = fig.dir.allcontents,
           recursive = TRUE,
           force = TRUE)
  
  }
}

# create figure subfolders if they don't already exist
fig.dir2 <- "./figures/combined-lab-results/"
if (!dir.exists(fig.dir2)) {
  dir.create(fig.dir2)
}

filters <- c("DBF", "SMF", "SMF+TBF", "TBF")
for (filter in filters) {
  filt.dir <- str_c(fig.dir2, filter, "/")
  if (!dir.exists(filt.dir)) {
    dir.create(filt.dir)
  }
  
  boxplots.dir <- str_c(filt.dir, "pctrmv_boxplots")
  if (!dir.exists(boxplots.dir)) {
    dir.create(boxplots.dir)
  }
  
  pctrmvvtime.dir <- str_c(filt.dir, "pctrmv-v-time")
  if (!dir.exists(pctrmvvtime.dir)) {
    dir.create(pctrmvvtime.dir)
  }
}

# step 1 - read in laboratory data
source("./code/01_read-clean_ca-labs.R")
source("./code/01_read-clean_sb-labs.R")

# step 2 - combine all laboratory data
source("./code/02_combine_all-labs.R")

# step 3 - visualize the number of samples, concentration over time, and detects vs. non-detects
source("./code/03_table_stats_inf-eff.R")
## results: Table - statistics for TrOCs and pathogens in treatment plant influent for data associated with TBF and SMF

# step 4 - calculate percent removal of analytes due to filtration and Chloramine Oxidation
source("./code/04_calc_pctrmv_chloramineoxidation.R")
source("./code/04_calc_pctrmv_filtration.R")

# step 5 - visualize percent removal data
source("./code/05_viz_boxplot_pctrmv_chloramineoxidation_TrOCs.R")
source("./code/05_viz_boxplot_pctrmv_filtration_TrOCs.R")
## results: see ./figures/combined-lab-results/*/pctrmv_boxplots
source("./code/05_viz_boxplot_pctrmv_filtration_pathogens.R")
## results: see ./figures/combined-lab-results/
source("./code/05_viz_boxplot_logrmv_filtration_pathogens.R")
## results: see ./figures/combined-lab-results/
source("./code/05_viz_pctrmv-v-time_filtration.R")

# step 6 - perform statistical tests on data (e.g., Wilcoxon Rank Sum test, Welch's t-test, Mann-Whitney U test)
source("./code/06_stat-test_chloramineoxidation_TrOCs.R")  # TrOC Removal
source("./code/06_stat-test_filtration_pathogens.R")  # Pathogen Removal
source("./code/06_stat-test_filtration_TrOCs.R")  # TrOC Removal
source("./code/06_stat-test_viz_tss-turb_operational-impacts.R")  # Operational Impacts

# step 7 - visualize the relationship between turbidity and pathogens
source("./code/07_viz_turbidity_pathogen_relationship.R")
