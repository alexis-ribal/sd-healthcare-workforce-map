################################################################################
# Master script: San Diego Healthcare Sector Analysis
# Author : Alexis Rivera
# Date   : March 10, 2026
#
# Description:
#   This master script organizes the scripts needed to produce the charts
#   and analysis by running each of them separately.
#   
################################################################################

# ------------------------------------------------------------------------------
# 0. Housekeeping: clear workspace and set working directory
# ------------------------------------------------------------------------------

# Remove all objects from the current R session
rm(list = ls())

# Show current working directory for logging
cat("Current working directory: ", getwd(), "\n")

# ------------------------------------------------------------------------------
# 1. Set code folder path - change it to folder where R scripts are located
# ------------------------------------------------------------------------------

codepath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/code/"

setwd(codepath)

# ------------------------------------------------------------------------------
# 2. Clean data
# ------------------------------------------------------------------------------

# OEWS data
source("sd_oews.R")

# EEO data
source("sd_equity_eeo.R")

# ACS data by zipcode
source("sd_hspz_acs.R")


# ------------------------------------------------------------------------------
# 3. Produce figures
# ------------------------------------------------------------------------------

# Figure 2
source("ces_projections_change.R"). # older version - needs path adjustments

# Figure 3 left
source("employment_cagr_bar_chart.R")

# Figure 3 right
source("wage_cagr_bar_chart.R")

# Figure 4
source("employment_vs_wage_cagr_bubbles.R")

# Figure 5
source("employment_forecasts_2016_2030.R")

# Figure 6
source("emp_share_by_degree.R")

# Figure 7
source("mean_earnings_race.R")


# ------------------------------------------------------------------------------
# END OF R-SCRIPT
# ------------------------------------------------------------------------------



