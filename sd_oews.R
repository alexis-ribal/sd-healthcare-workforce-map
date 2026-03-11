# ==============================================================================
# Script Name: San Diego OEWS Data Integration and Cleaning
# Author:      Alexis Rivera
# Date:        March 9, 2026
# Purpose:     Clean, standardize, and merge multi-year Occupational Employment 
#              and Wage Statistics (OEWS) for the San Diego MSA.
# ==============================================================================

# 1. LOAD DEPENDENCIES ---------------------------------------------------------
library(readr)   # For efficient data ingestion
library(dplyr)   # For data manipulation and piping
library(stringr) # For regex-based string cleaning

# 2. CONFIGURATION & PATHS -----------------------------------------------------
# Define local directory for raw data assets
path <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"

# Define variables requiring numeric conversion and quality flagging
vars <- c("tot_emp", "wage_avg", "wage_25pct", "wage_50pct", "wage_75pct", "wage_se")

# 3. DATA INGESTION ------------------------------------------------------------
# Primary dataset: Multi-year historical OES data
# Source: https://labormarketinfo.edd.ca.gov/cgi/dataanalysis/AreaSelection.asp?tableName=Oeswage&geogArea=0604000073
sd_oews_hist <- read_csv(paste0(path, "san_oews_hist.txt"))

# Supplemental dataset: 2025 OES data
# Source: https://labormarketinfo.edd.ca.gov/data/oes-employment-and-wages.html
sd_oews_2025 <- read_csv(paste0(path, "san_oews_2025.csv"))

# 4. STANDARDIZE HISTORICAL DATA -----------------------------------------------
# Filter to a single rate type to avoid double-counting and rename for snake_case consistency
sd_oews_clean <- sd_oews_hist %>%
  filter(`Rate Type` == "Hourly wage") %>%
  select(
    year       = Year,
    occ_code   = `Occ Code`,
    occ_name   = Occupation,
    tot_emp    = `Number of Employed`,
    wage_avg   = `Mean Wage`,
    wage_25pct = `25th Percentile Wage`,
    wage_50pct = `50th Percentile (Median) Wage`,
    wage_75pct = `75th Percentile Wage`,
    wage_se    = `Mean Relative Standard Error for Wage`
  ) %>%
  mutate(across(all_of(vars), ~ {
    # Remove currency symbols and commas, preserve decimal points
    as.numeric(str_replace_all(.x, "[\\$,]", ""))
  }))

# 5. STANDARDIZE 2025 DATA -----------------------------------------------------
# Ensure the 2025 file matches the historical schema and cleaning logic before merge
sd_oews_2025_clean <- sd_oews_2025 %>%
  mutate(
    # Remove hyphens from SOC codes (e.g., "11-1011" -> "111011") and convert to numeric
    occ_code = as.numeric(str_replace_all(occ_code, "(?<=\\d)-(?=\\d)", "")),
    across(all_of(vars), ~ as.numeric(str_replace_all(as.character(.x), "[\\$,]", "")))
  )

# 6. DATA INTEGRATION & SORTING ------------------------------------------------
# Vertically integrate both datasets and sort by time-series and occupation
sd_oews_panel <- bind_rows(sd_oews_clean, sd_oews_2025_clean) %>%
  arrange(year, occ_code)

# 7. DATA QUALITY CONTROL (QC) -------------------------------------------------
# Create a quality flag for suppressed or missing data points
# OEWS often uses 0 or NA to indicate data suppression
sd_oews_panel <- sd_oews_panel %>%
  mutate(
    # Flag rows where any key metric is 0 or NA
    flag = if_any(all_of(vars), ~ is.na(.) | . == 0),
    flag = as.integer(flag)
  ) %>%
  # Standardize all 0 values to NA for accurate statistical calculation
  mutate(across(all_of(vars), ~na_if(., 0)))

# 8. FINAL INSPECTION ----------------------------------------------------------
glimpse(sd_oews_panel)

# 9. EXPORT DATAFRAME
write_csv(sd_oews_panel, paste0(path,"sd_oews_panel.csv"))
