############################################################
## Setup
############################################################

# Load packages
library(readxl)    # read_excel
library(dplyr)     # data wrangling
library(stringr)   # string operations
library(tidyr)     # pivot_longer / pivot_wider

############################################################
## 1. Median earnings file (EEO-ALL11W...): 
##    keep median earnings by sex, simplify occupation,
##    rename columns C–J, reshape wide by race × sex
############################################################

# Define local directory for raw data assets
path <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"


# 1.1 Read the Data sheet
earnings_raw <- read_excel(
  path = paste0(path,"EEO-ALL11W for San Diego-Carlsbad, CA Metro Area 2014-2018 ACS 5-Year EEO Estimates .xlsx"),
  sheet = "Data"
)

# Inspect names (for reference)
names(earnings_raw)
# Expected key variables from the file:
#   occupation, indicator, Total All Groups, Hispanic or Latino, White alone,
#   Black or African American alone, American Indian /Alaska Native alone,
#   Asian alone, Native Hawaiian /Pacific Islander alone, Balance of not Hispanic or Latino [file:2]

# 1.2 Filter to only median earnings for Male and Female
earnings_mf <- earnings_raw %>%
  filter(
    indicator %in% c("Median Earnings - Male", "Median Earnings - Female")
  )

# 1.3 Simplify occupation text
# Example pattern in file:
# "Miscellaneous healthcare diagnosing or treating practitioners : 29-1290 / 3261" [file:2]
# We keep only the text before the colon and trim spaces.
earnings_mf <- earnings_mf %>%
  mutate(
    occupation_simple = str_trim(str_split_fixed(occupation, ":", 2)[, 1])
  )

# 1.4 Give short names to columns C–J
# Column A: occupation
# Column B: indicator
# Columns C–J: race/ethnicity groups [file:2]
earnings_mf <- earnings_mf %>%
  rename(
    total_all   = `Total All Groups`,
    hisp        = `Hispanic or Latino`,
    white       = `White alone`,
    black       = `Black or African American alone`,
    amind_ak    = `American Indian /Alaska Native alone`,
    asian       = `Asian alone`,
    nhpi        = `Native Hawaiian /Pacific Islander alone`,
    other_nonh  = `Balance of not Hispanic or Latino`
  )

# 1.5 Reshape so unit of observation is occupation,
#     with separate columns for median earnings by sex and race
#
# Strategy:
#   - Long format over race columns
#   - Encode sex from indicator
#   - Wide back out to one row per occupation, race, with sex-specific columns
#   - Then, if desired, collapse race to columns as well.

# 1.5.1 Long over race columns
# 1.5.1 Ensure race columns are numeric, then long over race columns
earnings_mf <- earnings_mf %>%
  mutate(
    across(
      c(total_all, hisp, white, black, amind_ak, asian, nhpi, other_nonh),
      ~ suppressWarnings(as.numeric(.x))
    )
  )

earnings_long <- earnings_mf %>%
  select(occupation_simple, indicator,
         total_all, hisp, white, black, amind_ak, asian, nhpi, other_nonh) %>%
  tidyr::pivot_longer(
    cols = c(total_all, hisp, white, black, amind_ak, asian, nhpi, other_nonh),
    names_to = "race_group",
    values_to = "median_earnings"
  )

# 1.5.2 Create sex variable from indicator
earnings_long <- earnings_long %>%
  mutate(
    sex = case_when(
      indicator == "Median Earnings - Male"   ~ "male",
      indicator == "Median Earnings - Female" ~ "female",
      TRUE                                    ~ NA_character_
    )
  )

# 1.5.3 Wide: one row per occupation and race, columns for male/female
earnings_wide_sex <- earnings_long %>%
  select(occupation_simple, race_group, sex, median_earnings) %>%
  pivot_wider(
    names_from  = sex,
    values_from = median_earnings,
    names_glue  = "median_{sex}"
  )

# 1.5.4 Optional final structure:
#       one row per occupation, columns: median_male_race, median_female_race
#       This matches "columns have median earnings for male and female for each race group".
sd_earn_eeo <- earnings_wide_sex %>%
  pivot_wider(
    id_cols     = occupation_simple,
    names_from  = race_group,
    values_from = c(median_male, median_female),
    names_glue  = "{.value}_{race_group}"
  )

# Export data
write_csv(sd_earn_eeo, paste0(path,"sd_earn_eeo.csv"))

# Result: earnings_final has one row per occupation, 
#         and columns like:
#         median_male_total_all, median_female_total_all,
#         median_male_hisp, median_female_hisp, etc. [file:2]


############################################################
## 2. Employment / education file (EEO-ALL08W...):
##    keep Male/Female, simplify occupation, short names,
##    derive degree variable, reshape wide by sex × race × degree
############################################################

# 2.1 Read the Data sheet
educ_raw <- read_excel(
  path = paste0(path,"EEO-ALL08W for San Diego-Carlsbad, CA Metro Area 2014-2018 ACS 5-Year EEO Estimates .xlsx"),
  sheet = "Data"
)

# Inspect names (for reference)
names(educ_raw)
# From snippet: occupation, indicator, Total All Groups, Hispanic or Latino, White alone,
# Black or African American alone, American Indian /Alaska Native alone, Asian alone,
# Native Hawaiian /Pacific Islander alone, Balance of not Hispanic or Latino [file:1]

# 2.2 Keep only observations with Male and Female under indicator
educ_mf <- educ_raw %>%
  filter(indicator %in% c("Male", "Female"))

# 2.3 Simplify occupation text
# Pattern example:
# "Miscellaneous healthcare diagnosing or treating practitioners: 29-1290 / 3261 -- Not high school graduate" [file:1]
# We keep the text before the colon as the occupation label.
educ_mf <- educ_mf %>%
  mutate(
    occupation_simple = str_trim(str_split_fixed(occupation, ":", 2)[, 1])
  )

# 2.4 Give short names to columns C–J (same structure as earnings file)
educ_mf <- educ_mf %>%
  rename(
    total_all   = `Total All Groups`,
    hisp        = `Hispanic or Latino`,
    white       = `White alone`,
    black       = `Black or African American alone`,
    amind_ak    = `American Indian /Alaska Native alone`,
    asian       = `Asian alone`,
    nhpi        = `Native Hawaiian /Pacific Islander alone`,
    other_nonh  = `Balance of not Hispanic or Latino`
  )

# 2.5 Create degree variable from occupation text
# Degree appears after "--", examples: 
#   "-- Not high school graduate"
#   "-- High school graduate (including equivalency)"
#   "-- Some college or associate degree"
#   "-- Bachelor's degree"
#   "-- Master's degree"
#   "-- Doctoral degree or professional degree" [file:1]
# We extract the part after "--" and trim it.

educ_mf <- educ_mf %>%
  mutate(
    degree = str_trim(str_split_fixed(occupation, "--", 2)[, 2]),
    degree = ifelse(degree == "", NA_character_, degree)
  )

# 2.6 Reshape so unit of observation is occupation,
#     with counts of employed by education level, sex, and race group

# 2.6.1 Long over race columns
educ_long <- educ_mf %>%
  select(occupation_simple, indicator, degree,
         total_all, hisp, white, black, amind_ak, asian, nhpi, other_nonh) %>%
  pivot_longer(
    cols      = c(total_all, hisp, white, black, amind_ak, asian, nhpi, other_nonh),
    names_to  = "race_group",
    values_to = "employment"
  )

# 2.6.2 Encode sex
educ_long <- educ_long %>%
  mutate(
    sex = case_when(
      indicator == "Male"   ~ "male",
      indicator == "Female" ~ "female",
      TRUE                  ~ NA_character_
    )
  )

# 2.6.3 Wide: one row per occupation, degree, race, with male/female cols
educ_wide_sex <- educ_long %>%
  select(occupation_simple, degree, race_group, sex, employment) %>%
  pivot_wider(
    names_from  = sex,
    values_from = employment,
    names_glue  = "emp_{sex}"
  )

# 2.6.4 Final: one row per occupation and degree, with race-specific columns
sd_educ_eeo <- educ_wide_sex %>%
  pivot_wider(
    id_cols     = c(occupation_simple, degree),
    names_from  = race_group,
    values_from = c(emp_male, emp_female),
    names_glue  = "{.value}_{race_group}"
  )

# Export data

write_csv(sd_educ_eeo, paste0(path,"sd_educ_eeo.csv"))

# Result: educ_final has one row per occupation × degree (education level),
#         and columns like:
#         emp_male_total_all, emp_female_total_all,
#         emp_male_hisp, emp_female_hisp, etc. [file:1]


