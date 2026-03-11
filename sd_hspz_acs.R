# ==============================================================================
# Script Name: San Diego ACS data for healthcare workers geographical distribution
# Author:      Alexis Rivera
# Date:        March 9, 2026
# Purpose:     Clean healthcare employment data by zipcode from ACS 2024.
# ==============================================================================

library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)

# 1. Setup
# census_api_key("YOUR_KEY_HERE", install = TRUE)
options(tigris_use_cache = TRUE)

# Define local directory for raw data assets
path <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"

# 2. Identify San Diego County ZCTAs
sd_county <- counties(state = "CA", cb = TRUE) %>% 
  filter(NAME == "San Diego")

sd_zctas <- zctas(cb = TRUE, year = 2020) %>% 
  st_filter(sd_county, .predicate = st_intersects) %>% 
  pull(GEOID20)

# 3. Define variables (Occupation & Total Employment)
# C24010 splits occupations by sex, so we pull both and sum them.
# Source: https://api.census.gov/data/2024/acs/acs5/variables.html
acs_vars <- c(
  total_emp = "C24030_001E",
  prac_male = "C24010_016E",
  prac_fem  = "C24010_052E",
  supp_male = "C24010_020E",
  supp_fem  = "C24010_056E"
)

# 4. Fetch the data (2024 ACS 5-Year)
sd_hc_density <- get_acs(
  geography = "zcta",
  variables = acs_vars,
  year = 2024,
  survey = "acs5",
  zcta = sd_zctas,
  output = "wide"
)

# 5. Process and Calculate Rates per 100 people
sd_hc_zc <- sd_hc_density %>%
  select(GEOID, total_emp, prac_male, prac_fem, supp_male, supp_fem) %>%
  mutate(
    # Summing male and female counts
    num_practitioners = prac_male + prac_fem,
    num_support       = supp_male + supp_fem,
    
    # Calculating rates per 100 residents
    prac_per_100 = (num_practitioners / total_emp) * 100,
    supp_per_100 = (num_support / total_emp) * 100,
    health_per_100 = (num_practitioners+num_support) / total_emp * 100,
    supp_vs_prac = num_support - num_practitioners
  ) %>%
  select(
    zip_code = GEOID, 
    total_employed = total_emp,
    num_practitioners,
    prac_per_100,
    num_support,
    supp_per_100,
    health_per_100,
    supp_vs_prac
  )

# 6. View results (sorted by density of practitioners)
sd_hc_zc %>% 
  arrange(desc(prac_per_100)) %>% 
  head(10)

# 7. Export data
write_csv(sd_hc_zc, paste0(path,"sd_hc_zc.csv"))
