# San Diego’s Healthcare Workforce Strategy  
Bridging the Specialty Gap While Building Workforce Equity

> Labor Market Analysis & Recommendations for SDWP Investment  
> Author: Alexis Rivera

---

## Overview

This repository documents a labor market analysis of San Diego’s healthcare sector, with a focus on:

- Identifying **specialty shortages** (especially nursing and advanced practice roles)  
- Understanding **wage and skill polarization** within healthcare  
- Highlighting **equity gaps** by race, ethnicity, sex, and geography  
- Providing **evidence-based policy recommendations** for the San Diego Workforce Partnership (SDWP)

The work was prepared as part of a Work Sample Assignment for the **Senior Research Analyst** position at SDWP and aims to demonstrate how to translate complex data into actionable workforce strategy.

---

## Reproducibility package files

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


