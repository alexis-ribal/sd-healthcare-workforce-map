############################################################
## Setup
############################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)

############################################################
## 1. Read in education/employment file
############################################################

# Path to the CSV with mean earnings by occupation / race / sex
datapath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"
outputpath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/output/"

sd_educ_eeo <- read_csv(
  file = paste0(datapath, "sd_educ_eeo.csv")
)

############################################################
## 2. Reshape to long and sum male + female (exclude totals)
############################################################

# We now EXCLUDE emp_*_total_all and keep only race-specific groups
emp_male_cols <- c(
  "emp_male_white",
  "emp_male_hisp",
  "emp_male_black",
  "emp_male_asian"
)

emp_female_cols <- c(
  "emp_female_white",
  "emp_female_hisp",
  "emp_female_black",
  "emp_female_asian"
)

educ_long <- sd_educ_eeo %>%
  select(
    occupation_simple,
    degree,
    all_of(emp_male_cols),
    all_of(emp_female_cols)
  ) %>%
  pivot_longer(
    cols = starts_with("emp_"),
    names_to = c("sex", "race_group"),
    names_pattern = "emp_(male|female)_(.*)",
    values_to = "employment"
  )

# 2.1 Sum male + female by occupation × degree × race_group
educ_sum <- educ_long %>%
  group_by(occupation_simple, degree, race_group) %>%
  summarise(
    employment_total = sum(employment, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
## 3. Convert counts to shares within occupation × race_group
############################################################

# 3.1 Compute total employment per occupation × race_group
occ_race_totals <- educ_sum %>%
  group_by(occupation_simple, race_group) %>%
  summarise(
    employment_occ_race_total = sum(employment_total, na.rm = TRUE),
    .groups = "drop"
  )

# 3.2 Join back and compute shares
educ_share <- educ_sum %>%
  left_join(
    occ_race_totals,
    by = c("occupation_simple", "race_group")
  ) %>%
  mutate(
    share = if_else(
      employment_occ_race_total > 0,
      employment_total / employment_occ_race_total,
      NA_real_
    )
  )

############################################################
## 4. Clean labels and enforce race/degree order
############################################################

educ_share <- educ_share %>%
  mutate(
    race_group_label = case_when(
      race_group == "white" ~ "White alone",
      race_group == "hisp"  ~ "Hispanic or Latino",
      race_group == "black" ~ "Black or African American alone",
      race_group == "asian" ~ "Asian alone",
      TRUE                  ~ race_group
    )
  )

# Order race groups: white, hisp, black, asian (no total now)
educ_share <- educ_share %>%
  mutate(
    race_group_label = factor(
      race_group_label,
      levels = c(
        "White alone",
        "Hispanic or Latino",
        "Black or African American alone",
        "Asian alone"
      )
    )
  )

degree_levels <- c(
  "Not high school graduate",
  "High school graduate (including equivalency)",
  "Some college or associate degree",
  "Bachelor's degree",
  "Master's degree",
  "Doctoral degree or professional degree"
)

educ_share <- educ_share %>%
  mutate(
    degree = factor(degree, levels = degree_levels)
  )

############################################################
## 5. Plot function: clustered bars by degree, y = share
############################################################

plot_occupation_degree_shares <- function(df_occ, occ_name) {
  # Position dodge used for both bars and labels
  dodge_width <- 0.8
  
  p <- ggplot(
    df_occ,
    aes(
      x    = race_group_label,
      y    = share,
      fill = degree
    )
  ) +
    # Bars
    geom_col(
      position = position_dodge(width = dodge_width),
      width = 0.7
    ) +
    # Percentage labels on each bar
    geom_text(
      aes(label = scales::percent(share, accuracy = 1)),
      position = position_dodge(width = dodge_width),
      hjust = -0.1,      # slightly to the right of the bar end after coord_flip
      size  = 2.5        # smaller font
    ) +
    coord_flip() +
    # Custom colors for the 6 degree levels (order must match degree_levels)
    scale_fill_manual(
      name   = "Degree level",
      values = c(
        "Not high school graduate"                     = "#06B7DF",
        "High school graduate (including equivalency)" = "#84D9F7",
        "Some college or associate degree"             = "#F69554",
        "Bachelor's degree"                            = "#4978BC",
        "Master's degree"                              = "#373F51",
        "Doctoral degree or professional degree"       = "#E4E4E4"
      )
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1),
      # Extra space on the right (top, after flip) for labels
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = paste0("Share of Employees by Degree and Race\n", occ_name),
      subtitle = "San Diego-Carlsbad, CA Metro Area, 2014–2018 ACS 5-year EEO Estimates",
      x = "Race/ethnicity group",
      y = "Share of employees",
      caption = "Source: EEO-ALL08W, 2014–2018 ACS 5-year EEO Estimates"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}

############################################################
## 6. Export charts per occupation
############################################################


unique_occupations <- unique(educ_share$occupation_simple)

for (occ in unique_occupations) {
  df_occ <- educ_share %>%
    filter(occupation_simple == occ, !is.na(race_group_label), !is.na(degree), !is.na(share))
  
  if (nrow(df_occ) == 0) next
  
  p_occ <- plot_occupation_degree_shares(df_occ, occ_name = occ)
  
  safe_name <- occ %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_trim(side = "both") %>%
    str_replace("^_", "") %>%
    str_replace("_$", "")
  
  file_out <- paste0(outputpath, "emp_share_by_degree_", safe_name, ".png")
  
  ggsave(
    filename = file_out,
    plot = p_occ,
    width = 9,
    height = 6,
    dpi = 300
  )
}
