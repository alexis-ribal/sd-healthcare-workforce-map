############################################################
## Setup
############################################################

# Load required packages
# ggplot2: plotting
# dplyr, tidyr: data wrangling
# readr: read CSV
# stringr: string handling if needed
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)  # for dollar/number formatting

############################################################
## 1. Read in earnings means file
##    (output from previous step in your workflow)
############################################################

# Path to the CSV with mean earnings by occupation / race / sex
datapath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"
outputpath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/output/"

sd_earn_eeo_mean <- read_csv(
  file = paste0(datapath, "sd_earn_eeo_mean.csv")
)

############################################################
## 2. Reshape data to long format for plotting
##    We want one row per occupation × race_group × sex
############################################################

# 2.1 Columns to keep (as requested)
race_cols_male <- c(
  "mean_male_total_all",
  "mean_male_hisp",
  "mean_male_white",
  "mean_male_black",
  "mean_male_asian"
)

race_cols_female <- c(
  "mean_female_total_all",
  "mean_female_hisp",
  "mean_female_white",
  "mean_female_black",
  "mean_female_asian"
)

earn_long <- sd_earn_eeo_mean %>%
  select(occupation_simple, all_of(race_cols_male), all_of(race_cols_female)) %>%
  pivot_longer(
    cols = -occupation_simple,
    names_to = c("sex", "race_group"),
    names_pattern = "mean_(male|female)_(.*)",
    values_to = "mean_earnings"
  )

# Keep only the race groups we care about
earn_long <- earn_long %>%
  filter(race_group %in% c("total_all", "hisp", "white", "black", "asian"))

############################################################
## 3. Apply occupation-specific drops and insert N/A rows
############################################################

# Helper to set a given sex/race to NA for selected occupations
drop_for_occ <- function(data, occ_vec, sex_value, race_value) {
  data %>%
    mutate(
      mean_earnings = ifelse(
        occupation_simple %in% occ_vec & sex == sex_value & race_group == race_value,
        NA_real_,
        mean_earnings
      )
    )
}

earn_long <- earn_long %>%
  # Drop mean_male_black for "Miscellaneous healthcare diagnosing or treating practitioners"
  drop_for_occ(
    occ_vec = c("Miscellaneous healthcare diagnosing or treating practitioners"),
    sex_value = "male",
    race_value = "black"
  ) %>%
  # Drop mean_male_hisp for "Nursing, psychiatric, and home health aides"
  drop_for_occ(
    occ_vec = c("Nursing, psychiatric, and home health aides"),
    sex_value = "male",
    race_value = "hisp"
  ) %>%
  # Drop mean_male_black for "Other health technologists and technicians"
  drop_for_occ(
    occ_vec = c("Other health technologists and technicians"),
    sex_value = "male",
    race_value = "black"
  )

# 3.1 Add explicit N/A rows where a sex/race is missing so that the
#      position is still occupied in the grouped bars.
#      We create label_text = "N/A" and mean_earnings = 0 for those.

# First, define all combinations we want per occupation
all_combos <- expand_grid(
  occupation_simple = unique(earn_long$occupation_simple),
  sex  = c("male", "female"),
  race_group = c("total_all", "hisp", "white", "black", "asian")
)

# Join with existing data
earn_complete <- all_combos %>%
  left_join(earn_long, by = c("occupation_simple", "sex", "race_group"))

# Mark rows that are structurally missing (no record at all) as N/A,
# and rows explicitly dropped (mean_earnings == NA but record exists) also as N/A.
earn_complete <- earn_complete %>%
  mutate(
    is_na_bar = is.na(mean_earnings),
    # For N/A bars we set mean_earnings to 0 so the bar is flat but place is taken
    mean_earnings_plot = ifelse(is_na_bar, 0, mean_earnings),
    label_text_raw = ifelse(is_na_bar, NA, mean_earnings)
  )

############################################################
## 4. Nice labels: race names, sex labels, dollar formatting
############################################################

earn_complete <- earn_complete %>%
  mutate(
    # First, ensure race_group is an ordered factor in the desired order
    race_group = factor(
      race_group,
      levels = c("total_all", "white", "hisp", "black", "asian")
    ),
    # Then create human-readable labels based on race_group
    race_group_label = case_when(
      race_group == "total_all" ~ "All groups",
      race_group == "white"     ~ "White",
      race_group == "hisp"      ~ "Hispanic or Latino",
      race_group == "black"     ~ "Black or African American",
      race_group == "asian"     ~ "Asian",
      TRUE                      ~ as.character(race_group)
    ),
    # Make labels a factor that preserves the same order as race_group
    race_group_label = factor(
      race_group_label,
      levels = c(
        "All groups",                       # total_all
        "White",                      # white
        "Hispanic or Latino",               # hisp
        "Black or African American",  # black
        "Asian"                       # asian
      )
    ),
    sex_label = recode(
      sex,
      "male" = "Male",
      "female" = "Female"
    ),
    # Label: $xx,xxx for numeric, "N/A" for missing
    label_text = ifelse(
      is_na_bar,
      "N/A",
      dollar(label_text_raw, accuracy = 1)
    )
  )

# Order race groups; put All groups at the top
earn_complete <- earn_complete %>%
  group_by(occupation_simple) %>%
  arrange(
    factor(
      race_group_label,
      levels = c(
        "All groups",
        "Hispanic or Latino",
        "White",
        "Black or African American",
        "Asian"
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    race_group_label = factor(
      race_group_label,
      levels = c(
        "All groups",
        "White",
        "Hispanic or Latino",
        "Black or African American",
        "Asian"
      )
    )
  )

############################################################
## 5. Plot function with N/A bars and dollar axis
############################################################

plot_occupation_earnings <- function(df_occ, occ_name) {
  p <- ggplot(
    df_occ,
    aes(
      x    = race_group_label,
      y    = mean_earnings_plot,
      fill = sex_label
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.7),
      width = 0.6
    ) +
    geom_text(
      aes(label = label_text),
      position = position_dodge(width = 0.7),
      hjust = 1.05,
      color = "white",
      size = 3
    ) +
    coord_flip() +
    scale_fill_manual(
      name = "Sex",
      values = c("Male" = "#06B7DF", "Female" = "#F69554")
    ) +
    scale_y_continuous(
      labels = dollar_format(accuracy = 1),  # y-axis with $ and commas
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = paste0("Mean Earnings by Sex and Race\n", occ_name),
      subtitle = "San Diego-Carlsbad, CA Metro Area, 2014–2018 ACS 5-year EEO Estimates",
      x = "Race/ethnicity group",
      y = "Mean earnings (dollars)"
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



unique_occupations <- unique(earn_complete$occupation_simple)

for (occ in unique_occupations) {
  df_occ <- earn_complete %>%
    filter(occupation_simple == occ)
  
  if (nrow(df_occ) == 0) next
  
  p_occ <- plot_occupation_earnings(df_occ, occ_name = occ)
  
  safe_name <- occ %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_trim(side = "both") %>%
    str_replace("^_", "") %>%
    str_replace("_$", "")
  
  file_out <- paste0(outputpath, "mean_earnings_", safe_name, ".png")
  
  ggsave(
    filename = file_out,
    plot = p_occ,
    width = 9,
    height = 6,
    dpi = 300
  )
}