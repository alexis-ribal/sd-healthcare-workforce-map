# ============================================================
# Compute employment CAGR by occupation and create a combined
# bar + point chart from the attached sd_oews_panel.csv file
# ============================================================

# ---------------------------
# 1) Load required packages
# ---------------------------
# tidyverse: data import, wrangling, and plotting
# ggrepel: non-overlapping text labels
# scales: percent formatting for labels and axes
library(tidyverse)
library(ggrepel)
library(scales)
library(ggplot2)
library(stringr)
library(ggtext)

# ---------------------------
# 2) Read the panel dataset
# ---------------------------

# Define local directory for raw data assets
datapath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"
outputpath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/output/"

# Update the path if the file is stored somewhere else.
panel <- readr::read_csv(paste0(datapath,"sd_oews_panel.csv"), show_col_types = FALSE)

# ---------------------------
# 3) Clean and filter data
# ---------------------------
# Requested filters:
# - drop rows with flag == 1
# - exclude aggregate occupation codes 290000 and 310000
# Also coerce key variables to consistent types.
panel_clean <- panel %>%
  mutate(
    year = as.integer(year),
    occ_code = as.character(occ_code),
    occ_name = as.character(occ_name),
    tot_emp = as.numeric(tot_emp),
    flag = as.integer(flag)
  ) %>%
  filter(
    flag != 1,
    !occ_code %in% c("290000", "310000")
  )

panel_clean <- panel_clean %>%
  mutate(
    occ_code = case_when(
      occ_code == "311013" ~ "311133",
      occ_code == "292021" ~ "291292",
      occ_code == "311014" ~ "311131",
      occ_code == "311012" ~ "311132",
      occ_code == "311015" ~ "311132",
      TRUE                 ~ occ_code
    )
  ) %>%
  mutate(
    occ_name = case_when(
      occ_name == "Licensed Practical and Licensed Vocational Nurses" ~ "Licensed Vocational Nurses",
      TRUE                                     ~ occ_name
    )
  )

# ------------------------------------------------------
# 4) Create a reusable function to compute CAGR by name
# ------------------------------------------------------
# CAGR formula:
#   CAGR = (ending_value / starting_value)^(1 / number_of_years) - 1
#
# For 2006-2015 and 2016-2025, the number of compounding intervals is 9.
#
# The function:
# - keeps one row per occ_name at the start year
# - keeps one row per occ_name at the end year
# - joins them by occ_name
# - computes CAGR only when both endpoints exist and are positive
# 4) Reusable function now for wages instead of employment
calc_cagr_wage <- function(data, start_year, end_year) {
  
  n_years <- end_year - start_year
  
  start_df <- data %>%
    filter(year == start_year) %>%
    select(occ_name, start_wage = wage_avg) %>%
    distinct()
  
  end_df <- data %>%
    filter(year == end_year) %>%
    select(occ_name, end_wage = wage_avg) %>%
    distinct()
  
  cagr_name <- paste0("cagr_", start_year, "_", end_year)
  
  full_join(start_df, end_df, by = "occ_name") %>%
    mutate(
      cagr = case_when(
        is.na(start_wage) ~ NA_real_,
        is.na(end_wage) ~ NA_real_,
        start_wage <= 0 ~ NA_real_,
        end_wage <= 0 ~ NA_real_,
        TRUE ~ (end_wage / start_wage)^(1 / n_years) - 1
      )
    ) %>%
    select(occ_name, cagr) %>%
    rename(!!cagr_name := cagr)
}

# 5) Compute wage CAGRs for the two periods
cagr_2006_2015 <- calc_cagr_wage(panel_clean, 2006, 2015)
cagr_2016_2025 <- calc_cagr_wage(panel_clean, 2016, 2025)

# 6) Combine both wage CAGR series into one plotting data frame
plot_df <- panel_clean %>%
  filter(year %in% c(2016, 2025)) %>%
  distinct(occ_name, occ_code, .keep_all = TRUE) %>%
  select(occ_name, occ_code) %>%
  left_join(cagr_2006_2015, by = "occ_name") %>%
  left_join(cagr_2016_2025, by = "occ_name") %>%
  filter(!is.na(cagr_2016_2025)) %>%
  arrange(desc(cagr_2016_2025), occ_name) %>%
  mutate(
    occ_name = factor(occ_name, levels = unique(occ_name)),
    bar_label = percent(cagr_2016_2025, accuracy = 0.1),
    bar_vjust = if_else(cagr_2016_2025 >= 0, -0.25, 1.15),
    occ_group = case_when(
      str_starts(occ_code, "29") ~ "29",
      str_starts(occ_code, "31") ~ "31",
      TRUE ~ "other"
    ),
    fill_color = case_when(
      occ_group == "29" ~ "#06B7DF",
      occ_group == "31" ~ "#F69554",
      TRUE ~ "grey70"
    )
  )

if (nrow(plot_df) == 0) {
  stop("No occupations have valid 2016 and 2025 wage values after filtering. Check your data and filters.")
}

plot_df <- plot_df %>%
  distinct(occ_code, .keep_all = TRUE)

write_csv(plot_df, paste0(datapath,"plot_df_wage.csv"))

# Bar-only chart, now for wage CAGR
cagr_plot <- ggplot(plot_df, aes(x = occ_name)) +
  geom_col(
    aes(y = cagr_2016_2025, fill = fill_color),
    width = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      y = cagr_2016_2025,
      label = bar_label,
      vjust = bar_vjust
    ),
    size  = 4,
    color = "black"
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  labs(
    title   = "Compound Annual Average Wage Growth by Occupation",
    subtitle = "Bars show 2016–2025 CAGR of average wages; color indicates occupation code family: <span style='color:#06B7DF;'>practitioners</span> and <span style='color:#F69554;'>support</span> occupations",
    x       = "Occupation name",
    y       = "Compound annual growth rate of average wage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_markdown(size = 16, face = "bold"),
    plot.subtitle = element_markdown(size = 16, face = "bold"),
    axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
    panel.grid.major.x = element_blank()
  )

print(cagr_plot)

ggsave(
  filename = paste0(outputpath,"wage_cagr_bar_chart.png"),
  plot = cagr_plot,
  width = 20,
  height = 10,
  dpi = 300
)
