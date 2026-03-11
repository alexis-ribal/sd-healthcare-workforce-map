# ============================================================
# Bubble scatterplot of employment vs wage CAGR by occupation
# ============================================================

# Assumes you already have:
#   panel_clean   : cleaned panel dataset (with occ_code recodes)
#   plot_df_emp   : data frame with employment CAGRs (cagr_2016_2025)
#   plot_df_wage  : data frame with wage CAGRs (cagr_2016_2025)
#
# Required packages
library(tidyverse)
library(scales)
library(stringr)
library(ggrepel)
library(ggtext)


# Define local directory for raw data assets
datapath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"
outputpath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/output/"

# Update the path if the file is stored somewhere else.
panel <- readr::read_csv(paste0(datapath,"sd_oews_panel.csv"), show_col_types = FALSE)



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


plot_df_emp <- readr::read_csv(paste0(datapath,"plot_df_emp.csv"), show_col_types = FALSE)
plot_df_wage <- readr::read_csv(paste0(datapath,"plot_df_wage.csv"), show_col_types = FALSE)

# -----------------------------------------------------------
# 1) Standardize names for employment and wage CAGRs
# -----------------------------------------------------------
# Here we rename columns so we can join cleanly and know which is which.
#   emp_cagr_2016_2025  : employment CAGR 2016–2025
#   wage_cagr_2016_2025 : wage CAGR 2016–2025

plot_df_emp_std <- plot_df_emp %>%
  mutate(occ_code = as.character(occ_code)) %>%        # ensure character
  select(occ_name, occ_code, cagr_2016_2025) %>%
  rename(emp_cagr_2016_2025 = cagr_2016_2025)

plot_df_wage_std <- plot_df_wage %>%
  mutate(occ_code = as.character(occ_code)) %>%        # ensure character
  select(occ_name, occ_code, cagr_2016_2025) %>%
  rename(wage_cagr_2016_2025 = cagr_2016_2025)

# -----------------------------------------------------------
# 2) Compute average employment over 2016–2025 period
# -----------------------------------------------------------
# We take the mean tot_emp across all years 2016–2025 for each occupation.
# This value will drive the bubble size in the scatterplot.

avg_emp_2016_2025 <- panel_clean %>%
  mutate(occ_code = as.character(occ_code)) %>%        # ensure character
  filter(year >= 2016, year <= 2025) %>%
  group_by(occ_name, occ_code) %>%
  summarise(
    avg_tot_emp_2016_2025 = mean(tot_emp, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------
# 3) Build combined bubble dataset
# -----------------------------------------------------------
# We join employment and wage CAGRs by occ_name and occ_code,
# then add the average employment, and compute color groups.

bubble_df <- plot_df_emp_std %>%
  inner_join(plot_df_wage_std, by = c("occ_name", "occ_code")) %>%
  inner_join(avg_emp_2016_2025, by = c("occ_name", "occ_code")) %>%
  mutate(
    occ_group = case_when(
      str_starts(occ_code, "29") ~ "Healthcare Practitioners and Technical Occupations",
      str_starts(occ_code, "31") ~ "Healthcare Support Occupations",
      TRUE ~ "Other"
    ),
    fill_color = case_when(
      str_starts(occ_code, "29") ~ "#06B7DF",
      str_starts(occ_code, "31") ~ "#F69554",
      TRUE ~ "grey70"
    ),
    label_text = occ_name
  ) %>%
  filter(
    !is.na(emp_cagr_2016_2025),
    !is.na(wage_cagr_2016_2025),
    !is.na(avg_tot_emp_2016_2025),
    avg_tot_emp_2016_2025 > 0,
    !occ_code %in% c("291128", "319094")   # exclude these two codes
  )

# Optional sanity check:
# head(bubble_df)

# -----------------------------------------------------------
# 4) Create bubble scatterplot
# -----------------------------------------------------------
# X-axis     : employment CAGR 2016–2025
# Y-axis     : wage CAGR 2016–2025
# Color      : occupation group (29 vs 31)
# Size       : average employment 2016–2025
# Labels     : occupation names with non-overlapping labels

bubble_plot <- ggplot(
  bubble_df,
  aes(
    x     = emp_cagr_2016_2025,
    y     = wage_cagr_2016_2025,
    size  = avg_tot_emp_2016_2025,
    color = fill_color
  )
) +
  # Bubbles
  geom_point(alpha = 0.7) +
  # Labels
  geom_text_repel(
    aes(label = label_text),
    size        = 3,
    color       = "black",
    max.overlaps = 100
  ) +
  # Vertical line at 0% employment CAGR
  geom_vline(
    xintercept = 0,
    linetype   = "dashed",
    color      = "#373F51"
  ) +
  # Horizontal line at 3.6% wage CAGR
  geom_hline(
    yintercept = 0.036,
    linetype   = "dashed",
    color      = "#373F51"
  ) +
  # Arrow: Faster employment growth (right‑pointing)
  geom_segment(
    data = tibble(x = 0.01, xend = 0.03, y = 0.07, yend = 0.07),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.2, "cm")),
    inherit.aes = FALSE,
    color = "#373F51"
  ) +
  geom_text(
    data = tibble(x = 0.01, y = 0.068, label = "Faster employment growth"),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size  = 3.5,
    color = "#373F51"
  ) +
  # Arrow: Faster wage growth (up‑pointing)
  geom_segment(
    data = tibble(x = 0.08, xend = 0.08, y = 0.04, yend = 0.055),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.2, "cm")),
    inherit.aes = FALSE,
    color = "#373F51"
  ) +
  geom_text(
    data = tibble(x = 0.081, y = 0.05, label = "Faster wage growth"),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    size  = 3.5,
    color = "#373F51"
  ) +
  # Colors, sizes, axes etc. (unchanged)
  scale_color_identity(
    guide  = "legend",
    labels = c(
      "#06B7DF" = "Healthcare Practitioners and Technical Occupations",
      "#F69554" = "Healthcare Support Occupations",
      "grey70"  = "Other"
    ),
    name   = "Occupation group"
  ) +
  scale_size_continuous(
    range  = c(2, 15),
    breaks = pretty(bubble_df$avg_tot_emp_2016_2025, 4),
    labels = scales::comma,
    name   = "Average employment, 2016–2025"
  ) +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    name   = "Compound annual employment growth rate, 2016–2025"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    name   = "Compound annual average wage growth rate, 2016–2025"
  ) +
  labs(
    title    = "Employment vs Wage Growth by Healthcare Occupation",
    subtitle = "Bubble size shows average employment (2016–2025)",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )


# -----------------------------------------------------------
# 5) Display and optionally save the bubble chart
# -----------------------------------------------------------
print(bubble_plot)

# Example save (adjust outputpath as needed)
ggsave(
  filename = paste0(outputpath, "employment_vs_wage_cagr_bubbles.png"),
  plot     = bubble_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)
