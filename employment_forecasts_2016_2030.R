# ============================================================
# Forecast total employment 2026–2030 and plot 2016–2030
# for selected occupations using the forecast package
# ============================================================

# 0) Load required packages
library(tidyverse)
library(forecast)   # native forecasting functions: auto.arima, forecast, etc.
library(lubridate)
library(ggplot2)
library(scales)
library(purrr)

# ------------------------------------------------------------
# 1) Filter panel_clean to target occupations and years
# ------------------------------------------------------------
# Target occupations:
#   311014, 292061, 291141, 291171


# Define local directory for raw data assets
datapath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/data/"
outputpath <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/assignment/output/"

# Update the path if the file is stored somewhere else.
panel <- readr::read_csv(paste0(datapath,"sd_oews_panel.csv"), show_col_types = FALSE)


panel_clean <- panel %>%
  mutate(
    year     = as.integer(year),
    occ_code = as.character(occ_code),
    occ_name = as.character(occ_name),
    tot_emp  = as.numeric(tot_emp),
    wage_avg = as.numeric(wage_avg),
    flag     = as.integer(flag)
  ) %>%
  filter(
    flag != 1,
    !occ_code %in% c("290000", "310000")
  ) %>%
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



target_codes <- c("311131", "292061", "291141", "291171")

hist_df <- panel_clean %>%
  filter(
    occ_code %in% target_codes,
    year >= 2016,
    year <= 2025
  ) %>%
  select(year, occ_code, occ_name, tot_emp) %>%
  arrange(occ_code, year)

# ------------------------------------------------------------
# 2) Helper: forecast employment for one occupation
# ------------------------------------------------------------
# Note: we pass occ_code and occ_name in from group_modify,
# so we never have to re-read unknown columns inside the helper.

# We fit a log-linear model:
#   log(tot_emp) = a + b * year + e
# and then exponentiate predictions for 2016–2030.

forecast_one_occ_trend_indexed <- function(df_occ) {
  df_occ <- df_occ %>% arrange(year)
  
  occ_code_val <- df_occ$occ_code[1]
  occ_name_val <- df_occ$occ_name[1]
  
  # Fit log-linear regression on positive employment
  df_fit <- df_occ %>%
    filter(!is.na(tot_emp), tot_emp > 0)
  
  mod <- lm(log(tot_emp) ~ year, data = df_fit)
  
  # Full year range 2016–2030
  years_full <- 2016:2030
  
  df_all_years <- tibble(
    year     = years_full,
    occ_code = occ_code_val,
    occ_name = occ_name_val
  ) %>%
    mutate(
      log_pred = predict(mod, newdata = tibble(year = year)),
      tot_emp  = exp(log_pred),
      type     = if_else(year <= 2025, "actual", "forecast")
    ) %>%
    # Replace 2016–2025 with actuals when available
    left_join(
      df_occ %>% select(year, tot_emp_actual = tot_emp),
      by = "year"
    ) %>%
    mutate(
      tot_emp = if_else(!is.na(tot_emp_actual), tot_emp_actual, tot_emp)
    ) %>%
    select(year, occ_code, occ_name, tot_emp, type)
  
  # Index to 2016 level = 1
  base_2016 <- df_all_years %>%
    filter(year == 2016) %>%
    pull(tot_emp)
  
  df_all_years %>%
    mutate(
      index_2016 = tot_emp / base_2016   # 1 in 2016
      # if you prefer percent, use: index_2016 = 100 * tot_emp / base_2016
    )
}

# ------------------------------------------------------------
# 3) Apply indexed trend forecasts per occupation
# ------------------------------------------------------------

forecast_df <- hist_df %>%
  split(.$occ_code) %>%
  purrr::map_dfr(forecast_one_occ_trend_indexed)

# ------------------------------------------------------------
# 4) Prepare plotting data with colors and linetype flag
# ------------------------------------------------------------

plot_df <- forecast_df %>%
  mutate(
    is_forecast = (year > 2025),
    # map codes to labels
    occ_label = case_when(
      occ_code == "311131" ~ occ_name,
      occ_code == "292061" ~ occ_name,
      occ_code == "291141" ~ occ_name,
      occ_code == "291171" ~ occ_name,
      TRUE                 ~ occ_name
    ),
    color_hex = case_when(
      occ_code == "311131" ~ "#06B7DF",
      occ_code == "292061" ~ "#F69554",
      occ_code == "291141" ~ "#4978BC",
      occ_code == "291171" ~ "#373F51",
      TRUE                 ~ "#999999"
    )
  ) %>%
  # enforce legend order by occ_code
  mutate(
    occ_label = factor(
      occ_label,
      levels = plot_df %>%
        distinct(occ_code, occ_name) %>%
        filter(occ_code %in% c("311131", "292061", "291141", "291171")) %>%
        arrange(factor(occ_code, levels = c("311131", "292061", "291141", "291171"))) %>%
        pull(occ_name)
    )
  )


color_map <- plot_df %>%
  distinct(occ_label, color_hex) %>%
  deframe()

# ------------------------------------------------------------
# 5) Line chart: indexed to 2016 = 1 (or 100)
# ------------------------------------------------------------

employment_forecast_plot <- ggplot(
  plot_df,
  aes(
    x     = year,
    y     = index_2016,
    group = occ_label,
    color = occ_label
  )
) +
  geom_line(
    data = ~ filter(.x, !is_forecast),
    linewidth = 1
  ) +
  geom_line(
    data = ~ filter(.x, is_forecast),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(
    data = ~ filter(.x, !is_forecast),
    size = 1.5
  ) +
  geom_point(
    data = ~ filter(.x, is_forecast),
    size  = 1.5,
    shape = 1
  ) +
  scale_color_manual(
    name   = "Occupation",
    values = color_map,
    breaks = levels(plot_df$occ_label)   # ensures legend order
  ) +
  scale_x_continuous(
    breaks = seq(2016, 2030, by = 2),
    minor_breaks = 2016:2030,
    name = "Year"
  ) +
  scale_y_continuous(
    # if index_2016 is 1 in 2016:
    labels = percent_format(accuracy = 1),
    name   = "Employment index (2016 = 100%)"
    # if you used 100 * tot_emp/base_2016, instead use:
    # labels = label_number(accuracy = 1),
    # name   = "Employment index (2016 = 100)"
  ) +
  labs(
    title    = "Indexed Historical and Forecasted Employment by Occupation",
    subtitle = "Values indexed to 2016 = 100%; actual 2016–2025 (solid), trend forecasts 2026–2030 (dashed)",
    caption  = "Log-linear trend model fitted on 2016–2025 employment by occupation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

print(employment_forecast_plot)



ggsave(
  filename = paste0(outputpath, "employment_forecasts_2016_2030.png"),
  plot     = employment_forecast_plot,
  width    = 10,
  height   = 6,
  dpi      = 300
)