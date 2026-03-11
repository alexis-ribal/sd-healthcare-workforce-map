# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Load the dataset
path <- "~/Library/CloudStorage/Dropbox/Jobs/2026/San Diego Workforce Partnership/portfolio/dashboards/ces_projections/"

ces_data <- read_excel(paste0(path,"ces_san_2014_2025.xlsx"))

# 2. Create clean dataframe with current employment data
# Filter for non-seasonally adjusted data (N) to avoid duplication
clean_data <- ces_data %>%
  filter(`Seasonally Adjusted (Y/N)` == "N") %>%
  select(`Series Code`, `Industry Title`, Year, Month, `Current Employment`) %>%
  mutate(`Current Employment` = as.numeric(`Current Employment`))

# 3. Compute annual averages by Series Code and Industry Title
annual_avg <- clean_data %>%
  group_by(Year, `Series Code`, `Industry Title`) %>%
  summarise(
    Annual_Employment = mean(`Current Employment`, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Prepare for linear projection (2026-2030)
# Get unique series codes and industry titles
sectors <- annual_avg %>%
  select(`Series Code`, `Industry Title`) %>%
  distinct()

# Function to project employment for each sector
project_employment <- function(sector_code, sector_name, data) {
  # Filter data for this specific sector
  sector_data <- data %>%
    filter(`Series Code` == sector_code, `Industry Title` == sector_name)
  
  # Fit linear model
  model <- lm(Annual_Employment ~ Year, data = sector_data)
  
  # Create projection years (2026-2030)
  future_years <- data.frame(Year = 2026:2030)
  
  # Predict future values
  predictions <- predict(model, newdata = future_years)
  
  # Create output dataframe
  projections <- data.frame(
    Year = future_years$Year,
    `Series Code` = sector_code,
    `Industry Title` = sector_name,
    Annual_Employment = predictions,
    check.names = FALSE
  )
  
  return(projections)
}

# 5. Apply projection function to all sectors
all_projections <- sectors %>%
  rowwise() %>%
  do(project_employment(.$`Series Code`, .$`Industry Title`, annual_avg)) %>%
  ungroup()

# 6. Combine historical annual averages with projections
final_data <- bind_rows(
  annual_avg %>% mutate(Data_Type = "Historical"),
  all_projections %>% mutate(Data_Type = "Projection")
)


# Display summary
cat("\n=== Summary Statistics ===\n")
cat("Total sectors:", nrow(sectors), "\n")
cat("Historical years:", paste(sort(unique(annual_avg$Year)), collapse = ", "), "\n")
cat("Projection years: 2026-2030\n")
cat("Output file: san_diego_employment_projections_2014_2030.csv\n")

# Display sample projections for major sectors
sample_sectors <- c("00000000", "30000000", "60000000", "70000000", "90000000")
sample_names <- c("Total Nonfarm", "Manufacturing", "Professional and Business Services", 
                  "Leisure and Hospitality", "Government")

# Sort the data by Year and Industry Title
final_data <- final_data %>%
  arrange(`Industry Title`, Year)



# Define sectors of interest
selected_sectors <- c("Financial Activities", 
                      "Department Stores",
                      "Food and Beverage Retailers",
                      "Health Care and Social Assistance")

# Filter data for selected sectors
plot_data <- final_data %>%
  filter(`Industry Title` %in% selected_sectors)

# Create the plot
ggplot(plot_data, aes(x = Year, y = Annual_Employment, color = `Industry Title`)) +
  geom_line(aes(linetype = Data_Type), size = 1) +
  geom_point(aes(shape = Data_Type), size = 2) +
  scale_linetype_manual(values = c("Historical" = "solid", "Projection" = "dashed")) +
  scale_shape_manual(values = c("Historical" = 16, "Projection" = 17)) +
  geom_vline(xintercept = 2025.5, linetype = "dotted", color = "gray50", size = 0.8) +
  labs(
    title = "San Diego Employment Trends: Historical and Projected",
    subtitle = "Selected Sectors (2014-2030)",
    x = "Year",
    y = "Annual Employment",
    color = "Industry Sector",
    linetype = "Data Type",
    shape = "Data Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.box = "vertical",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(2014, 2030, 2)) +
  scale_y_continuous(labels = scales::comma)







# Calculate average employment for different periods by sector
period_comparison <- annual_avg %>%
  mutate(Period = case_when(
    Year >= 2014 & Year <= 2019 ~ "2014-2019",
    Year >= 2020 & Year <= 2025 ~ "2020-2025",
    TRUE ~ "Other"
  )) %>%
  filter(Period != "Other") %>%
  group_by(`Series Code`, `Industry Title`, Period) %>%
  summarise(Avg_Employment = mean(Annual_Employment, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Period,
    values_from = Avg_Employment,
    names_prefix = "Avg_"
  )

# Calculate percent change for each period
period_comparison <- period_comparison %>%
  mutate(
    Pct_Change_2014_2019 = ((`Avg_2014-2019` - lag(`Avg_2014-2019`, default = first(`Avg_2014-2019`))) / 
                              lag(`Avg_2014-2019`, default = first(`Avg_2014-2019`))) * 100,
    Pct_Change_2020_2025 = ((`Avg_2020-2025` - `Avg_2014-2019`) / `Avg_2014-2019`) * 100,
    Pct_Change_Pre_Post = ((`Avg_2020-2025` - `Avg_2014-2019`) / `Avg_2014-2019`) * 100
  )

# Recalculate properly: percent change WITHIN each period
period_change <- annual_avg %>%
  group_by(`Series Code`, `Industry Title`) %>%
  summarise(
    Emp_2014 = if(any(Year == 2014)) Annual_Employment[Year == 2014][1] else NA_real_,
    Emp_2019 = if(any(Year == 2019)) Annual_Employment[Year == 2019][1] else NA_real_,
    Emp_2020 = if(any(Year == 2020)) Annual_Employment[Year == 2020][1] else NA_real_,
    Emp_2025 = if(any(Year == 2025)) Annual_Employment[Year == 2025][1] else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    Pct_Change_2014_2019 = ((Emp_2019 - Emp_2014) / Emp_2014) * 100,
    Pct_Change_2020_2025 = ((Emp_2025 - Emp_2020) / Emp_2020) * 100,
    Avg_Employment = (Emp_2014 + Emp_2019 + Emp_2020 + Emp_2025) / 4
  ) %>%
  filter(!is.na(Pct_Change_2014_2019) & !is.na(Pct_Change_2020_2025) &
           !is.infinite(Pct_Change_2014_2019) & !is.infinite(Pct_Change_2020_2025) &
           !is.na(Avg_Employment))

# Remove aggregate/total industry titles from period_change
aggregate_titles <- c("Total Nonfarm", "Total, All Industries", "Total Private", 
                      "Goods Producing", "Service-Providing", "Private Service Providing", "Warehousing and Storage")

period_change <- period_change %>%
  filter(!`Industry Title` %in% aggregate_titles)
  

# Create scatterplot
ggplot(period_change, aes(x = Pct_Change_2014_2019, y = Pct_Change_2020_2025)) +
  geom_point(aes(size = Avg_Employment, color = Avg_Employment), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "red", size = 0.5) +
  scale_color_gradient(low = "lightblue", high = "darkblue", labels = scales::comma) +
  scale_size_continuous(range = c(1, 10), labels = scales::comma) +
  labs(
    title = "San Diego Employment Growth: Pre-Pandemic vs Post-Pandemic",
    subtitle = "Percent change by sector (2014-2019 vs 2020-2025)",
    x = "% Change in Employment (2014-2019)",
    y = "% Change in Employment (2020-2025)",
    size = "Avg Employment",
    color = "Avg Employment",
    caption = "Note: Bubble size represents average employment across all years.\nRed diagonal line indicates equal growth in both periods."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "right",
    plot.caption = element_text(hjust = 0, size = 9, color = "gray50")
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))




# Identify and label notable sectors
notable_sectors <- period_change %>%
  filter(abs(Pct_Change_2014_2019) > 20 | abs(Pct_Change_2020_2025) > 20 |
           Avg_Employment > quantile(Avg_Employment, 0.9, na.rm = TRUE))

notable_sectors <- notable_sectors %>%
  filter(!`Industry Title` %in% c("Total Nonfarm", "Total, All Industries", "Total Private", 
                                  "Goods Producing", "Service-Providing", "Private Service Providing",
                                  "Warehousing and Storage"))

# Create enhanced scatterplot with labels
library(ggrepel)


ggplot(period_change, aes(x = Pct_Change_2014_2019, y = Pct_Change_2020_2025)) +
  geom_point(aes(size = Avg_Employment, color = Avg_Employment), alpha = 0.6) +
  geom_text_repel(
    data = notable_sectors,
    aes(label = `Industry Title`),
    size = 3,
    max.overlaps = 15,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "red", size = 0.5) +
  scale_color_gradient(low = "lightblue", high = "darkblue", labels = scales::comma) +
  scale_size_continuous(range = c(1, 10), labels = scales::comma) +
  labs(
    title = "San Diego Employment Growth: Pre-Pandemic vs Post-Pandemic Recovery",
    subtitle = "Percent change by sector (2014-2019 vs 2020-2025) - Notable sectors labeled",
    x = "% Change in Employment (2014-2019)",
    y = "% Change in Employment (2020-2025)",
    size = "Avg Employment",
    color = "Avg Employment",
    caption = "Note: Bubble size represents average employment. Red diagonal = equal growth in both periods.\nQuadrants: Upper-right = growth in both periods; Lower-left = decline in both periods.\nSource: Alexis Rivera based on California Current Employment Statistics (CES)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "right",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray50")
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))



# Print summary statistics
cat("\n=== Employment Growth Period Comparison ===\n")
cat(sprintf("Total sectors analyzed: %d\n", nrow(period_change)))
cat(sprintf("\nPre-pandemic growth (2014-2019): %.1f%% median\n", 
            median(period_change$Pct_Change_2014_2019, na.rm = TRUE)))
cat(sprintf("Post-pandemic period (2020-2025): %.1f%% median\n", 
            median(period_change$Pct_Change_2020_2025, na.rm = TRUE)))

# Quadrant analysis
cat("\n=== Quadrant Analysis ===\n")
cat(sprintf("Growth in both periods: %d sectors (%.1f%%)\n",
            sum(period_change$Pct_Change_2014_2019 > 0 & period_change$Pct_Change_2020_2025 > 0),
            mean(period_change$Pct_Change_2014_2019 > 0 & period_change$Pct_Change_2020_2025 > 0) * 100))
cat(sprintf("Decline in both periods: %d sectors (%.1f%%)\n",
            sum(period_change$Pct_Change_2014_2019 < 0 & period_change$Pct_Change_2020_2025 < 0),
            mean(period_change$Pct_Change_2014_2019 < 0 & period_change$Pct_Change_2020_2025 < 0) * 100))
cat(sprintf("Pre-pandemic growth, post-pandemic decline: %d sectors (%.1f%%)\n",
            sum(period_change$Pct_Change_2014_2019 > 0 & period_change$Pct_Change_2020_2025 < 0),
            mean(period_change$Pct_Change_2014_2019 > 0 & period_change$Pct_Change_2020_2025 < 0) * 100))
cat(sprintf("Pre-pandemic decline, post-pandemic growth: %d sectors (%.1f%%)\n",
            sum(period_change$Pct_Change_2014_2019 < 0 & period_change$Pct_Change_2020_2025 > 0),
            mean(period_change$Pct_Change_2014_2019 < 0 & period_change$Pct_Change_2020_2025 > 0) * 100))

# Top gainers and losers
cat("\n=== Top 10 Growth Sectors (2020-2025) ===\n")
top_gainers <- period_change %>%
  arrange(desc(Pct_Change_2020_2025)) %>%
  head(10) %>%
  select(`Industry Title`, Pct_Change_2014_2019, Pct_Change_2020_2025, Avg_Employment)
print(top_gainers)

cat("\n=== Top 10 Declining Sectors (2020-2025) ===\n")
top_losers <- period_change %>%
  arrange(Pct_Change_2020_2025) %>%
  head(10) %>%
  select(`Industry Title`, Pct_Change_2014_2019, Pct_Change_2020_2025, Avg_Employment)
print(top_losers)



# ==== PERIOD COMPARISON: 2020-2025 vs 2026-2030 ====

# 1. Build annual averages including projections (already in final_data)
#    We assume final_data has: Year, Series Code, Industry Title,
#    Annual_Employment, Data_Type = "Historical"/"Projection"

# 2. Extract key endpoint employment levels by sector
period_change_proj <- final_data %>%
  group_by(`Series Code`, `Industry Title`) %>%
  summarise(
    Emp_2020 = if (any(Year == 2020 & Data_Type == "Historical"))
      Annual_Employment[Year == 2020 & Data_Type == "Historical"][1] else NA_real_,
    Emp_2025 = if (any(Year == 2025 & Data_Type == "Historical"))
      Annual_Employment[Year == 2025 & Data_Type == "Historical"][1] else NA_real_,
    Emp_2026 = if (any(Year == 2026 & Data_Type == "Projection"))
      Annual_Employment[Year == 2026 & Data_Type == "Projection"][1] else NA_real_,
    Emp_2030 = if (any(Year == 2030 & Data_Type == "Projection"))
      Annual_Employment[Year == 2030 & Data_Type == "Projection"][1] else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    Pct_Change_2020_2025 = ((Emp_2025 - Emp_2020) / Emp_2020) * 100,
    Pct_Change_2026_2030 = ((Emp_2030 - Emp_2026) / Emp_2026) * 100,
    Avg_Employment_2020_2030 = rowMeans(cbind(Emp_2020, Emp_2025, Emp_2026, Emp_2030),
                                        na.rm = TRUE)
  ) %>%
  filter(
    !is.na(Pct_Change_2020_2025), !is.na(Pct_Change_2026_2030),
    !is.infinite(Pct_Change_2020_2025), !is.infinite(Pct_Change_2026_2030),
    !is.na(Avg_Employment_2020_2030)
  )


# 3. Apply the same sector filters used before (aggregates + warehousing)
aggregate_titles <- c("Total Nonfarm", "Total, All Industries", "Total Private", 
                      "Goods Producing", "Service-Providing", "Private Service Providing",
                      "Warehousing and Storage", "Newspaper, Periodical, Book, and Directory Publishers")

period_change_proj <- period_change_proj %>%
  filter(!`Industry Title` %in% aggregate_titles)


# 4. Define notable sectors for labeling using the SAME criteria as before
notable_sectors_proj <- period_change_proj %>%
  filter(
    abs(Pct_Change_2020_2025) > 20 |
      abs(Pct_Change_2026_2030) > 20 |
      Avg_Employment_2020_2030 >
      quantile(Avg_Employment_2020_2030, 0.9, na.rm = TRUE)
  )

notable_sectors_proj <- notable_sectors_proj %>%
  filter(!`Industry Title` %in% c("Total Nonfarm", "Total, All Industries", "Total Private", 
                                  "Goods Producing", "Service-Providing", "Private Service Providing",
                                  "Warehousing and Storage", "Newspaper, Periodical, Book, and Directory Publishers"))


# 6. Labeled version using same notable-sector logic

ggplot(period_change_proj,
       aes(x = Pct_Change_2020_2025, y = Pct_Change_2026_2030)) +
  geom_point(aes(size = Avg_Employment_2020_2030,
                 color = Avg_Employment_2020_2030),
             alpha = 0.6) +
  geom_text_repel(
    data = notable_sectors_proj,
    aes(label = `Industry Title`),
    size = 3,
    max.overlaps = 15,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted",
              color = "red", linewidth = 0.5) +
  scale_color_gradient(low = "lightblue", high = "darkblue",
                       labels = scales::comma) +
  scale_size_continuous(range = c(1, 10), labels = scales::comma) +
  labs(
    title = "San Diego Employment Growth: Current vs Projected",
    subtitle = "2020-2025 actual vs 2026-2030 projected growth by sector\nNotable sectors labeled",
    x = "% Change in Employment (2020-2025)",
    y = "% Change in Employment (2026-2030, projected)",
    size = "Avg Employment",
    color = "Avg Employment",
    caption = "Quadrants: upper-right = strong recent and projected growth; lower-right = recent growth but projected slowdown/decline; upper-left = weak recent but strong projected growth.\nSource: Alexis Rivera based on California Current Employment Statistics (CES)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "gray30"),
    legend.position = "right",
    plot.caption = element_text(hjust = 0, size = 8, colour = "gray50")
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))



# ==== MERGE period_change (2014-2019 vs 2020-2025)
#      AND period_change_proj (2020-2025 vs 2026-2030) ====

# 1. Select and rename key columns from each dataframe
period_change_export <- period_change %>%
  select(`Series Code`, `Industry Title`,
         Pct_Change_2014_2019,
         Pct_Change_2020_2025,
         Avg_Employment) %>%
  rename(
    Pct_Change_2014_2019 = Pct_Change_2014_2019,
    Pct_Change_2020_2025_hist = Pct_Change_2020_2025,
    Avg_Employment_2014_2025 = Avg_Employment
  )

period_change_proj_export <- period_change_proj %>%
  select(`Series Code`, `Industry Title`,
         Pct_Change_2020_2025,
         Pct_Change_2026_2030,
         Avg_Employment_2020_2030)

# 2. Merge by Series Code and Industry Title
period_change_full <- period_change_export %>%
  full_join(period_change_proj_export,
            by = c("Series Code", "Industry Title"))

# 3. Optional: reorder columns for readability
period_change_full <- period_change_full %>%
  select(
    `Series Code`, `Industry Title`,
    Pct_Change_2014_2019,
    Pct_Change_2020_2025_hist,
    Pct_Change_2020_2025,
    Pct_Change_2026_2030,
    Avg_Employment_2014_2025,
    Avg_Employment_2020_2030
  )

# 4. Export to CSV
write.csv(period_change_full,
          paste0(path,"period_change_full.csv"),
          row.names = FALSE)

