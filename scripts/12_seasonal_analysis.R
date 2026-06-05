# =========================================
# 12 Seasonal Analysis of Carbon Fluxes and Environmental Drivers
# Site: MukaHead / CEMACS, Penang, Malaysia
# Author: Cai Xiaoliang
# Purpose:
#   Seasonal analysis of NEE, GPP, Reco and environmental drivers.
#
# Core rule:
#   12 reads saved CSV files only.
#   It does not depend on objects from previous scripts.
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

daily_file <- file.path(
  base_path,
  paste0("daily_flux_for_modeling_", analysis_year, ".csv")
)

monthly_file <- file.path(
  base_path,
  paste0("monthly_flux_", analysis_year, ".csv")
)

seasonal_daily_output_file <- file.path(
  base_path,
  paste0("seasonal_daily_summary_", analysis_year, ".csv")
)

seasonal_monthly_output_file <- file.path(
  base_path,
  paste0("seasonal_monthly_summary_", analysis_year, ".csv")
)

seasonal_comparison_output_file <- file.path(
  base_path,
  paste0("seasonal_comparison_tests_", analysis_year, ".csv")
)

# =========================================
# 1. Read daily data
# =========================================

daily_flux <- read_csv(
  daily_file,
  show_col_types = FALSE
)

required_daily_cols <- c(
  "Date",
  "Daily_NEE_mean",
  "Daily_GPP_mean",
  "Daily_Reco_mean",
  "Daily_Rg_mean",
  "Daily_Tair_mean",
  "Daily_VPD_mean"
)

missing_daily_cols <- setdiff(
  required_daily_cols,
  names(daily_flux)
)

if (length(missing_daily_cols) > 0) {
  stop(
    paste(
      "Missing required daily columns:",
      paste(missing_daily_cols, collapse = ", ")
    )
  )
}

# =========================================
# 2. Add seasonal classification
# =========================================
# Muka Head / Penang seasonal classification:
# Dry season:
#   Dec, Jan, Feb
# Wet season:
#   May, Jun, Jul, Aug, Sep, Oct
# Transition season:
#   Mar, Apr, Nov

daily_flux <- daily_flux %>%
  mutate(
    Date = as.Date(Date),
    Month_num = month(Date),
    Month = month(Date, label = TRUE, abbr = TRUE),
    Season = case_when(
      Month_num %in% c(12, 1, 2) ~ "Dry",
      Month_num %in% c(5, 6, 7, 8, 9, 10) ~ "Wet",
      Month_num %in% c(3, 4, 11) ~ "Transition",
      TRUE ~ NA_character_
    ),
    Season = factor(
      Season,
      levels = c(
        "Dry",
        "Transition",
        "Wet"
      )
    )
  )

cat("\n===== Daily season distribution =====\n")
print(
  table(daily_flux$Season)
)

# =========================================
# 3. Seasonal daily summary
# =========================================

seasonal_daily_summary <- daily_flux %>%
  group_by(Season) %>%
  summarise(
    Days = n(),

    NEE_mean = mean(Daily_NEE_mean, na.rm = TRUE),
    NEE_sd = sd(Daily_NEE_mean, na.rm = TRUE),
    NEE_median = median(Daily_NEE_mean, na.rm = TRUE),

    GPP_mean = mean(Daily_GPP_mean, na.rm = TRUE),
    GPP_sd = sd(Daily_GPP_mean, na.rm = TRUE),
    GPP_median = median(Daily_GPP_mean, na.rm = TRUE),

    Reco_mean = mean(Daily_Reco_mean, na.rm = TRUE),
    Reco_sd = sd(Daily_Reco_mean, na.rm = TRUE),
    Reco_median = median(Daily_Reco_mean, na.rm = TRUE),

    Rg_mean = mean(Daily_Rg_mean, na.rm = TRUE),
    Rg_sd = sd(Daily_Rg_mean, na.rm = TRUE),

    Tair_mean = mean(Daily_Tair_mean, na.rm = TRUE),
    Tair_sd = sd(Daily_Tair_mean, na.rm = TRUE),

    VPD_mean = mean(Daily_VPD_mean, na.rm = TRUE),
    VPD_sd = sd(Daily_VPD_mean, na.rm = TRUE),

    .groups = "drop"
  )

cat("\n===== Seasonal daily summary =====\n")
print(seasonal_daily_summary)

write_csv(
  seasonal_daily_summary,
  seasonal_daily_output_file
)

# =========================================
# 4. Read monthly data
# =========================================

monthly_flux <- read_csv(
  monthly_file,
  show_col_types = FALSE
)

required_monthly_cols <- c(
  "Month",
  "Monthly_NEE_mean",
  "Monthly_GPP_mean",
  "Monthly_Reco_mean",
  "Monthly_NEE_sum_gC",
  "Monthly_GPP_sum_gC",
  "Monthly_Reco_sum_gC",
  "Monthly_Rg_mean",
  "Monthly_Tair_mean",
  "Monthly_VPD_mean"
)

missing_monthly_cols <- setdiff(
  required_monthly_cols,
  names(monthly_flux)
)

if (length(missing_monthly_cols) > 0) {
  stop(
    paste(
      "Missing required monthly columns:",
      paste(missing_monthly_cols, collapse = ", ")
    )
  )
}

monthly_flux <- monthly_flux %>%
  mutate(
    Month_chr = as.character(Month),
    Month_num = match(
      Month_chr,
      month.abb
    ),
    Season = case_when(
      Month_num %in% c(12, 1, 2) ~ "Dry",
      Month_num %in% c(5, 6, 7, 8, 9, 10) ~ "Wet",
      Month_num %in% c(3, 4, 11) ~ "Transition",
      TRUE ~ NA_character_
    ),
    Season = factor(
      Season,
      levels = c(
        "Dry",
        "Transition",
        "Wet"
      )
    )
  )

# =========================================
# 5. Seasonal monthly carbon budget summary
# =========================================

seasonal_monthly_summary <- monthly_flux %>%
  group_by(Season) %>%
  summarise(
    Months = n(),

    Seasonal_NEE_sum_gC = sum(Monthly_NEE_sum_gC, na.rm = TRUE),
    Seasonal_GPP_sum_gC = sum(Monthly_GPP_sum_gC, na.rm = TRUE),
    Seasonal_Reco_sum_gC = sum(Monthly_Reco_sum_gC, na.rm = TRUE),

    Seasonal_NEE_mean = mean(Monthly_NEE_mean, na.rm = TRUE),
    Seasonal_GPP_mean = mean(Monthly_GPP_mean, na.rm = TRUE),
    Seasonal_Reco_mean = mean(Monthly_Reco_mean, na.rm = TRUE),

    Seasonal_Rg_mean = mean(Monthly_Rg_mean, na.rm = TRUE),
    Seasonal_Tair_mean = mean(Monthly_Tair_mean, na.rm = TRUE),
    Seasonal_VPD_mean = mean(Monthly_VPD_mean, na.rm = TRUE),

    .groups = "drop"
  )

cat("\n===== Seasonal monthly carbon budget summary =====\n")
print(seasonal_monthly_summary)

write_csv(
  seasonal_monthly_summary,
  seasonal_monthly_output_file
)

# =========================================
# 6. Seasonal comparison tests
# =========================================

run_kruskal <- function(data, response_var) {
  test_formula <- as.formula(
    paste(
      response_var,
      "~ Season"
    )
  )

  test_result <- kruskal.test(
    test_formula,
    data = data
  )

  tibble(
    Variable = response_var,
    Test = "Kruskal-Wallis",
    Statistic = unname(test_result$statistic),
    DF = unname(test_result$parameter),
    P_value = test_result$p.value
  )
}

seasonal_tests <- bind_rows(
  run_kruskal(
    daily_flux,
    "Daily_NEE_mean"
  ),
  run_kruskal(
    daily_flux,
    "Daily_GPP_mean"
  ),
  run_kruskal(
    daily_flux,
    "Daily_Reco_mean"
  ),
  run_kruskal(
    daily_flux,
    "Daily_Rg_mean"
  ),
  run_kruskal(
    daily_flux,
    "Daily_Tair_mean"
  ),
  run_kruskal(
    daily_flux,
    "Daily_VPD_mean"
  )
)

cat("\n===== Seasonal comparison tests =====\n")
print(seasonal_tests)

write_csv(
  seasonal_tests,
  seasonal_comparison_output_file
)

# =========================================
# 7. Plot seasonal daily carbon flux boxplot
# =========================================

daily_flux_long <- daily_flux %>%
  select(
    Season,
    Daily_NEE_mean,
    Daily_GPP_mean,
    Daily_Reco_mean
  ) %>%
  pivot_longer(
    cols = -Season,
    names_to = "Variable",
    values_to = "Value"
  )

p1 <- ggplot(
  daily_flux_long,
  aes(
    x = Season,
    y = Value,
    fill = Season
  )
) +
  geom_boxplot(
    alpha = 0.7,
    outlier.alpha = 0.4
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Seasonal Distribution of Daily Carbon Fluxes (", analysis_year, ")"),
    x = "Season",
    y = "Daily Mean Flux"
  ) +
  theme(
    legend.position = "none"
  )

print(p1)

ggsave(
  file.path(
    base_path,
    paste0("Seasonal_Daily_Carbon_Flux_Boxplot_", analysis_year, ".png")
  ),
  p1,
  width = 12,
  height = 7,
  dpi = 600
)

# =========================================
# 8. Plot seasonal environmental drivers boxplot
# =========================================

daily_env_long <- daily_flux %>%
  select(
    Season,
    Daily_Rg_mean,
    Daily_Tair_mean,
    Daily_VPD_mean
  ) %>%
  pivot_longer(
    cols = -Season,
    names_to = "Variable",
    values_to = "Value"
  )

p2 <- ggplot(
  daily_env_long,
  aes(
    x = Season,
    y = Value,
    fill = Season
  )
) +
  geom_boxplot(
    alpha = 0.7,
    outlier.alpha = 0.4
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Seasonal Distribution of Environmental Drivers (", analysis_year, ")"),
    x = "Season",
    y = "Daily Mean Value"
  ) +
  theme(
    legend.position = "none"
  )

print(p2)

ggsave(
  file.path(
    base_path,
    paste0("Seasonal_Environmental_Drivers_Boxplot_", analysis_year, ".png")
  ),
  p2,
  width = 12,
  height = 7,
  dpi = 600
)

# =========================================
# 9. Plot seasonal carbon budget bar chart
# =========================================

seasonal_budget_long <- seasonal_monthly_summary %>%
  select(
    Season,
    Seasonal_NEE_sum_gC,
    Seasonal_GPP_sum_gC,
    Seasonal_Reco_sum_gC
  ) %>%
  pivot_longer(
    cols = -Season,
    names_to = "Variable",
    values_to = "Value"
  )

p3 <- ggplot(
  seasonal_budget_long,
  aes(
    x = Season,
    y = Value,
    fill = Variable
  )
) +
  geom_col(
    position = "dodge"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Seasonal Carbon Budget (", analysis_year, ")"),
    x = "Season",
    y = "g C m-2 season-1"
  )

print(p3)

ggsave(
  file.path(
    base_path,
    paste0("Seasonal_Carbon_Budget_", analysis_year, ".png")
  ),
  p3,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 10. Plot seasonal mean environmental drivers
# =========================================

seasonal_env_long <- seasonal_monthly_summary %>%
  select(
    Season,
    Seasonal_Rg_mean,
    Seasonal_Tair_mean,
    Seasonal_VPD_mean
  ) %>%
  pivot_longer(
    cols = -Season,
    names_to = "Variable",
    values_to = "Value"
  )

p4 <- ggplot(
  seasonal_env_long,
  aes(
    x = Season,
    y = Value,
    fill = Variable
  )
) +
  geom_col(
    position = "dodge"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Seasonal Mean Environmental Drivers (", analysis_year, ")"),
    x = "Season",
    y = "Mean Value"
  )

print(p4)

ggsave(
  file.path(
    base_path,
    paste0("Seasonal_Mean_Environmental_Drivers_", analysis_year, ".png")
  ),
  p4,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 11. Save final file list
# =========================================

saved_files <- c(
  seasonal_daily_output_file,
  seasonal_monthly_output_file,
  seasonal_comparison_output_file,
  file.path(base_path, paste0("Seasonal_Daily_Carbon_Flux_Boxplot_", analysis_year, ".png")),
  file.path(base_path, paste0("Seasonal_Environmental_Drivers_Boxplot_", analysis_year, ".png")),
  file.path(base_path, paste0("Seasonal_Carbon_Budget_", analysis_year, ".png")),
  file.path(base_path, paste0("Seasonal_Mean_Environmental_Drivers_", analysis_year, ".png"))
)

cat("\n=========================================\n")
cat("12 seasonal analysis completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
