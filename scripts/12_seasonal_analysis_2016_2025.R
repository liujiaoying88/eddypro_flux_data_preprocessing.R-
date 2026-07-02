# ============================================================
# 12_seasonal_analysis_2016_2025.R
# Seasonal Analysis of Carbon Fluxes and Environmental Drivers
# Site: MukaHead / CEMACS, Penang, Malaysia
# Author: Cai Xiaoliang
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

base_path <- "/Users/caixiaoliang/Documents"

daily_file <- file.path(base_path, "08_daily_partitioned_flux_2016_2025.csv")
monthly_file <- file.path(base_path, "09_monthly_flux_budget_2016_2025.csv")

seasonal_daily_output_file <- file.path(base_path, "12_seasonal_daily_summary_2016_2025.csv")
seasonal_monthly_output_file <- file.path(base_path, "12_seasonal_monthly_summary_2016_2025.csv")
seasonal_comparison_output_file <- file.path(base_path, "12_seasonal_comparison_tests_2016_2025.csv")
seasonal_yearly_output_file <- file.path(base_path, "12_seasonal_yearly_summary_2016_2025.csv")

# =========================
# 1. Read daily data
# =========================

daily_flux <- read_csv(
  daily_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

required_daily_cols <- c(
  "Year", "Date",
  "NEE_daily", "GPP_daily", "Reco_daily",
  "Rg_daily", "Tair_daily", "VPD_daily"
)

missing_daily_cols <- setdiff(required_daily_cols, names(daily_flux))

if (length(missing_daily_cols) > 0) {
  cat("\nMissing daily columns:\n")
  print(missing_daily_cols)
  stop("Required daily columns are missing.")
}

# =========================
# 2. Add seasonal classification
# =========================
# Penang / Muka Head working classification:
# Dry: Dec, Jan, Feb
# Wet: May, Jun, Jul, Aug, Sep, Oct
# Transition: Mar, Apr, Nov

daily_flux <- daily_flux %>%
  mutate(
    Year = as.integer(Year),
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
      levels = c("Dry", "Transition", "Wet")
    ),

    NEE_daily = as.numeric(NEE_daily),
    GPP_daily = as.numeric(GPP_daily),
    Reco_daily = as.numeric(Reco_daily),
    Rg_daily = as.numeric(Rg_daily),
    Tair_daily = as.numeric(Tair_daily),
    VPD_daily = as.numeric(VPD_daily)
  ) %>%
  filter(
    Year >= 2016,
    Year <= 2025,
    !is.na(Season)
  )

cat("\n===== Daily season distribution =====\n")
print(table(daily_flux$Season))

cat("\n===== Daily season distribution by year =====\n")
print(table(daily_flux$Year, daily_flux$Season))

# =========================
# 3. Seasonal daily summary
# =========================

seasonal_daily_summary <- daily_flux %>%
  group_by(Season) %>%
  summarise(
    Days = n(),

    Valid_NEE_days = sum(!is.na(NEE_daily)),
    Valid_GPP_days = sum(!is.na(GPP_daily)),
    Valid_Reco_days = sum(!is.na(Reco_daily)),

    Valid_NEE_percent = round(Valid_NEE_days / Days * 100, 2),
    Valid_GPP_percent = round(Valid_GPP_days / Days * 100, 2),
    Valid_Reco_percent = round(Valid_Reco_days / Days * 100, 2),

    NEE_mean = mean(NEE_daily, na.rm = TRUE),
    NEE_sd = sd(NEE_daily, na.rm = TRUE),
    NEE_median = median(NEE_daily, na.rm = TRUE),

    GPP_mean = mean(GPP_daily, na.rm = TRUE),
    GPP_sd = sd(GPP_daily, na.rm = TRUE),
    GPP_median = median(GPP_daily, na.rm = TRUE),

    Reco_mean = mean(Reco_daily, na.rm = TRUE),
    Reco_sd = sd(Reco_daily, na.rm = TRUE),
    Reco_median = median(Reco_daily, na.rm = TRUE),

    Rg_mean = mean(Rg_daily, na.rm = TRUE),
    Rg_sd = sd(Rg_daily, na.rm = TRUE),

    Tair_mean = mean(Tair_daily, na.rm = TRUE),
    Tair_sd = sd(Tair_daily, na.rm = TRUE),

    VPD_mean = mean(VPD_daily, na.rm = TRUE),
    VPD_sd = sd(VPD_daily, na.rm = TRUE),

    .groups = "drop"
  )

cat("\n===== Seasonal daily summary =====\n")
print(seasonal_daily_summary)

write_csv(seasonal_daily_summary, seasonal_daily_output_file, na = "NA")

# =========================
# 4. Seasonal yearly daily summary
# =========================

seasonal_yearly_summary <- daily_flux %>%
  group_by(Year, Season) %>%
  summarise(
    Days = n(),

    Valid_NEE_days = sum(!is.na(NEE_daily)),
    Valid_GPP_days = sum(!is.na(GPP_daily)),
    Valid_Reco_days = sum(!is.na(Reco_daily)),

    Valid_NEE_percent = round(Valid_NEE_days / Days * 100, 2),
    Valid_GPP_percent = round(Valid_GPP_days / Days * 100, 2),
    Valid_Reco_percent = round(Valid_Reco_days / Days * 100, 2),

    NEE_mean = ifelse(Valid_NEE_percent >= 50, mean(NEE_daily, na.rm = TRUE), NA_real_),
    GPP_mean = ifelse(Valid_GPP_percent >= 50, mean(GPP_daily, na.rm = TRUE), NA_real_),
    Reco_mean = ifelse(Valid_Reco_percent >= 50, mean(Reco_daily, na.rm = TRUE), NA_real_),

    Rg_mean = mean(Rg_daily, na.rm = TRUE),
    Tair_mean = mean(Tair_daily, na.rm = TRUE),
    VPD_mean = mean(VPD_daily, na.rm = TRUE),

    .groups = "drop"
  )

cat("\n===== Seasonal yearly summary =====\n")
print(seasonal_yearly_summary, n = Inf)

write_csv(seasonal_yearly_summary, seasonal_yearly_output_file, na = "NA")

# =========================
# 5. Read monthly budget data
# =========================

monthly_flux <- read_csv(
  monthly_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

required_monthly_cols <- c(
  "Year", "Month",
  "NEE_mean", "GPP_mean", "Reco_mean",
  "NEE_sum_gC", "GPP_sum_gC", "Reco_sum_gC",
  "Rg_mean", "Tair_mean", "VPD_mean"
)

missing_monthly_cols <- setdiff(required_monthly_cols, names(monthly_flux))

if (length(missing_monthly_cols) > 0) {
  cat("\nMissing monthly columns:\n")
  print(missing_monthly_cols)
  stop("Required monthly columns are missing.")
}

monthly_flux <- monthly_flux %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month),

    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Dry",
      Month %in% c(5, 6, 7, 8, 9, 10) ~ "Wet",
      Month %in% c(3, 4, 11) ~ "Transition",
      TRUE ~ NA_character_
    ),

    Season = factor(
      Season,
      levels = c("Dry", "Transition", "Wet")
    )
  ) %>%
  filter(
    Year >= 2016,
    Year <= 2025,
    !is.na(Season)
  )

# =========================
# 6. Seasonal monthly carbon budget summary
# =========================

seasonal_monthly_summary <- monthly_flux %>%
  group_by(Season) %>%
  summarise(
    Months = n(),

    Valid_NEE_months = sum(!is.na(NEE_sum_gC)),
    Valid_GPP_months = sum(!is.na(GPP_sum_gC)),
    Valid_Reco_months = sum(!is.na(Reco_sum_gC)),

    Seasonal_NEE_sum_gC = sum(NEE_sum_gC, na.rm = TRUE),
    Seasonal_GPP_sum_gC = sum(GPP_sum_gC, na.rm = TRUE),
    Seasonal_Reco_sum_gC = sum(Reco_sum_gC, na.rm = TRUE),

    Seasonal_NEE_mean = mean(NEE_mean, na.rm = TRUE),
    Seasonal_GPP_mean = mean(GPP_mean, na.rm = TRUE),
    Seasonal_Reco_mean = mean(Reco_mean, na.rm = TRUE),

    Seasonal_Rg_mean = mean(Rg_mean, na.rm = TRUE),
    Seasonal_Tair_mean = mean(Tair_mean, na.rm = TRUE),
    Seasonal_VPD_mean = mean(VPD_mean, na.rm = TRUE),

    .groups = "drop"
  )

cat("\n===== Seasonal monthly carbon budget summary =====\n")
print(seasonal_monthly_summary)

write_csv(seasonal_monthly_summary, seasonal_monthly_output_file, na = "NA")

# =========================
# 7. Seasonal comparison tests
# =========================

run_kruskal <- function(data, response_var) {
  d <- data %>%
    select(Season, all_of(response_var)) %>%
    drop_na()

  if (nrow(d) < 10 || length(unique(d$Season)) < 2) {
    return(
      tibble(
        Variable = response_var,
        Test = "Kruskal-Wallis",
        Statistic = NA_real_,
        DF = NA_real_,
        P_value = NA_real_,
        n = nrow(d)
      )
    )
  }

  test_result <- kruskal.test(
    as.formula(paste(response_var, "~ Season")),
    data = d
  )

  tibble(
    Variable = response_var,
    Test = "Kruskal-Wallis",
    Statistic = unname(test_result$statistic),
    DF = unname(test_result$parameter),
    P_value = test_result$p.value,
    n = nrow(d)
  )
}

seasonal_tests <- bind_rows(
  run_kruskal(daily_flux, "NEE_daily"),
  run_kruskal(daily_flux, "GPP_daily"),
  run_kruskal(daily_flux, "Reco_daily"),
  run_kruskal(daily_flux, "Rg_daily"),
  run_kruskal(daily_flux, "Tair_daily"),
  run_kruskal(daily_flux, "VPD_daily")
)

cat("\n===== Seasonal comparison tests =====\n")
print(seasonal_tests)

write_csv(seasonal_tests, seasonal_comparison_output_file, na = "NA")

# =========================
# 8. Plot seasonal daily carbon flux boxplot
# =========================

daily_flux_long <- daily_flux %>%
  select(
    Season,
    NEE_daily,
    GPP_daily,
    Reco_daily
  ) %>%
  pivot_longer(
    cols = c(NEE_daily, GPP_daily, Reco_daily),
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
    title = "Seasonal Distribution of Daily Carbon Fluxes (2016–2025)",
    x = "Season",
    y = expression(Daily~mean~flux~(mu*mol~m^{-2}~s^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

print(p1)

ggsave(
  file.path(base_path, "12_seasonal_daily_carbon_flux_boxplot_2016_2025.png"),
  p1,
  width = 12,
  height = 7,
  dpi = 300
)

# =========================
# 9. Plot seasonal environmental drivers boxplot
# =========================

daily_env_long <- daily_flux %>%
  select(
    Season,
    Rg_daily,
    Tair_daily,
    VPD_daily
  ) %>%
  pivot_longer(
    cols = c(Rg_daily, Tair_daily, VPD_daily),
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
    title = "Seasonal Distribution of Environmental Drivers (2016–2025)",
    x = "Season",
    y = "Daily mean value"
  ) +
  theme(
    legend.position = "none"
  )

print(p2)

ggsave(
  file.path(base_path, "12_seasonal_environmental_drivers_boxplot_2016_2025.png"),
  p2,
  width = 12,
  height = 7,
  dpi = 300
)

# =========================
# 10. Plot seasonal carbon budget
# =========================

seasonal_budget_long <- seasonal_monthly_summary %>%
  select(
    Season,
    Seasonal_NEE_sum_gC,
    Seasonal_GPP_sum_gC,
    Seasonal_Reco_sum_gC
  ) %>%
  pivot_longer(
    cols = c(
      Seasonal_NEE_sum_gC,
      Seasonal_GPP_sum_gC,
      Seasonal_Reco_sum_gC
    ),
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
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Seasonal Carbon Budget (2016–2025)",
    x = "Season",
    y = expression(g~C~m^{-2})
  )

print(p3)

ggsave(
  file.path(base_path, "12_seasonal_carbon_budget_2016_2025.png"),
  p3,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 11. Plot seasonal mean environmental drivers
# =========================

seasonal_env_long <- seasonal_monthly_summary %>%
  select(
    Season,
    Seasonal_Rg_mean,
    Seasonal_Tair_mean,
    Seasonal_VPD_mean
  ) %>%
  pivot_longer(
    cols = c(
      Seasonal_Rg_mean,
      Seasonal_Tair_mean,
      Seasonal_VPD_mean
    ),
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
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Seasonal Mean Environmental Drivers (2016–2025)",
    x = "Season",
    y = "Mean value"
  )

print(p4)

ggsave(
  file.path(base_path, "12_seasonal_mean_environmental_drivers_2016_2025.png"),
  p4,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 12. Finished
# =========================

saved_files <- c(
  seasonal_daily_output_file,
  seasonal_yearly_output_file,
  seasonal_monthly_output_file,
  seasonal_comparison_output_file,
  file.path(base_path, "12_seasonal_daily_carbon_flux_boxplot_2016_2025.png"),
  file.path(base_path, "12_seasonal_environmental_drivers_boxplot_2016_2025.png"),
  file.path(base_path, "12_seasonal_carbon_budget_2016_2025.png"),
  file.path(base_path, "12_seasonal_mean_environmental_drivers_2016_2025.png")
)

cat("\n=========================================\n")
cat("12 seasonal analysis completed successfully.\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
