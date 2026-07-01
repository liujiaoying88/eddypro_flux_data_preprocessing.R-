# ============================================================
# 09_monthly_seasonal_carbon_budget_2016_2025.R
# Monthly, seasonal, wet/dry season and annual carbon budget analysis
# Site: Muka Head
# Author: Cai Xiaoliang
#
# Input:
#   08_daily_partitioned_flux_2016_2025.csv
#
# Core rule:
#   09 reads daily partitioned output from 08.
#   It does not run gap filling.
#   It does not run flux partitioning.
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

base_path <- "/Users/caixiaoliang/Documents"

input_file <- file.path(
  base_path,
  "08_daily_partitioned_flux_2016_2025.csv"
)

monthly_output_file <- file.path(
  base_path,
  "09_monthly_flux_budget_2016_2025.csv"
)

seasonal_output_file <- file.path(
  base_path,
  "09_seasonal_flux_budget_2016_2025.csv"
)

wetdry_output_file <- file.path(
  base_path,
  "09_wetdry_flux_budget_2016_2025.csv"
)

annual_output_file <- file.path(
  base_path,
  "09_annual_carbon_budget_2016_2025.csv"
)

yearly_qc_file <- file.path(
  base_path,
  "09_yearly_budget_QC_2016_2025.csv"
)

monthly_cor_file <- file.path(
  base_path,
  "09_monthly_correlation_matrix_2016_2025.csv"
)

fig_monthly_flux_file <- file.path(
  base_path,
  "09_monthly_mean_fluxes_2016_2025.png"
)

fig_monthly_budget_file <- file.path(
  base_path,
  "09_monthly_carbon_budget_2016_2025.png"
)

fig_annual_budget_file <- file.path(
  base_path,
  "09_annual_carbon_budget_2016_2025.png"
)

fig_seasonal_budget_file <- file.path(
  base_path,
  "09_seasonal_carbon_budget_2016_2025.png"
)

fig_wetdry_budget_file <- file.path(
  base_path,
  "09_wetdry_carbon_budget_2016_2025.png"
)

fig_env_file <- file.path(
  base_path,
  "09_monthly_environment_drivers_2016_2025.png"
)

# =========================
# 2. Read daily partitioned data
# =========================

daily <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 3. Check required columns
# =========================

required_cols <- c(
  "Year",
  "DoY",
  "Date",
  "NEE_daily",
  "GPP_daily",
  "Reco_daily",
  "VPD_daily",
  "Tair_daily",
  "Rg_daily"
)

missing_cols <- setdiff(required_cols, names(daily))

if (length(missing_cols) > 0) {
  cat("\nMissing columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

# =========================
# 4. Prepare variables
# =========================

daily <- daily %>%
  mutate(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Date = as.Date(Date),
    Month = month(Date),
    Month_name = month(Date, label = TRUE, abbr = TRUE),

    NEE_daily = as.numeric(NEE_daily),
    GPP_daily = as.numeric(GPP_daily),
    Reco_daily = as.numeric(Reco_daily),

    VPD_daily = as.numeric(VPD_daily),
    Tair_daily = as.numeric(Tair_daily),
    Rg_daily = as.numeric(Rg_daily),

    Season = case_when(
      Month %in% c(12, 1, 2) ~ "DJF",
      Month %in% c(3, 4, 5) ~ "MAM",
      Month %in% c(6, 7, 8) ~ "JJA",
      Month %in% c(9, 10, 11) ~ "SON",
      TRUE ~ NA_character_
    ),

    # Malaysia-oriented wet / dry season definition
    # You can adjust this based on local rainfall data later.
    HydroSeason = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Wet season",
      Month %in% c(4, 5, 6, 7, 8, 9, 10) ~ "Dry season",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    Year >= 2016,
    Year <= 2025,
    !is.na(Date)
  ) %>%
  arrange(Date)

# =========================
# 5. Daily flux conversion
# =========================
# Unit assumption:
# Daily NEE/GPP/Reco are daily mean fluxes in umol CO2 m-2 s-1.
#
# Conversion:
# umol CO2 m-2 s-1 * 86400 s day-1 * 12.011 / 1,000,000
# = g C m-2 day-1

daily <- daily %>%
  mutate(
    NEE_gC_day = NEE_daily * 86400 * 12.011 / 1000000,
    GPP_gC_day = GPP_daily * 86400 * 12.011 / 1000000,
    Reco_gC_day = Reco_daily * 86400 * 12.011 / 1000000
  )

# =========================
# 6. Yearly QC
# =========================

yearly_qc <- daily %>%
  group_by(Year) %>%
  summarise(
    days = n(),

    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    valid_percent_NEE = round(valid_days_NEE / days * 100, 2),
    valid_percent_GPP = round(valid_days_GPP / days * 100, 2),
    valid_percent_Reco = round(valid_days_Reco / days * 100, 2),

    missing_days_NEE = sum(is.na(NEE_daily)),
    missing_days_GPP = sum(is.na(GPP_daily)),
    missing_days_Reco = sum(is.na(Reco_daily)),

    .groups = "drop"
  )

cat("\n===== Yearly QC =====\n")
print(yearly_qc, n = Inf)

write_csv(
  yearly_qc,
  yearly_qc_file,
  na = "NA"
)

# =========================
# 7. Monthly budget
# =========================

monthly_budget <- daily %>%
  group_by(Year, Month, Month_name) %>%
  summarise(
    days = n(),

    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    valid_percent_NEE = round(valid_days_NEE / days * 100, 2),
    valid_percent_GPP = round(valid_days_GPP / days * 100, 2),
    valid_percent_Reco = round(valid_days_Reco / days * 100, 2),

    NEE_mean = mean(NEE_daily, na.rm = TRUE),
    GPP_mean = mean(GPP_daily, na.rm = TRUE),
    Reco_mean = mean(Reco_daily, na.rm = TRUE),

    NEE_sum_gC = sum(NEE_gC_day, na.rm = TRUE),
    GPP_sum_gC = sum(GPP_gC_day, na.rm = TRUE),
    Reco_sum_gC = sum(Reco_gC_day, na.rm = TRUE),

    VPD_mean = mean(VPD_daily, na.rm = TRUE),
    Tair_mean = mean(Tair_daily, na.rm = TRUE),
    Rg_mean = mean(Rg_daily, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    NEE_mean = ifelse(valid_percent_NEE >= 50, NEE_mean, NA_real_),
    GPP_mean = ifelse(valid_percent_GPP >= 50, GPP_mean, NA_real_),
    Reco_mean = ifelse(valid_percent_Reco >= 50, Reco_mean, NA_real_),

    NEE_sum_gC = ifelse(valid_percent_NEE >= 50, NEE_sum_gC, NA_real_),
    GPP_sum_gC = ifelse(valid_percent_GPP >= 50, GPP_sum_gC, NA_real_),
    Reco_sum_gC = ifelse(valid_percent_Reco >= 50, Reco_sum_gC, NA_real_)
  ) %>%
  arrange(Year, Month)

cat("\n===== Monthly budget =====\n")
print(monthly_budget, n = Inf)

write_csv(
  monthly_budget,
  monthly_output_file,
  na = "NA"
)

# =========================
# 8. Seasonal budget
# =========================

seasonal_budget <- daily %>%
  group_by(Year, Season) %>%
  summarise(
    days = n(),

    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    valid_percent_NEE = round(valid_days_NEE / days * 100, 2),
    valid_percent_GPP = round(valid_days_GPP / days * 100, 2),
    valid_percent_Reco = round(valid_days_Reco / days * 100, 2),

    NEE_mean = mean(NEE_daily, na.rm = TRUE),
    GPP_mean = mean(GPP_daily, na.rm = TRUE),
    Reco_mean = mean(Reco_daily, na.rm = TRUE),

    NEE_sum_gC = sum(NEE_gC_day, na.rm = TRUE),
    GPP_sum_gC = sum(GPP_gC_day, na.rm = TRUE),
    Reco_sum_gC = sum(Reco_gC_day, na.rm = TRUE),

    VPD_mean = mean(VPD_daily, na.rm = TRUE),
    Tair_mean = mean(Tair_daily, na.rm = TRUE),
    Rg_mean = mean(Rg_daily, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    NEE_mean = ifelse(valid_percent_NEE >= 50, NEE_mean, NA_real_),
    GPP_mean = ifelse(valid_percent_GPP >= 50, GPP_mean, NA_real_),
    Reco_mean = ifelse(valid_percent_Reco >= 50, Reco_mean, NA_real_),

    NEE_sum_gC = ifelse(valid_percent_NEE >= 50, NEE_sum_gC, NA_real_),
    GPP_sum_gC = ifelse(valid_percent_GPP >= 50, GPP_sum_gC, NA_real_),
    Reco_sum_gC = ifelse(valid_percent_Reco >= 50, Reco_sum_gC, NA_real_)
  ) %>%
  arrange(Year, Season)

cat("\n===== Seasonal budget =====\n")
print(seasonal_budget, n = Inf)

write_csv(
  seasonal_budget,
  seasonal_output_file,
  na = "NA"
)

# =========================
# 9. Wet / dry season budget
# =========================

wetdry_budget <- daily %>%
  group_by(Year, HydroSeason) %>%
  summarise(
    days = n(),

    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    valid_percent_NEE = round(valid_days_NEE / days * 100, 2),
    valid_percent_GPP = round(valid_days_GPP / days * 100, 2),
    valid_percent_Reco = round(valid_days_Reco / days * 100, 2),

    NEE_mean = mean(NEE_daily, na.rm = TRUE),
    GPP_mean = mean(GPP_daily, na.rm = TRUE),
    Reco_mean = mean(Reco_daily, na.rm = TRUE),

    NEE_sum_gC = sum(NEE_gC_day, na.rm = TRUE),
    GPP_sum_gC = sum(GPP_gC_day, na.rm = TRUE),
    Reco_sum_gC = sum(Reco_gC_day, na.rm = TRUE),

    VPD_mean = mean(VPD_daily, na.rm = TRUE),
    Tair_mean = mean(Tair_daily, na.rm = TRUE),
    Rg_mean = mean(Rg_daily, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    NEE_mean = ifelse(valid_percent_NEE >= 50, NEE_mean, NA_real_),
    GPP_mean = ifelse(valid_percent_GPP >= 50, GPP_mean, NA_real_),
    Reco_mean = ifelse(valid_percent_Reco >= 50, Reco_mean, NA_real_),

    NEE_sum_gC = ifelse(valid_percent_NEE >= 50, NEE_sum_gC, NA_real_),
    GPP_sum_gC = ifelse(valid_percent_GPP >= 50, GPP_sum_gC, NA_real_),
    Reco_sum_gC = ifelse(valid_percent_Reco >= 50, Reco_sum_gC, NA_real_)
  ) %>%
  arrange(Year, HydroSeason)

cat("\n===== Wet / dry season budget =====\n")
print(wetdry_budget, n = Inf)

write_csv(
  wetdry_budget,
  wetdry_output_file,
  na = "NA"
)

# =========================
# 10. Annual carbon budget
# =========================

annual_budget <- daily %>%
  group_by(Year) %>%
  summarise(
    days = n(),

    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    valid_percent_NEE = round(valid_days_NEE / days * 100, 2),
    valid_percent_GPP = round(valid_days_GPP / days * 100, 2),
    valid_percent_Reco = round(valid_days_Reco / days * 100, 2),

    NEE_mean = mean(NEE_daily, na.rm = TRUE),
    GPP_mean = mean(GPP_daily, na.rm = TRUE),
    Reco_mean = mean(Reco_daily, na.rm = TRUE),

    Annual_NEE_gC = sum(NEE_gC_day, na.rm = TRUE),
    Annual_GPP_gC = sum(GPP_gC_day, na.rm = TRUE),
    Annual_Reco_gC = sum(Reco_gC_day, na.rm = TRUE),

    VPD_mean = mean(VPD_daily, na.rm = TRUE),
    Tair_mean = mean(Tair_daily, na.rm = TRUE),
    Rg_mean = mean(Rg_daily, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    Annual_NEE_gC = ifelse(valid_percent_NEE >= 50, Annual_NEE_gC, NA_real_),
    Annual_GPP_gC = ifelse(valid_percent_GPP >= 50, Annual_GPP_gC, NA_real_),
    Annual_Reco_gC = ifelse(valid_percent_Reco >= 50, Annual_Reco_gC, NA_real_)
  )

cat("\n===== Annual carbon budget =====\n")
print(annual_budget, n = Inf)

write_csv(
  annual_budget,
  annual_output_file,
  na = "NA"
)

# =========================
# 11. Monthly correlation analysis
# =========================

cor_data <- monthly_budget %>%
  select(
    NEE_mean,
    GPP_mean,
    Reco_mean,
    VPD_mean,
    Tair_mean,
    Rg_mean
  )

cor_matrix <- cor(
  cor_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)

cor_df <- as.data.frame(cor_matrix) %>%
  rownames_to_column("Variable")

cat("\n===== Monthly Pearson correlation matrix =====\n")
print(cor_df)

write_csv(
  cor_df,
  monthly_cor_file,
  na = "NA"
)

# =========================
# 12. Plot monthly mean fluxes
# =========================

monthly_flux_long <- monthly_budget %>%
  select(
    Year,
    Month,
    Month_name,
    NEE_mean,
    GPP_mean,
    Reco_mean
  ) %>%
  pivot_longer(
    cols = c(NEE_mean, GPP_mean, Reco_mean),
    names_to = "Variable",
    values_to = "Value"
  )

p_monthly_flux <- ggplot(
  monthly_flux_long,
  aes(
    x = Month,
    y = Value,
    group = interaction(Year, Variable),
    color = Variable
  )
) +
  geom_line(alpha = 0.45, linewidth = 0.5) +
  geom_point(alpha = 0.65, size = 1.2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  theme_minimal() +
  labs(
    title = "Monthly Mean NEE, GPP and Reco (2016–2025)",
    x = "Month",
    y = expression(Mean~flux~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_monthly_flux)

ggsave(
  fig_monthly_flux_file,
  plot = p_monthly_flux,
  width = 12,
  height = 6,
  dpi = 300
)

# =========================
# 13. Plot monthly carbon budget
# =========================

monthly_budget_long <- monthly_budget %>%
  select(
    Year,
    Month,
    Month_name,
    NEE_sum_gC,
    GPP_sum_gC,
    Reco_sum_gC
  ) %>%
  pivot_longer(
    cols = c(NEE_sum_gC, GPP_sum_gC, Reco_sum_gC),
    names_to = "Variable",
    values_to = "Value"
  )

p_monthly_budget <- ggplot(
  monthly_budget_long,
  aes(
    x = Month,
    y = Value,
    group = interaction(Year, Variable),
    color = Variable
  )
) +
  geom_line(alpha = 0.45, linewidth = 0.5) +
  geom_point(alpha = 0.65, size = 1.2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  theme_minimal() +
  labs(
    title = "Monthly Carbon Budget (2016–2025)",
    x = "Month",
    y = expression(g~C~m^{-2}~month^{-1})
  )

print(p_monthly_budget)

ggsave(
  fig_monthly_budget_file,
  plot = p_monthly_budget,
  width = 12,
  height = 6,
  dpi = 300
)

# =========================
# 14. Plot annual carbon budget
# =========================

annual_budget_long <- annual_budget %>%
  select(
    Year,
    Annual_NEE_gC,
    Annual_GPP_gC,
    Annual_Reco_gC
  ) %>%
  pivot_longer(
    cols = c(Annual_NEE_gC, Annual_GPP_gC, Annual_Reco_gC),
    names_to = "Variable",
    values_to = "Value"
  )

p_annual_budget <- ggplot(
  annual_budget_long,
  aes(
    x = Year,
    y = Value,
    color = Variable
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Annual Carbon Budget (2016–2025)",
    x = "Year",
    y = expression(g~C~m^{-2}~yr^{-1})
  )

print(p_annual_budget)

ggsave(
  fig_annual_budget_file,
  plot = p_annual_budget,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 15. Plot seasonal carbon budget
# =========================

seasonal_budget_long <- seasonal_budget %>%
  select(
    Year,
    Season,
    NEE_sum_gC,
    GPP_sum_gC,
    Reco_sum_gC
  ) %>%
  pivot_longer(
    cols = c(NEE_sum_gC, GPP_sum_gC, Reco_sum_gC),
    names_to = "Variable",
    values_to = "Value"
  )

p_seasonal_budget <- ggplot(
  seasonal_budget_long,
  aes(
    x = Season,
    y = Value,
    color = Variable
  )
) +
  geom_boxplot(outlier.alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Seasonal Carbon Budget Distribution (2016–2025)",
    x = "Season",
    y = expression(g~C~m^{-2}~season^{-1})
  )

print(p_seasonal_budget)

ggsave(
  fig_seasonal_budget_file,
  plot = p_seasonal_budget,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 16. Plot wet / dry season carbon budget
# =========================

wetdry_budget_long <- wetdry_budget %>%
  select(
    Year,
    HydroSeason,
    NEE_sum_gC,
    GPP_sum_gC,
    Reco_sum_gC
  ) %>%
  pivot_longer(
    cols = c(NEE_sum_gC, GPP_sum_gC, Reco_sum_gC),
    names_to = "Variable",
    values_to = "Value"
  )

p_wetdry_budget <- ggplot(
  wetdry_budget_long,
  aes(
    x = HydroSeason,
    y = Value,
    color = Variable
  )
) +
  geom_boxplot(outlier.alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Wet/Dry Season Carbon Budget Distribution (2016–2025)",
    x = "Hydrological season",
    y = expression(g~C~m^{-2}~season^{-1})
  )

print(p_wetdry_budget)

ggsave(
  fig_wetdry_budget_file,
  plot = p_wetdry_budget,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 17. Plot monthly environmental drivers
# =========================

monthly_env_long <- monthly_budget %>%
  select(
    Year,
    Month,
    VPD_mean,
    Tair_mean,
    Rg_mean
  ) %>%
  pivot_longer(
    cols = c(VPD_mean, Tair_mean, Rg_mean),
    names_to = "Variable",
    values_to = "Value"
  )

p_env <- ggplot(
  monthly_env_long,
  aes(
    x = Month,
    y = Value,
    group = interaction(Year, Variable),
    color = Variable
  )
) +
  geom_line(alpha = 0.45, linewidth = 0.5) +
  geom_point(alpha = 0.65, size = 1.2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  theme_minimal() +
  labs(
    title = "Monthly Environmental Drivers (2016–2025)",
    x = "Month",
    y = "Mean value"
  )

print(p_env)

ggsave(
  fig_env_file,
  plot = p_env,
  width = 12,
  height = 6,
  dpi = 300
)

# =========================
# 18. Finished
# =========================

cat("\n=========================================\n")
cat("09 monthly / seasonal / carbon budget analysis completed successfully.\n")
cat("=========================================\n")
cat("Saved files:\n")
cat(monthly_output_file, "\n")
cat(seasonal_output_file, "\n")
cat(wetdry_output_file, "\n")
cat(annual_output_file, "\n")
cat(yearly_qc_file, "\n")
cat(monthly_cor_file, "\n")
cat(fig_monthly_flux_file, "\n")
cat(fig_monthly_budget_file, "\n")
cat(fig_annual_budget_file, "\n")
cat(fig_seasonal_budget_file, "\n")
cat(fig_wetdry_budget_file, "\n")
cat(fig_env_file, "\n")
