# =========================================
# 09 Monthly Flux, Carbon Budget and Environmental Analysis
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Monthly and annual analysis of NEE, GPP, Reco and environmental drivers
#   based on REddyProc partitioned output.
#
# Core rule:
#   09 reads partitioned_with_datetime_YEAR.csv if available.
#   Otherwise it reads reddyproc_partitioned_YEAR.csv and reconstructs DateTime.
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

partitioned_datetime_file <- file.path(
  base_path,
  paste0("partitioned_with_datetime_", analysis_year, ".csv")
)

partitioned_file <- file.path(
  base_path,
  paste0("reddyproc_partitioned_", analysis_year, ".csv")
)

monthly_output_file <- file.path(
  base_path,
  paste0("monthly_flux_", analysis_year, ".csv")
)

annual_output_file <- file.path(
  base_path,
  paste0("annual_carbon_summary_", analysis_year, ".csv")
)

correlation_output_file <- file.path(
  base_path,
  paste0("monthly_correlation_", analysis_year, ".csv")
)

spearman_output_file <- file.path(
  base_path,
  paste0("monthly_spearman_correlation_", analysis_year, ".csv")
)

# =========================================
# 1. Read partitioned data
# =========================================

if (file.exists(partitioned_datetime_file)) {
  partitioned <- read_csv(
    partitioned_datetime_file,
    show_col_types = FALSE
  )
} else {
  partitioned <- read_csv(
    partitioned_file,
    show_col_types = FALSE
  )

  expected_rows <- ifelse(
    leap_year(analysis_year),
    366 * 48,
    365 * 48
  )

  if (nrow(partitioned) != expected_rows) {
    warning(
      paste(
        "Partitioned row count is",
        nrow(partitioned),
        "but expected",
        expected_rows
      )
    )
  }

  partitioned <- partitioned %>%
    mutate(
      DateTime = as.POSIXct(
        as.Date(paste0(analysis_year, "-01-01")),
        tz = "Asia/Kuala_Lumpur"
      ) +
        minutes(30) * row_number(),

      Year = year(DateTime),
      DoY = yday(DateTime),
      Hour = hour(DateTime) + minute(DateTime) / 60
    ) %>%
    relocate(
      DateTime,
      Year,
      DoY,
      Hour
    )
}

# =========================================
# 2. Select key variables
# =========================================

nee_col <- "NEE_U50_f"
gpp_col <- "GPP_DT_U50"
reco_col <- "Reco_DT_U50"

required_cols <- c(
  "DateTime",
  nee_col,
  gpp_col,
  reco_col,
  "Rg_f",
  "Tair_f",
  "VPD_f"
)

missing_cols <- setdiff(
  required_cols,
  names(partitioned)
)

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# =========================================
# 3. Prepare clean analysis dataset
# =========================================

partitioned <- partitioned %>%
  mutate(
    DateTime = as.POSIXct(
      DateTime,
      tz = "Asia/Kuala_Lumpur"
    ),

    NEE_raw = as.numeric(.data[[nee_col]]),
    GPP = as.numeric(.data[[gpp_col]]),
    Reco = as.numeric(.data[[reco_col]]),

    Rg = as.numeric(Rg_f),
    Tair = as.numeric(Tair_f),
    VPD = as.numeric(VPD_f),

    Month = month(DateTime, label = TRUE, abbr = TRUE),
    Month_num = month(DateTime),

    Season = case_when(
      Month_num %in% c(12, 1, 2) ~ "DJF",
      Month_num %in% c(3, 4, 5) ~ "MAM",
      Month_num %in% c(6, 7, 8) ~ "JJA",
      Month_num %in% c(9, 10, 11) ~ "SON",
      TRUE ~ NA_character_
    )
  )

# =========================================
# 4. NEE outlier filtering for budget calculation
# =========================================
# REddyProc NEE may still contain rare extreme values after gap filling.
# Use percentile-based filtering for monthly and annual budget stability.

nee_lower <- quantile(
  partitioned$NEE_raw,
  0.005,
  na.rm = TRUE
)

nee_upper <- quantile(
  partitioned$NEE_raw,
  0.995,
  na.rm = TRUE
)

partitioned <- partitioned %>%
  mutate(
    NEE = ifelse(
      NEE_raw < nee_lower |
        NEE_raw > nee_upper,
      NA,
      NEE_raw
    ),

    # Half-hourly flux conversion:
    # umol CO2 m-2 s-1 -> g C m-2 per 30 min
    NEE_gC_30min = NEE * 1800 * 12.011 / 1000000,
    GPP_gC_30min = GPP * 1800 * 12.011 / 1000000,
    Reco_gC_30min = Reco * 1800 * 12.011 / 1000000
  )

cat("\n===== NEE filtering threshold =====\n")
cat("NEE lower threshold:", nee_lower, "\n")
cat("NEE upper threshold:", nee_upper, "\n")

cat("\n===== Removed NEE outliers =====\n")
print(
  sum(
    is.na(partitioned$NEE) &
      !is.na(partitioned$NEE_raw)
  )
)

# =========================================
# 5. Monthly summary
# =========================================

monthly_flux <- partitioned %>%
  group_by(Month) %>%
  summarise(
    Records = n(),

    Monthly_NEE_mean = mean(NEE, na.rm = TRUE),
    Monthly_GPP_mean = mean(GPP, na.rm = TRUE),
    Monthly_Reco_mean = mean(Reco, na.rm = TRUE),

    Monthly_NEE_sum_gC = sum(NEE_gC_30min, na.rm = TRUE),
    Monthly_GPP_sum_gC = sum(GPP_gC_30min, na.rm = TRUE),
    Monthly_Reco_sum_gC = sum(Reco_gC_30min, na.rm = TRUE),

    Monthly_Rg_mean = mean(Rg, na.rm = TRUE),
    Monthly_Tair_mean = mean(Tair, na.rm = TRUE),
    Monthly_VPD_mean = mean(VPD, na.rm = TRUE),

    NEE_NA = sum(is.na(NEE)),
    GPP_NA = sum(is.na(GPP)),
    Reco_NA = sum(is.na(Reco)),
    Rg_NA = sum(is.na(Rg)),
    Tair_NA = sum(is.na(Tair)),
    VPD_NA = sum(is.na(VPD)),

    .groups = "drop"
  )

cat("\n===== Monthly flux summary =====\n")
print(monthly_flux)

# =========================================
# 6. Seasonal summary
# =========================================

seasonal_flux <- partitioned %>%
  group_by(Season) %>%
  summarise(
    Records = n(),

    Seasonal_NEE_mean = mean(NEE, na.rm = TRUE),
    Seasonal_GPP_mean = mean(GPP, na.rm = TRUE),
    Seasonal_Reco_mean = mean(Reco, na.rm = TRUE),

    Seasonal_NEE_sum_gC = sum(NEE_gC_30min, na.rm = TRUE),
    Seasonal_GPP_sum_gC = sum(GPP_gC_30min, na.rm = TRUE),
    Seasonal_Reco_sum_gC = sum(Reco_gC_30min, na.rm = TRUE),

    Seasonal_Rg_mean = mean(Rg, na.rm = TRUE),
    Seasonal_Tair_mean = mean(Tair, na.rm = TRUE),
    Seasonal_VPD_mean = mean(VPD, na.rm = TRUE),

    .groups = "drop"
  )

cat("\n===== Seasonal flux summary =====\n")
print(seasonal_flux)

write_csv(
  seasonal_flux,
  file.path(
    base_path,
    paste0("seasonal_flux_", analysis_year, ".csv")
  )
)

# =========================================
# 7. Annual summary
# =========================================

annual_summary <- monthly_flux %>%
  summarise(
    Annual_NEE_gC = sum(Monthly_NEE_sum_gC, na.rm = TRUE),
    Annual_GPP_gC = sum(Monthly_GPP_sum_gC, na.rm = TRUE),
    Annual_Reco_gC = sum(Monthly_Reco_sum_gC, na.rm = TRUE),

    Annual_NEE_mean = mean(Monthly_NEE_mean, na.rm = TRUE),
    Annual_GPP_mean = mean(Monthly_GPP_mean, na.rm = TRUE),
    Annual_Reco_mean = mean(Monthly_Reco_mean, na.rm = TRUE),

    Mean_Rg = mean(Monthly_Rg_mean, na.rm = TRUE),
    Mean_Tair = mean(Monthly_Tair_mean, na.rm = TRUE),
    Mean_VPD = mean(Monthly_VPD_mean, na.rm = TRUE)
  )

cat("\n===== Annual summary =====\n")
print(annual_summary)

# =========================================
# 8. Plot monthly mean carbon fluxes
# =========================================

monthly_flux_mean_long <- monthly_flux %>%
  select(
    Month,
    Monthly_NEE_mean,
    Monthly_GPP_mean,
    Monthly_Reco_mean
  ) %>%
  pivot_longer(
    cols = -Month,
    names_to = "Variable",
    values_to = "Value"
  )

p1 <- ggplot(
  monthly_flux_mean_long,
  aes(
    x = Month,
    y = Value,
    group = Variable,
    color = Variable
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = paste0("Monthly Mean Carbon Fluxes (", analysis_year, ")"),
    x = "Month",
    y = "Mean Flux"
  )

print(p1)

ggsave(
  file.path(
    base_path,
    paste0("Monthly_Mean_Carbon_Fluxes_", analysis_year, ".png")
  ),
  p1,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 9. Plot monthly carbon budget
# =========================================

monthly_flux_sum_long <- monthly_flux %>%
  select(
    Month,
    Monthly_NEE_sum_gC,
    Monthly_GPP_sum_gC,
    Monthly_Reco_sum_gC
  ) %>%
  pivot_longer(
    cols = -Month,
    names_to = "Variable",
    values_to = "Value"
  )

p2 <- ggplot(
  monthly_flux_sum_long,
  aes(
    x = Month,
    y = Value,
    group = Variable,
    color = Variable
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = paste0("Monthly Carbon Budget (", analysis_year, ")"),
    x = "Month",
    y = "g C m-2 month-1"
  )

print(p2)

ggsave(
  file.path(
    base_path,
    paste0("Monthly_Carbon_Budget_", analysis_year, ".png")
  ),
  p2,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 10. Plot monthly environmental drivers
# =========================================

monthly_env_long <- monthly_flux %>%
  select(
    Month,
    Monthly_Rg_mean,
    Monthly_Tair_mean,
    Monthly_VPD_mean
  ) %>%
  pivot_longer(
    cols = -Month,
    names_to = "Variable",
    values_to = "Value"
  )

p3 <- ggplot(
  monthly_env_long,
  aes(
    x = Month,
    y = Value,
    group = Variable,
    color = Variable
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = paste0("Monthly Environmental Drivers (", analysis_year, ")"),
    x = "Month",
    y = "Mean Value"
  )

print(p3)

ggsave(
  file.path(
    base_path,
    paste0("Monthly_Environmental_Drivers_", analysis_year, ".png")
  ),
  p3,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 11. Standardized environmental drivers
# =========================================

monthly_scaled <- monthly_flux %>%
  mutate(
    Rg_scaled = as.numeric(scale(Monthly_Rg_mean)),
    Tair_scaled = as.numeric(scale(Monthly_Tair_mean)),
    VPD_scaled = as.numeric(scale(Monthly_VPD_mean))
  ) %>%
  select(
    Month,
    Rg_scaled,
    Tair_scaled,
    VPD_scaled
  ) %>%
  pivot_longer(
    cols = -Month,
    names_to = "Variable",
    values_to = "Value"
  )

p4 <- ggplot(
  monthly_scaled,
  aes(
    x = Month,
    y = Value,
    group = Variable,
    color = Variable
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = paste0("Standardized Monthly Environmental Drivers (", analysis_year, ")"),
    x = "Month",
    y = "Scaled Value"
  )

print(p4)

ggsave(
  file.path(
    base_path,
    paste0("Scaled_Environmental_Drivers_", analysis_year, ".png")
  ),
  p4,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 12. Correlation analysis
# =========================================

correlation_data <- monthly_flux %>%
  select(
    Monthly_NEE_mean,
    Monthly_GPP_mean,
    Monthly_Reco_mean,
    Monthly_Rg_mean,
    Monthly_Tair_mean,
    Monthly_VPD_mean
  )

pearson_cor_matrix <- cor(
  correlation_data,
  use = "complete.obs",
  method = "pearson"
)

spearman_cor_matrix <- cor(
  correlation_data,
  use = "complete.obs",
  method = "spearman"
)

cat("\n===== Monthly Pearson correlation matrix =====\n")
print(pearson_cor_matrix)

cat("\n===== Monthly Spearman correlation matrix =====\n")
print(spearman_cor_matrix)

pearson_cor_df <- as.data.frame(pearson_cor_matrix) %>%
  rownames_to_column(
    var = "Variable"
  )

spearman_cor_df <- as.data.frame(spearman_cor_matrix) %>%
  rownames_to_column(
    var = "Variable"
  )

# =========================================
# 13. Simple regression models
# =========================================

model_gpp_rg <- lm(
  Monthly_GPP_mean ~ Monthly_Rg_mean,
  data = monthly_flux
)

model_gpp_vpd <- lm(
  Monthly_GPP_mean ~ Monthly_VPD_mean,
  data = monthly_flux
)

model_reco_tair <- lm(
  Monthly_Reco_mean ~ Monthly_Tair_mean,
  data = monthly_flux
)

model_nee_env <- lm(
  Monthly_NEE_mean ~ Monthly_Rg_mean + Monthly_Tair_mean + Monthly_VPD_mean,
  data = monthly_flux
)

model_summary <- tibble(
  Model = c(
    "GPP ~ Rg",
    "GPP ~ VPD",
    "Reco ~ Tair",
    "NEE ~ Rg + Tair + VPD"
  ),
  R_squared = c(
    summary(model_gpp_rg)$r.squared,
    summary(model_gpp_vpd)$r.squared,
    summary(model_reco_tair)$r.squared,
    summary(model_nee_env)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(model_gpp_rg)$adj.r.squared,
    summary(model_gpp_vpd)$adj.r.squared,
    summary(model_reco_tair)$adj.r.squared,
    summary(model_nee_env)$adj.r.squared
  ),
  P_value = c(
    summary(model_gpp_rg)$coefficients[2, 4],
    summary(model_gpp_vpd)$coefficients[2, 4],
    summary(model_reco_tair)$coefficients[2, 4],
    NA_real_
  )
)

cat("\n===== Regression model summary =====\n")
print(model_summary)

# =========================================
# 14. Save outputs
# =========================================

write_csv(
  monthly_flux,
  monthly_output_file
)

write_csv(
  annual_summary,
  annual_output_file
)

write_csv(
  pearson_cor_df,
  correlation_output_file
)

write_csv(
  spearman_cor_df,
  spearman_output_file
)

write_csv(
  model_summary,
  file.path(
    base_path,
    paste0("monthly_regression_summary_", analysis_year, ".csv")
  )
)

cat("\n=========================================\n")
cat("09 monthly flux and environmental analysis completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
cat(monthly_output_file, "\n")
cat(annual_output_file, "\n")
cat(correlation_output_file, "\n")
cat(spearman_output_file, "\n")
cat(file.path(base_path, paste0("seasonal_flux_", analysis_year, ".csv")), "\n")
cat(file.path(base_path, paste0("monthly_regression_summary_", analysis_year, ".csv")), "\n")
