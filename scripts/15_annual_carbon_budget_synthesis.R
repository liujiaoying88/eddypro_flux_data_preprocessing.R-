# =========================================
# 15 Annual Carbon Budget Synthesis
# Site: MukaHead / CEMACS, Penang, Malaysia
# Author: Cai Xiaoliang
# Purpose:
#   Synthesize annual carbon budget, environmental drivers,
#   data quality, gap-filling and uStar information for one year.
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

partitioned_file <- file.path(
  base_path,
  paste0("partitioned_with_datetime_", analysis_year, ".csv")
)

monthly_flux_file <- file.path(
  base_path,
  paste0("monthly_flux_", analysis_year, ".csv")
)

seasonal_summary_file <- file.path(
  base_path,
  paste0("seasonal_monthly_summary_", analysis_year, ".csv")
)

data_quality_file <- file.path(
  base_path,
  paste0("data_quality_summary_", analysis_year, ".csv")
)

ustar_summary_file <- file.path(
  base_path,
  paste0("ustar_summary_", analysis_year, ".csv")
)

annual_synthesis_file <- file.path(
  base_path,
  paste0("annual_synthesis_", analysis_year, ".csv")
)

annual_carbon_budget_file <- file.path(
  base_path,
  paste0("annual_carbon_budget_final_", analysis_year, ".csv")
)

annual_quality_file <- file.path(
  base_path,
  paste0("annual_quality_final_", analysis_year, ".csv")
)

annual_env_file <- file.path(
  base_path,
  paste0("annual_environment_final_", analysis_year, ".csv")
)

# =========================================
# 1. Read files
# =========================================

partitioned <- read_csv(
  partitioned_file,
  show_col_types = FALSE
)

monthly_flux <- read_csv(
  monthly_flux_file,
  show_col_types = FALSE
)

seasonal_summary <- read_csv(
  seasonal_summary_file,
  show_col_types = FALSE
)

data_quality <- read_csv(
  data_quality_file,
  show_col_types = FALSE
)

ustar_summary <- read_csv(
  ustar_summary_file,
  show_col_types = FALSE
)

# =========================================
# 2. Check required columns
# =========================================

required_monthly_cols <- c(
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

required_quality_cols <- c(
  "Raw_record_completeness",
  "CO2_flux_QC_accepted_ratio",
  "CO2_flux_QC_rejected_ratio",
  "NEE_gapfilled_ratio",
  "Final_product_completeness"
)

missing_quality_cols <- setdiff(
  required_quality_cols,
  names(data_quality)
)

if (length(missing_quality_cols) > 0) {
  stop(
    paste(
      "Missing required quality columns:",
      paste(missing_quality_cols, collapse = ", ")
    )
  )
}

# =========================================
# 3. Annual carbon budget
# =========================================

annual_carbon_budget <- monthly_flux %>%
  summarise(
    Year = analysis_year,

    Annual_NEE_sum_gC = sum(
      Monthly_NEE_sum_gC,
      na.rm = TRUE
    ),

    Annual_GPP_sum_gC = sum(
      Monthly_GPP_sum_gC,
      na.rm = TRUE
    ),

    Annual_Reco_sum_gC = sum(
      Monthly_Reco_sum_gC,
      na.rm = TRUE
    ),

    Annual_NEE_mean = mean(
      Monthly_NEE_mean,
      na.rm = TRUE
    ),

    Annual_GPP_mean = mean(
      Monthly_GPP_mean,
      na.rm = TRUE
    ),

    Annual_Reco_mean = mean(
      Monthly_Reco_mean,
      na.rm = TRUE
    )
  )

write_csv(
  annual_carbon_budget,
  annual_carbon_budget_file
)

cat("\n===== Annual carbon budget =====\n")
print(annual_carbon_budget)

# =========================================
# 4. Annual environmental drivers
# =========================================

annual_environment <- monthly_flux %>%
  summarise(
    Year = analysis_year,

    Annual_Rg_mean = mean(
      Monthly_Rg_mean,
      na.rm = TRUE
    ),

    Annual_Tair_mean = mean(
      Monthly_Tair_mean,
      na.rm = TRUE
    ),

    Annual_VPD_mean = mean(
      Monthly_VPD_mean,
      na.rm = TRUE
    ),

    Annual_Rg_sd = sd(
      Monthly_Rg_mean,
      na.rm = TRUE
    ),

    Annual_Tair_sd = sd(
      Monthly_Tair_mean,
      na.rm = TRUE
    ),

    Annual_VPD_sd = sd(
      Monthly_VPD_mean,
      na.rm = TRUE
    )
  )

write_csv(
  annual_environment,
  annual_env_file
)

cat("\n===== Annual environmental drivers =====\n")
print(annual_environment)

# =========================================
# 5. Seasonal contribution to annual carbon budget
# =========================================

seasonal_contribution <- seasonal_summary %>%
  mutate(
    Year = analysis_year,
    GPP_contribution_ratio = Seasonal_GPP_sum_gC /
      sum(
        Seasonal_GPP_sum_gC,
        na.rm = TRUE
      ),
    Reco_contribution_ratio = Seasonal_Reco_sum_gC /
      sum(
        Seasonal_Reco_sum_gC,
        na.rm = TRUE
      ),
    NEE_contribution_ratio = Seasonal_NEE_sum_gC /
      sum(
        Seasonal_NEE_sum_gC,
        na.rm = TRUE
      )
  )

write_csv(
  seasonal_contribution,
  file.path(
    base_path,
    paste0("seasonal_contribution_", analysis_year, ".csv")
  )
)

cat("\n===== Seasonal contribution =====\n")
print(seasonal_contribution)

# =========================================
# 6. Annual data quality summary
# =========================================

annual_quality <- data_quality %>%
  transmute(
    Year = analysis_year,

    Raw_record_completeness = Raw_record_completeness,

    CO2_flux_QC_accepted_ratio = CO2_flux_QC_accepted_ratio,

    CO2_flux_QC_rejected_ratio = CO2_flux_QC_rejected_ratio,

    NEE_gapfilled_ratio = NEE_gapfilled_ratio,

    Measured_NEE_ratio = 1 - NEE_gapfilled_ratio,

    Final_product_completeness = Final_product_completeness
  )

write_csv(
  annual_quality,
  annual_quality_file
)

cat("\n===== Annual data quality =====\n")
print(annual_quality)

# =========================================
# 7. uStar key statistics
# =========================================

ustar_numeric <- ustar_summary %>%
  select(
    where(is.numeric)
  )

ustar_key <- tibble(
  Year = analysis_year,
  Ustar_summary_available = ncol(ustar_numeric) > 0
)

if (ncol(ustar_numeric) > 0) {
  ustar_key <- bind_cols(
    ustar_key,
    ustar_numeric
  )
}

cat("\n===== uStar summary =====\n")
print(ustar_key)

# =========================================
# 8. Combine annual synthesis table
# =========================================

annual_synthesis <- annual_carbon_budget %>%
  left_join(
    annual_environment,
    by = "Year"
  ) %>%
  left_join(
    annual_quality,
    by = "Year"
  ) %>%
  left_join(
    ustar_key,
    by = "Year"
  )

write_csv(
  annual_synthesis,
  annual_synthesis_file
)

cat("\n===== Annual synthesis table =====\n")
print(annual_synthesis)

# =========================================
# 9. Plot annual carbon budget
# =========================================

annual_carbon_long <- annual_carbon_budget %>%
  select(
    Year,
    Annual_NEE_sum_gC,
    Annual_GPP_sum_gC,
    Annual_Reco_sum_gC
  ) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Variable",
    values_to = "Value"
  )

p1 <- ggplot(
  annual_carbon_long,
  aes(
    x = Variable,
    y = Value,
    fill = Variable
  )
) +
  geom_col() +
  theme_minimal() +
  labs(
    title = paste0("Annual Carbon Budget (", analysis_year, ")"),
    x = "",
    y = "g C m-2 yr-1"
  ) +
  theme(
    legend.position = "none"
  )

print(p1)

ggsave(
  file.path(
    base_path,
    paste0("Annual_Carbon_Budget_", analysis_year, ".png")
  ),
  p1,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 10. Plot annual data quality
# =========================================

annual_quality_long <- annual_quality %>%
  select(
    Raw_record_completeness,
    CO2_flux_QC_accepted_ratio,
    CO2_flux_QC_rejected_ratio,
    NEE_gapfilled_ratio,
    Measured_NEE_ratio,
    Final_product_completeness
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "Ratio"
  )

p2 <- ggplot(
  annual_quality_long,
  aes(
    x = reorder(Metric, Ratio),
    y = Ratio
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = paste0("Annual Data Quality Indicators (", analysis_year, ")"),
    x = "",
    y = "Ratio"
  )

print(p2)

ggsave(
  file.path(
    base_path,
    paste0("Annual_Data_Quality_Indicators_", analysis_year, ".png")
  ),
  p2,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 11. Finished
# =========================================

saved_files <- c(
  annual_synthesis_file,
  annual_carbon_budget_file,
  annual_quality_file,
  annual_env_file,
  file.path(base_path, paste0("seasonal_contribution_", analysis_year, ".csv")),
  file.path(base_path, paste0("Annual_Carbon_Budget_", analysis_year, ".png")),
  file.path(base_path, paste0("Annual_Data_Quality_Indicators_", analysis_year, ".png"))
)

cat("\n=========================================\n")
cat("15 annual carbon budget synthesis completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
