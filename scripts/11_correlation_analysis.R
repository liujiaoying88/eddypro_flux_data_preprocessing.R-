# =========================================
# 11 Correlation Analysis of Carbon Fluxes and Environmental Drivers
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Pearson and Spearman correlation analysis for daily and monthly
#   carbon fluxes and environmental drivers.
#
# Core rule:
#   11 reads saved CSV files only.
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

daily_pearson_file <- file.path(
  base_path,
  paste0("daily_pearson_correlation_", analysis_year, ".csv")
)

daily_spearman_file <- file.path(
  base_path,
  paste0("daily_spearman_correlation_", analysis_year, ".csv")
)

monthly_pearson_file <- file.path(
  base_path,
  paste0("monthly_pearson_correlation_", analysis_year, ".csv")
)

monthly_spearman_file <- file.path(
  base_path,
  paste0("monthly_spearman_correlation_", analysis_year, ".csv")
)

# =========================================
# 1. Helper functions
# =========================================

cor_to_long <- function(cor_matrix) {
  as.data.frame(cor_matrix) %>%
    rownames_to_column(var = "Variable_1") %>%
    pivot_longer(
      cols = -Variable_1,
      names_to = "Variable_2",
      values_to = "Correlation"
    )
}

plot_correlation_heatmap <- function(cor_long, title_text) {
  ggplot(
    cor_long,
    aes(
      x = Variable_1,
      y = Variable_2,
      fill = Correlation
    )
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = round(Correlation, 2)),
      size = 4
    ) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    ) +
    labs(
      title = title_text,
      x = "",
      y = "",
      fill = "r"
    )
}

# =========================================
# 2. Read daily data
# =========================================

daily_flux <- read_csv(
  daily_file,
  show_col_types = FALSE
)

required_daily_cols <- c(
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

daily_cor_data <- daily_flux %>%
  select(
    all_of(required_daily_cols)
  )

# =========================================
# 3. Daily correlations
# =========================================

daily_pearson <- cor(
  daily_cor_data,
  use = "complete.obs",
  method = "pearson"
)

daily_spearman <- cor(
  daily_cor_data,
  use = "complete.obs",
  method = "spearman"
)

daily_pearson_long <- cor_to_long(daily_pearson)
daily_spearman_long <- cor_to_long(daily_spearman)

write_csv(
  daily_pearson_long,
  daily_pearson_file
)

write_csv(
  daily_spearman_long,
  daily_spearman_file
)

cat("\n===== Daily Pearson correlation =====\n")
print(daily_pearson)

cat("\n===== Daily Spearman correlation =====\n")
print(daily_spearman)

# =========================================
# 4. Daily heatmaps
# =========================================

p1 <- plot_correlation_heatmap(
  daily_pearson_long,
  paste0("Daily Pearson Correlation Matrix (", analysis_year, ")")
)

print(p1)

ggsave(
  file.path(
    base_path,
    paste0("Daily_Pearson_Correlation_Heatmap_", analysis_year, ".png")
  ),
  p1,
  width = 10,
  height = 8,
  dpi = 600
)

p2 <- plot_correlation_heatmap(
  daily_spearman_long,
  paste0("Daily Spearman Correlation Matrix (", analysis_year, ")")
)

print(p2)

ggsave(
  file.path(
    base_path,
    paste0("Daily_Spearman_Correlation_Heatmap_", analysis_year, ".png")
  ),
  p2,
  width = 10,
  height = 8,
  dpi = 600
)

# =========================================
# 5. Read monthly data
# =========================================

monthly_flux <- read_csv(
  monthly_file,
  show_col_types = FALSE
)

required_monthly_cols <- c(
  "Monthly_NEE_mean",
  "Monthly_GPP_mean",
  "Monthly_Reco_mean",
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

monthly_cor_data <- monthly_flux %>%
  select(
    all_of(required_monthly_cols)
  )

# =========================================
# 6. Monthly correlations
# =========================================

monthly_pearson <- cor(
  monthly_cor_data,
  use = "complete.obs",
  method = "pearson"
)

monthly_spearman <- cor(
  monthly_cor_data,
  use = "complete.obs",
  method = "spearman"
)

monthly_pearson_long <- cor_to_long(monthly_pearson)
monthly_spearman_long <- cor_to_long(monthly_spearman)

write_csv(
  monthly_pearson_long,
  monthly_pearson_file
)

write_csv(
  monthly_spearman_long,
  monthly_spearman_file
)

cat("\n===== Monthly Pearson correlation =====\n")
print(monthly_pearson)

cat("\n===== Monthly Spearman correlation =====\n")
print(monthly_spearman)

# =========================================
# 7. Monthly heatmaps
# =========================================

p3 <- plot_correlation_heatmap(
  monthly_pearson_long,
  paste0("Monthly Pearson Correlation Matrix (", analysis_year, ")")
)

print(p3)

ggsave(
  file.path(
    base_path,
    paste0("Monthly_Pearson_Correlation_Heatmap_", analysis_year, ".png")
  ),
  p3,
  width = 10,
  height = 8,
  dpi = 600
)

p4 <- plot_correlation_heatmap(
  monthly_spearman_long,
  paste0("Monthly Spearman Correlation Matrix (", analysis_year, ")")
)

print(p4)

ggsave(
  file.path(
    base_path,
    paste0("Monthly_Spearman_Correlation_Heatmap_", analysis_year, ".png")
  ),
  p4,
  width = 10,
  height = 8,
  dpi = 600
)

# =========================================
# 8. Extract flux-environment correlations
# =========================================

daily_flux_env_cor <- daily_pearson_long %>%
  filter(
    Variable_1 %in% c(
      "Daily_NEE_mean",
      "Daily_GPP_mean",
      "Daily_Reco_mean"
    ),
    Variable_2 %in% c(
      "Daily_Rg_mean",
      "Daily_Tair_mean",
      "Daily_VPD_mean"
    )
  ) %>%
  arrange(
    Variable_1,
    desc(abs(Correlation))
  )

monthly_flux_env_cor <- monthly_pearson_long %>%
  filter(
    Variable_1 %in% c(
      "Monthly_NEE_mean",
      "Monthly_GPP_mean",
      "Monthly_Reco_mean"
    ),
    Variable_2 %in% c(
      "Monthly_Rg_mean",
      "Monthly_Tair_mean",
      "Monthly_VPD_mean"
    )
  ) %>%
  arrange(
    Variable_1,
    desc(abs(Correlation))
  )

write_csv(
  daily_flux_env_cor,
  file.path(
    base_path,
    paste0("daily_flux_environment_correlation_", analysis_year, ".csv")
  )
)

write_csv(
  monthly_flux_env_cor,
  file.path(
    base_path,
    paste0("monthly_flux_environment_correlation_", analysis_year, ".csv")
  )
)

cat("\n===== Daily flux-environment Pearson correlations =====\n")
print(daily_flux_env_cor)

cat("\n===== Monthly flux-environment Pearson correlations =====\n")
print(monthly_flux_env_cor)

# =========================================
# 9. Finished
# =========================================

saved_files <- c(
  daily_pearson_file,
  daily_spearman_file,
  monthly_pearson_file,
  monthly_spearman_file,
  file.path(base_path, paste0("Daily_Pearson_Correlation_Heatmap_", analysis_year, ".png")),
  file.path(base_path, paste0("Daily_Spearman_Correlation_Heatmap_", analysis_year, ".png")),
  file.path(base_path, paste0("Monthly_Pearson_Correlation_Heatmap_", analysis_year, ".png")),
  file.path(base_path, paste0("Monthly_Spearman_Correlation_Heatmap_", analysis_year, ".png")),
  file.path(base_path, paste0("daily_flux_environment_correlation_", analysis_year, ".csv")),
  file.path(base_path, paste0("monthly_flux_environment_correlation_", analysis_year, ".csv"))
)

cat("\n=========================================\n")
cat("11 correlation analysis completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
