# =========================================
# 10 Statistical Modeling of Carbon Fluxes and Environmental Drivers
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

monthly_file <- file.path(base_path, paste0("monthly_flux_", analysis_year, ".csv"))
partitioned_file <- file.path(base_path, paste0("partitioned_with_datetime_", analysis_year, ".csv"))

daily_output_file <- file.path(base_path, paste0("daily_flux_for_modeling_", analysis_year, ".csv"))
monthly_model_output_file <- file.path(base_path, paste0("monthly_model_summary_", analysis_year, ".csv"))
daily_model_output_file <- file.path(base_path, paste0("daily_model_summary_", analysis_year, ".csv"))
monthly_coefficients_output_file <- file.path(base_path, paste0("monthly_model_coefficients_", analysis_year, ".csv"))
daily_coefficients_output_file <- file.path(base_path, paste0("daily_model_coefficients_", analysis_year, ".csv"))

# =========================================
# 1. Helper functions
# =========================================

extract_model_summary <- function(model, model_name) {
  s <- summary(model)
  
  tibble(
    Model = model_name,
    R_squared = s$r.squared,
    Adjusted_R_squared = s$adj.r.squared,
    F_statistic = unname(s$fstatistic[1]),
    DF_model = unname(s$fstatistic[2]),
    DF_residual = unname(s$fstatistic[3]),
    Model_P_value = pf(
      s$fstatistic[1],
      s$fstatistic[2],
      s$fstatistic[3],
      lower.tail = FALSE
    )
  )
}

extract_coefficients <- function(model, model_name) {
  coef_df <- as.data.frame(summary(model)$coefficients) %>%
    rownames_to_column(var = "Term") %>%
    as_tibble()
  
  names(coef_df) <- c(
    "Term",
    "Estimate",
    "Std_Error",
    "T_value",
    "P_value"
  )
  
  coef_df %>%
    mutate(Model = model_name) %>%
    select(
      Model,
      Term,
      Estimate,
      Std_Error,
      T_value,
      P_value
    )
}

# =========================================
# 2. Read monthly data
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

# =========================================
# 3. Monthly models
# =========================================

monthly_models <- list(
  Monthly_NEE_env = lm(
    Monthly_NEE_mean ~ Monthly_Rg_mean + Monthly_Tair_mean + Monthly_VPD_mean,
    data = monthly_flux
  ),
  Monthly_GPP_env = lm(
    Monthly_GPP_mean ~ Monthly_Rg_mean + Monthly_Tair_mean + Monthly_VPD_mean,
    data = monthly_flux
  ),
  Monthly_Reco_env = lm(
    Monthly_Reco_mean ~ Monthly_Rg_mean + Monthly_Tair_mean + Monthly_VPD_mean,
    data = monthly_flux
  ),
  Monthly_GPP_Rg = lm(
    Monthly_GPP_mean ~ Monthly_Rg_mean,
    data = monthly_flux
  ),
  Monthly_GPP_VPD = lm(
    Monthly_GPP_mean ~ Monthly_VPD_mean,
    data = monthly_flux
  ),
  Monthly_Reco_Tair = lm(
    Monthly_Reco_mean ~ Monthly_Tair_mean,
    data = monthly_flux
  )
)

monthly_model_summary <- bind_rows(
  lapply(
    names(monthly_models),
    function(model_name) {
      extract_model_summary(
        monthly_models[[model_name]],
        model_name
      )
    }
  )
)

monthly_coefficients <- bind_rows(
  lapply(
    names(monthly_models),
    function(model_name) {
      extract_coefficients(
        monthly_models[[model_name]],
        model_name
      )
    }
  )
)

write_csv(monthly_model_summary, monthly_model_output_file)
write_csv(monthly_coefficients, monthly_coefficients_output_file)

cat("\n===== Monthly model summary =====\n")
print(monthly_model_summary)

# =========================================
# 4. Read partitioned data
# =========================================

partitioned <- read_csv(
  partitioned_file,
  show_col_types = FALSE
)

required_partitioned_cols <- c(
  "DateTime",
  "NEE_U50_f",
  "GPP_DT_U50",
  "Reco_DT_U50",
  "Rg_f",
  "Tair_f",
  "VPD_f"
)

missing_partitioned_cols <- setdiff(
  required_partitioned_cols,
  names(partitioned)
)

if (length(missing_partitioned_cols) > 0) {
  stop(
    paste(
      "Missing required partitioned columns:",
      paste(missing_partitioned_cols, collapse = ", ")
    )
  )
}

partitioned <- partitioned %>%
  mutate(
    DateTime = as.POSIXct(
      DateTime,
      tz = "Asia/Kuala_Lumpur"
    ),
    NEE_raw = as.numeric(NEE_U50_f),
    GPP = as.numeric(GPP_DT_U50),
    Reco = as.numeric(Reco_DT_U50),
    Rg = as.numeric(Rg_f),
    Tair = as.numeric(Tair_f),
    VPD = as.numeric(VPD_f)
  )

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
      NEE_raw < nee_lower | NEE_raw > nee_upper,
      NA,
      NEE_raw
    )
  )

# =========================================
# 5. Daily dataset
# =========================================

daily_flux <- partitioned %>%
  mutate(
    Date = as.Date(DateTime)
  ) %>%
  group_by(Date) %>%
  summarise(
    Daily_NEE_mean = mean(NEE, na.rm = TRUE),
    Daily_GPP_mean = mean(GPP, na.rm = TRUE),
    Daily_Reco_mean = mean(Reco, na.rm = TRUE),
    Daily_Rg_mean = mean(Rg, na.rm = TRUE),
    Daily_Tair_mean = mean(Tair, na.rm = TRUE),
    Daily_VPD_mean = mean(VPD, na.rm = TRUE),
    Records = n(),
    NEE_NA = sum(is.na(NEE)),
    GPP_NA = sum(is.na(GPP)),
    Reco_NA = sum(is.na(Reco)),
    .groups = "drop"
  )

write_csv(daily_flux, daily_output_file)

cat("\n===== Daily flux preview =====\n")
print(head(daily_flux))

# =========================================
# 6. Daily models
# =========================================

daily_models <- list(
  Daily_NEE_env = lm(
    Daily_NEE_mean ~ Daily_Rg_mean + Daily_Tair_mean + Daily_VPD_mean,
    data = daily_flux
  ),
  Daily_GPP_env = lm(
    Daily_GPP_mean ~ Daily_Rg_mean + Daily_Tair_mean + Daily_VPD_mean,
    data = daily_flux
  ),
  Daily_Reco_env = lm(
    Daily_Reco_mean ~ Daily_Rg_mean + Daily_Tair_mean + Daily_VPD_mean,
    data = daily_flux
  ),
  Daily_GPP_Rg = lm(
    Daily_GPP_mean ~ Daily_Rg_mean,
    data = daily_flux
  ),
  Daily_GPP_VPD = lm(
    Daily_GPP_mean ~ Daily_VPD_mean,
    data = daily_flux
  ),
  Daily_Reco_Tair = lm(
    Daily_Reco_mean ~ Daily_Tair_mean,
    data = daily_flux
  )
)

daily_model_summary <- bind_rows(
  lapply(
    names(daily_models),
    function(model_name) {
      extract_model_summary(
        daily_models[[model_name]],
        model_name
      )
    }
  )
)

daily_coefficients <- bind_rows(
  lapply(
    names(daily_models),
    function(model_name) {
      extract_coefficients(
        daily_models[[model_name]],
        model_name
      )
    }
  )
)

write_csv(daily_model_summary, daily_model_output_file)
write_csv(daily_coefficients, daily_coefficients_output_file)

cat("\n===== Daily model summary =====\n")
print(daily_model_summary)

# =========================================
# 7. Diagnostic plots
# =========================================

p1 <- ggplot(
  daily_flux,
  aes(x = Daily_Rg_mean, y = Daily_GPP_mean)
) +
  geom_point(alpha = 0.5, color = "forestgreen") +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(
    title = paste0("Daily GPP vs Radiation (", analysis_year, ")"),
    x = "Daily Mean Rg",
    y = "Daily Mean GPP"
  )

print(p1)

ggsave(
  file.path(base_path, paste0("Daily_GPP_vs_Rg_", analysis_year, ".png")),
  p1,
  width = 10,
  height = 6,
  dpi = 600
)

p2 <- ggplot(
  daily_flux,
  aes(x = Daily_VPD_mean, y = Daily_GPP_mean)
) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(
    title = paste0("Daily GPP vs VPD (", analysis_year, ")"),
    x = "Daily Mean VPD",
    y = "Daily Mean GPP"
  )

print(p2)

ggsave(
  file.path(base_path, paste0("Daily_GPP_vs_VPD_", analysis_year, ".png")),
  p2,
  width = 10,
  height = 6,
  dpi = 600
)

p3 <- ggplot(
  daily_flux,
  aes(x = Daily_Tair_mean, y = Daily_Reco_mean)
) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(
    title = paste0("Daily Reco vs Air Temperature (", analysis_year, ")"),
    x = "Daily Mean Air Temperature",
    y = "Daily Mean Reco"
  )

print(p3)

ggsave(
  file.path(base_path, paste0("Daily_Reco_vs_Tair_", analysis_year, ".png")),
  p3,
  width = 10,
  height = 6,
  dpi = 600
)

p4 <- ggplot(
  daily_flux,
  aes(x = Daily_Rg_mean, y = Daily_NEE_mean)
) +
  geom_point(alpha = 0.5, color = "forestgreen") +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(
    title = paste0("Daily NEE vs Radiation (", analysis_year, ")"),
    x = "Daily Mean Rg",
    y = "Daily Mean NEE"
  )

print(p4)

ggsave(
  file.path(base_path, paste0("Daily_NEE_vs_Rg_", analysis_year, ".png")),
  p4,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 8. Finished
# =========================================

saved_files <- c(
  daily_output_file,
  monthly_model_output_file,
  daily_model_output_file,
  monthly_coefficients_output_file,
  daily_coefficients_output_file,
  file.path(base_path, paste0("Daily_GPP_vs_Rg_", analysis_year, ".png")),
  file.path(base_path, paste0("Daily_GPP_vs_VPD_", analysis_year, ".png")),
  file.path(base_path, paste0("Daily_Reco_vs_Tair_", analysis_year, ".png")),
  file.path(base_path, paste0("Daily_NEE_vs_Rg_", analysis_year, ".png"))
)

cat("\n=========================================\n")
cat("10 statistical modeling completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
