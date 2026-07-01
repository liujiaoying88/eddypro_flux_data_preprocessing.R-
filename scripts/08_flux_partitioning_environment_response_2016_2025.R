# ============================================================
# 08_flux_partitioning_environment_response_2016_2025.R
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

base_path <- "/Users/caixiaoliang/Documents"

input_file <- file.path(
  base_path,
  "reddyproc_partitioned_2016_2025_WITH_GPP_RECO.csv"
)

# Output files
output_halfhour_file <- file.path(base_path, "08_partitioned_selected_2016_2025.csv")
output_daily_file <- file.path(base_path, "08_daily_partitioned_flux_2016_2025.csv")
output_monthly_file <- file.path(base_path, "08_monthly_partitioned_flux_2016_2025.csv")
output_seasonal_file <- file.path(base_path, "08_seasonal_partitioned_flux_2016_2025.csv")
output_yearly_file <- file.path(base_path, "08_yearly_partitioned_flux_2016_2025.csv")
output_correlation_file <- file.path(base_path, "08_correlation_results_2016_2025.csv")
output_regression_file <- file.path(base_path, "08_regression_results_2016_2025.csv")

# Figure files
fig_gpp_rg_file <- file.path(base_path, "08_GPP_vs_Rg_2016_2025.png")
fig_gpp_vpd_file <- file.path(base_path, "08_GPP_vs_VPD_daytime_2016_2025.png")
fig_reco_tair_file <- file.path(base_path, "08_Reco_vs_Tair_2016_2025.png")
fig_nee_rg_file <- file.path(base_path, "08_NEE_vs_Rg_2016_2025.png")
fig_daily_file <- file.path(base_path, "08_daily_NEE_GPP_Reco_2016_2025.png")
fig_monthly_file <- file.path(base_path, "08_monthly_NEE_GPP_Reco_2016_2025.png")
fig_seasonal_file <- file.path(base_path, "08_seasonal_NEE_GPP_Reco_2016_2025.png")
fig_yearly_file <- file.path(base_path, "08_yearly_NEE_GPP_Reco_2016_2025.png")

# =========================
# 1. Read data
# =========================

partitioned <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 2. Select columns
# =========================

choose_col <- function(data, candidates, label) {
  available <- candidates[candidates %in% names(data)]

  if (length(available) == 0) {
    cat("\nAvailable columns containing", label, ":\n")
    print(grep(label, names(data), value = TRUE))
    stop(paste("No valid column found for", label))
  }

  available[1]
}

nee_col <- choose_col(partitioned, c("NEE_U50_f", "NEE_uStar_f", "NEE_f", "NEE"), "NEE")
gpp_col <- choose_col(partitioned, c("GPP_DT_U50", "GPP_DT_uStar", "GPP_DT"), "GPP")
reco_col <- choose_col(partitioned, c("Reco_DT_U50", "Reco_DT_uStar", "Reco_DT"), "Reco")
rg_col <- choose_col(partitioned, c("Rg_f", "Rg"), "Rg")
tair_col <- choose_col(partitioned, c("Tair_f", "Tair"), "Tair")
vpd_col <- choose_col(partitioned, c("VPD_f", "VPD"), "VPD")

cat("\nSelected columns:\n")
cat("NEE :", nee_col, "\n")
cat("GPP :", gpp_col, "\n")
cat("Reco:", reco_col, "\n")
cat("Rg  :", rg_col, "\n")
cat("Tair:", tair_col, "\n")
cat("VPD :", vpd_col, "\n")

# =========================
# 3. Prepare data
# =========================

partitioned_selected <- partitioned %>%
  mutate(
    DateTime = ymd_hms(DateTime, tz = "Asia/Kuala_Lumpur"),
    Year = year(DateTime),
    Month = month(DateTime),
    DoY = yday(DateTime),
    Hour = hour(DateTime) + minute(DateTime) / 60,
    Date = as.Date(DateTime),

    NEE = as.numeric(.data[[nee_col]]),
    GPP = as.numeric(.data[[gpp_col]]),
    Reco = as.numeric(.data[[reco_col]]),
    Rg = as.numeric(.data[[rg_col]]),
    Tair = as.numeric(.data[[tair_col]]),
    VPD = as.numeric(.data[[vpd_col]])
  ) %>%
  dplyr::filter(
    Year >= 2016,
    Year <= 2025,
    !is.na(DateTime)
  ) %>%
  arrange(DateTime)

cat("\nDate range:\n")
print(range(partitioned_selected$DateTime, na.rm = TRUE))

cat("\nRecords by year:\n")
print(table(partitioned_selected$Year))

# =========================
# 4. Safe cleaning function
# =========================

clean_xy <- function(data, x_col, y_col) {

  d <- data %>%
    dplyr::filter(
      !is.na(.data[[x_col]]),
      !is.na(.data[[y_col]])
    )

  if (nrow(d) < 10) {
    return(d)
  }

  x_low <- quantile(d[[x_col]], 0.005, na.rm = TRUE)
  x_high <- quantile(d[[x_col]], 0.995, na.rm = TRUE)
  y_low <- quantile(d[[y_col]], 0.005, na.rm = TRUE)
  y_high <- quantile(d[[y_col]], 0.995, na.rm = TRUE)

  d %>%
    dplyr::filter(
      .data[[x_col]] >= x_low,
      .data[[x_col]] <= x_high,
      .data[[y_col]] >= y_low,
      .data[[y_col]] <= y_high
    )
}

# =========================
# 5. Half-hourly response plots
# =========================

gpp_rg_data <- partitioned_selected %>%
  dplyr::filter(!is.na(GPP), !is.na(Rg), Rg > 0)

gpp_rg_data <- clean_xy(gpp_rg_data, "Rg", "GPP")

p_gpp_rg <- ggplot(gpp_rg_data, aes(x = Rg, y = GPP)) +
  geom_point(alpha = 0.05, color = "forestgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Radiation Response of GPP (2016–2025)",
    x = expression(R[g]~(W~m^{-2})),
    y = expression(GPP~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_gpp_rg)
ggsave(fig_gpp_rg_file, p_gpp_rg, width = 10, height = 6, dpi = 300)

gpp_vpd_data <- partitioned_selected %>%
  dplyr::filter(!is.na(GPP), !is.na(VPD), !is.na(Rg), Rg > 300, VPD > 0)

gpp_vpd_data <- clean_xy(gpp_vpd_data, "VPD", "GPP")

p_gpp_vpd <- ggplot(gpp_vpd_data, aes(x = VPD, y = GPP)) +
  geom_point(alpha = 0.05, color = "darkred") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Daytime VPD Response of GPP (2016–2025)",
    x = "VPD (hPa)",
    y = expression(GPP~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_gpp_vpd)
ggsave(fig_gpp_vpd_file, p_gpp_vpd, width = 10, height = 6, dpi = 300)

reco_tair_data <- partitioned_selected %>%
  dplyr::filter(!is.na(Reco), !is.na(Tair))

reco_tair_data <- clean_xy(reco_tair_data, "Tair", "Reco")

p_reco_tair <- ggplot(reco_tair_data, aes(x = Tair, y = Reco)) +
  geom_point(alpha = 0.05, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Temperature Response of Reco (2016–2025)",
    x = "Air temperature (°C)",
    y = expression(Reco~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_reco_tair)
ggsave(fig_reco_tair_file, p_reco_tair, width = 10, height = 6, dpi = 300)

nee_rg_data <- partitioned_selected %>%
  dplyr::filter(!is.na(NEE), !is.na(Rg), Rg > 0)

nee_rg_data <- clean_xy(nee_rg_data, "Rg", "NEE")

p_nee_rg <- ggplot(nee_rg_data, aes(x = Rg, y = NEE)) +
  geom_point(alpha = 0.05, color = "forestgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Radiation Response of NEE (2016–2025)",
    x = expression(R[g]~(W~m^{-2})),
    y = expression(NEE~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_nee_rg)
ggsave(fig_nee_rg_file, p_nee_rg, width = 10, height = 6, dpi = 300)

# =========================
# 6. Daily scale
# =========================

daily_flux <- partitioned_selected %>%
  group_by(Year, DoY) %>%
  summarise(
    Date = first(Date),
    Month = first(Month),
    n_records = n(),

    n_valid_NEE = sum(!is.na(NEE)),
    n_valid_GPP = sum(!is.na(GPP)),
    n_valid_Reco = sum(!is.na(Reco)),

    valid_percent_NEE = round(n_valid_NEE / 48 * 100, 2),
    valid_percent_GPP = round(n_valid_GPP / 48 * 100, 2),
    valid_percent_Reco = round(n_valid_Reco / 48 * 100, 2),

    NEE_daily = ifelse(valid_percent_NEE >= 50, mean(NEE, na.rm = TRUE), NA_real_),
    GPP_daily = ifelse(valid_percent_GPP >= 50, mean(GPP, na.rm = TRUE), NA_real_),
    Reco_daily = ifelse(valid_percent_Reco >= 50, mean(Reco, na.rm = TRUE), NA_real_),

    Rg_daily = mean(Rg, na.rm = TRUE),
    Tair_daily = mean(Tair, na.rm = TRUE),
    VPD_daily = mean(VPD, na.rm = TRUE),

    .groups = "drop"
  )

p_daily <- daily_flux %>%
  select(Date, NEE_daily, GPP_daily, Reco_daily) %>%
  pivot_longer(
    cols = c(NEE_daily, GPP_daily, Reco_daily),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = Date, y = Value, color = Variable)) +
  geom_line(linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "Daily NEE, GPP and Reco (2016–2025)",
    x = "Date",
    y = expression(Flux~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_daily)
ggsave(fig_daily_file, p_daily, width = 14, height = 6, dpi = 300)

# =========================
# 7. Monthly scale
# =========================

monthly_flux <- daily_flux %>%
  group_by(Year, Month) %>%
  summarise(
    n_days = n(),
    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    valid_day_percent_NEE = round(valid_days_NEE / n_days * 100, 2),
    valid_day_percent_GPP = round(valid_days_GPP / n_days * 100, 2),
    valid_day_percent_Reco = round(valid_days_Reco / n_days * 100, 2),

    NEE_monthly = ifelse(valid_day_percent_NEE >= 50, mean(NEE_daily, na.rm = TRUE), NA_real_),
    GPP_monthly = ifelse(valid_day_percent_GPP >= 50, mean(GPP_daily, na.rm = TRUE), NA_real_),
    Reco_monthly = ifelse(valid_day_percent_Reco >= 50, mean(Reco_daily, na.rm = TRUE), NA_real_),

    Rg_monthly = mean(Rg_daily, na.rm = TRUE),
    Tair_monthly = mean(Tair_daily, na.rm = TRUE),
    VPD_monthly = mean(VPD_daily, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(Date = as.Date(paste0(Year, "-", Month, "-15")))

p_monthly <- monthly_flux %>%
  select(Date, NEE_monthly, GPP_monthly, Reco_monthly) %>%
  pivot_longer(
    cols = c(NEE_monthly, GPP_monthly, Reco_monthly),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = Date, y = Value, color = Variable)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1) +
  theme_minimal() +
  labs(
    title = "Monthly NEE, GPP and Reco (2016–2025)",
    x = "Date",
    y = expression(Flux~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_monthly)
ggsave(fig_monthly_file, p_monthly, width = 14, height = 6, dpi = 300)

# =========================
# 8. Seasonal scale
# =========================

daily_flux <- daily_flux %>%
  mutate(
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Wet season",
      Month %in% c(4, 5, 6, 7, 8, 9, 10) ~ "Dry / transition season",
      TRUE ~ "Other"
    )
  )

seasonal_flux <- daily_flux %>%
  group_by(Year, Season) %>%
  summarise(
    n_days = n(),
    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    NEE_seasonal = mean(NEE_daily, na.rm = TRUE),
    GPP_seasonal = mean(GPP_daily, na.rm = TRUE),
    Reco_seasonal = mean(Reco_daily, na.rm = TRUE),

    Rg_seasonal = mean(Rg_daily, na.rm = TRUE),
    Tair_seasonal = mean(Tair_daily, na.rm = TRUE),
    VPD_seasonal = mean(VPD_daily, na.rm = TRUE),

    .groups = "drop"
  )

p_seasonal <- seasonal_flux %>%
  select(Year, Season, NEE_seasonal, GPP_seasonal, Reco_seasonal) %>%
  pivot_longer(
    cols = c(NEE_seasonal, GPP_seasonal, Reco_seasonal),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = factor(Year), y = Value, fill = Season)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Seasonal NEE, GPP and Reco (2016–2025)",
    x = "Year",
    y = expression(Flux~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_seasonal)
ggsave(fig_seasonal_file, p_seasonal, width = 14, height = 7, dpi = 300)

# =========================
# 9. Yearly scale
# =========================

yearly_flux <- daily_flux %>%
  group_by(Year) %>%
  summarise(
    n_days = n(),
    valid_days_NEE = sum(!is.na(NEE_daily)),
    valid_days_GPP = sum(!is.na(GPP_daily)),
    valid_days_Reco = sum(!is.na(Reco_daily)),

    NEE_yearly = mean(NEE_daily, na.rm = TRUE),
    GPP_yearly = mean(GPP_daily, na.rm = TRUE),
    Reco_yearly = mean(Reco_daily, na.rm = TRUE),

    Rg_yearly = mean(Rg_daily, na.rm = TRUE),
    Tair_yearly = mean(Tair_daily, na.rm = TRUE),
    VPD_yearly = mean(VPD_daily, na.rm = TRUE),

    .groups = "drop"
  )

p_yearly <- yearly_flux %>%
  select(Year, NEE_yearly, GPP_yearly, Reco_yearly) %>%
  pivot_longer(
    cols = c(NEE_yearly, GPP_yearly, Reco_yearly),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = Year, y = Value, color = Variable)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Yearly NEE, GPP and Reco (2016–2025)",
    x = "Year",
    y = expression(Flux~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_yearly)
ggsave(fig_yearly_file, p_yearly, width = 10, height = 6, dpi = 300)

# =========================
# 10. Correlation and regression
# =========================

cor_test_result <- function(data, x, y) {
  d <- data %>% select(all_of(c(x, y))) %>% drop_na()

  if (nrow(d) < 10) {
    return(tibble(variable_x = x, variable_y = y, pearson_r = NA_real_, p_value = NA_real_, n = nrow(d)))
  }

  test <- cor.test(d[[x]], d[[y]], method = "pearson")

  tibble(
    variable_x = x,
    variable_y = y,
    pearson_r = as.numeric(test$estimate),
    p_value = test$p.value,
    n = nrow(d)
  )
}

correlation_results <- bind_rows(
  cor_test_result(daily_flux, "NEE_daily", "Rg_daily"),
  cor_test_result(daily_flux, "NEE_daily", "Tair_daily"),
  cor_test_result(daily_flux, "NEE_daily", "VPD_daily"),
  cor_test_result(daily_flux, "GPP_daily", "Rg_daily"),
  cor_test_result(daily_flux, "GPP_daily", "Tair_daily"),
  cor_test_result(daily_flux, "GPP_daily", "VPD_daily"),
  cor_test_result(daily_flux, "Reco_daily", "Rg_daily"),
  cor_test_result(daily_flux, "Reco_daily", "Tair_daily"),
  cor_test_result(daily_flux, "Reco_daily", "VPD_daily")
)

regression_result <- function(data, response, predictor) {
  d <- data %>% select(all_of(c(response, predictor))) %>% drop_na()

  if (nrow(d) < 10) {
    return(tibble(response = response, predictor = predictor, intercept = NA_real_, slope = NA_real_, r_squared = NA_real_, adjusted_r_squared = NA_real_, p_value = NA_real_, n = nrow(d)))
  }

  model <- lm(as.formula(paste(response, "~", predictor)), data = d)
  sm <- summary(model)

  tibble(
    response = response,
    predictor = predictor,
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r_squared = sm$r.squared,
    adjusted_r_squared = sm$adj.r.squared,
    p_value = sm$coefficients[2, 4],
    n = nrow(d)
  )
}

regression_results <- bind_rows(
  regression_result(daily_flux, "NEE_daily", "Rg_daily"),
  regression_result(daily_flux, "NEE_daily", "Tair_daily"),
  regression_result(daily_flux, "NEE_daily", "VPD_daily"),
  regression_result(daily_flux, "GPP_daily", "Rg_daily"),
  regression_result(daily_flux, "GPP_daily", "Tair_daily"),
  regression_result(daily_flux, "GPP_daily", "VPD_daily"),
  regression_result(daily_flux, "Reco_daily", "Rg_daily"),
  regression_result(daily_flux, "Reco_daily", "Tair_daily"),
  regression_result(daily_flux, "Reco_daily", "VPD_daily")
)

# =========================
# 11. Save outputs
# =========================

write_csv(partitioned_selected, output_halfhour_file, na = "NA")
write_csv(daily_flux, output_daily_file, na = "NA")
write_csv(monthly_flux, output_monthly_file, na = "NA")
write_csv(seasonal_flux, output_seasonal_file, na = "NA")
write_csv(yearly_flux, output_yearly_file, na = "NA")
write_csv(correlation_results, output_correlation_file, na = "NA")
write_csv(regression_results, output_regression_file, na = "NA")

cat("\n08 completed successfully.\n")
