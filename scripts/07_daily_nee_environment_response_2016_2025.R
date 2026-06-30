# ============================================================
# 07_daily_NEE_environment_response_2016_2025.R
# Daily Gap-filled NEE and Environmental Response Analysis
# Site: MukaHead
# Author: Cai Xiaoliang
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/reddyproc_filled_2016_2025.csv"

output_daily_file <- "/Users/caixiaoliang/Documents/07_daily_gapfilled_NEE_environment_2016_2025.csv"
output_yearly_qc_file <- "/Users/caixiaoliang/Documents/07_daily_NEE_environment_yearly_QC_2016_2025.csv"

fig_daily_nee_file <- "/Users/caixiaoliang/Documents/07_daily_gapfilled_NEE_2016_2025.png"
fig_nee_vpd_file <- "/Users/caixiaoliang/Documents/07_NEE_vs_VPD_daily_2016_2025.png"
fig_nee_tair_file <- "/Users/caixiaoliang/Documents/07_NEE_vs_Tair_daily_2016_2025.png"
fig_nee_rg_file <- "/Users/caixiaoliang/Documents/07_NEE_vs_Rg_daily_2016_2025.png"

# =========================
# 2. Read data
# =========================

filled <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0", "-9999.00")
)

# =========================
# 3. Check required columns
# =========================

required_cols <- c(
  "Year", "DoY", "Hour",
  "NEE_uStar_f",
  "VPD_f",
  "Tair_f",
  "Rg_f"
)

missing_cols <- setdiff(required_cols, names(filled))

if (length(missing_cols) > 0) {
  cat("\nMissing columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

# =========================
# 4. Prepare variables
# =========================

filled <- filled %>%
  mutate(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),

    Date = as.Date(paste0(Year, "-01-01")) + days(DoY - 1),
    Month = month(Date),

    NEE = as.numeric(NEE_uStar_f),
    VPD = as.numeric(VPD_f),
    Tair = as.numeric(Tair_f),
    Rg = as.numeric(Rg_f)
  ) %>%
  filter(
    Year >= 2016,
    Year <= 2025,
    !is.na(Year),
    !is.na(DoY),
    !is.na(Hour)
  ) %>%
  arrange(Year, DoY, Hour)

# =========================
# 5. Basic QC
# =========================

cat("\n===== Records by year =====\n")
print(table(filled$Year))

cat("\n===== Half-hourly records per day check =====\n")
print(summary(filled %>% count(Year, DoY) %>% pull(n)))

cat("\n===== NEE summary =====\n")
print(summary(filled$NEE))

cat("\n===== VPD summary =====\n")
print(summary(filled$VPD))

cat("\n===== Tair summary =====\n")
print(summary(filled$Tair))

cat("\n===== Rg summary =====\n")
print(summary(filled$Rg))

# =========================
# 6. Daily aggregation
# =========================

filled_daily <- filled %>%
  group_by(Year, DoY) %>%
  summarise(
    Date = first(Date),
    Month = first(Month),

    n_records = n(),
    n_valid_NEE = sum(!is.na(NEE)),
    valid_percent = round(n_valid_NEE / 48 * 100, 2),

    NEE_daily = ifelse(
      valid_percent >= 50,
      mean(NEE, na.rm = TRUE),
      NA_real_
    ),

    NEE_daily_sd = ifelse(
      valid_percent >= 50,
      sd(NEE, na.rm = TRUE),
      NA_real_
    ),

    VPD_daily = ifelse(
      sum(!is.na(VPD)) >= 24,
      mean(VPD, na.rm = TRUE),
      NA_real_
    ),

    Tair_daily = ifelse(
      sum(!is.na(Tair)) >= 24,
      mean(Tair, na.rm = TRUE),
      NA_real_
    ),

    Rg_daily = ifelse(
      sum(!is.na(Rg)) >= 24,
      mean(Rg, na.rm = TRUE),
      NA_real_
    ),

    .groups = "drop"
  ) %>%
  filter(
    n_records >= 40
  )

# =========================
# 7. Yearly QC
# =========================

yearly_qc <- filled_daily %>%
  group_by(Year) %>%
  summarise(
    days = n(),
    valid_days_NEE = sum(!is.na(NEE_daily)),
    missing_days_NEE = sum(is.na(NEE_daily)),
    valid_day_percent = round(valid_days_NEE / days * 100, 2),
    mean_NEE = mean(NEE_daily, na.rm = TRUE),
    sd_NEE = sd(NEE_daily, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n===== Yearly daily QC =====\n")
print(yearly_qc, n = Inf)

write_csv(
  yearly_qc,
  output_yearly_qc_file,
  na = "NA"
)

# =========================
# 8. Save daily dataset
# =========================

write_csv(
  filled_daily,
  output_daily_file,
  na = "NA"
)

# =========================
# 9. Daily NEE time series
# =========================

p_daily_nee <- ggplot(
  filled_daily,
  aes(x = Date, y = NEE_daily)
) +
  geom_line(color = "darkred", linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "2016–2025 Daily Gap-filled NEE",
    x = "Date",
    y = expression(Daily~NEE~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_daily_nee)

ggsave(
  fig_daily_nee_file,
  plot = p_daily_nee,
  width = 14,
  height = 6,
  dpi = 300
)

# =========================
# 10. Daily NEE response to VPD
# =========================

p_nee_vpd <- ggplot(
  filled_daily %>%
    filter(
      !is.na(NEE_daily),
      !is.na(VPD_daily)
    ),
  aes(x = VPD_daily, y = NEE_daily)
) +
  geom_point(alpha = 0.35, color = "darkred", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Daily NEE Response to VPD (2016–2025)",
    x = "Daily mean VPD (hPa)",
    y = expression(Daily~NEE~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_nee_vpd)

ggsave(
  fig_nee_vpd_file,
  plot = p_nee_vpd,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 11. Daily NEE response to air temperature
# =========================

p_nee_tair <- ggplot(
  filled_daily %>%
    filter(
      !is.na(NEE_daily),
      !is.na(Tair_daily)
    ),
  aes(x = Tair_daily, y = NEE_daily)
) +
  geom_point(alpha = 0.35, color = "blue", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Daily NEE Response to Air Temperature (2016–2025)",
    x = "Daily mean air temperature (°C)",
    y = expression(Daily~NEE~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_nee_tair)

ggsave(
  fig_nee_tair_file,
  plot = p_nee_tair,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 12. Daily NEE response to radiation
# =========================

p_nee_rg <- ggplot(
  filled_daily %>%
    filter(
      !is.na(NEE_daily),
      !is.na(Rg_daily)
    ),
  aes(x = Rg_daily, y = NEE_daily)
) +
  geom_point(alpha = 0.35, color = "darkgreen", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Daily NEE Response to Radiation (2016–2025)",
    x = expression(Daily~mean~R[g]~(W~m^{-2})),
    y = expression(Daily~NEE~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_nee_rg)

ggsave(
  fig_nee_rg_file,
  plot = p_nee_rg,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 13. Finished
# =========================

cat("\n07 completed successfully.\n")
cat("Daily dataset saved as:\n")
cat(output_daily_file, "\n")
cat("Yearly QC saved as:\n")
cat(output_yearly_qc_file, "\n")
cat("Figures saved:\n")
cat(fig_daily_nee_file, "\n")
cat(fig_nee_vpd_file, "\n")
cat(fig_nee_tair_file, "\n")
cat(fig_nee_rg_file, "\n")
