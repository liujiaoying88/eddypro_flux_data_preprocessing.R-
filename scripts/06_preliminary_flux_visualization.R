# ============================================================
# 06_preliminary_flux_visualization.R
# Preliminary CO2 flux visualization for 2016–2025 Muka Head data
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_qc.csv"

output_daily_file <- "/Users/caixiaoliang/Documents/daily_co2_flux_2016_2025.csv"

fig_halfhourly_file <- "/Users/caixiaoliang/Documents/co2_flux_timeseries_2016_2025.png"

fig_daily_file <- "/Users/caixiaoliang/Documents/daily_co2_flux_2016_2025.png"

# =========================
# 2. Read QC dataset
# =========================

flux <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 3. Prepare variables
# =========================

flux <- flux %>%
  mutate(
    datetime = as.POSIXct(datetime),
    date_only = as.Date(datetime),
    year = year(datetime),
    month = month(datetime),
    co2_flux = as.numeric(co2_flux)
  ) %>%
  filter(!is.na(datetime))

# =========================
# 4. Basic check
# =========================

cat("\n===== Data range =====\n")
print(range(flux$datetime, na.rm = TRUE))

cat("\n===== Number of records =====\n")
print(nrow(flux))

cat("\n===== Missing CO2 flux percentage =====\n")
print(mean(is.na(flux$co2_flux)) * 100)

cat("\n===== Records by year =====\n")
print(table(flux$year))

# =========================
# 5. Half-hourly CO2 flux time series
# =========================

p_halfhourly <- ggplot(
  flux,
  aes(x = datetime, y = co2_flux)
) +
  geom_line(color = "blue", linewidth = 0.15) +
  theme_minimal() +
  labs(
    title = "2016–2025 Half-hourly CO2 Flux Time Series",
    x = "Date",
    y = expression(CO[2]~flux~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_halfhourly)

ggsave(
  fig_halfhourly_file,
  plot = p_halfhourly,
  width = 14,
  height = 6,
  dpi = 300
)

# =========================
# 6. Daily mean dataset
# =========================

flux_daily <- flux %>%
  group_by(date_only) %>%
  summarise(
    year = first(year),
    month = first(month),
    n_records = n(),
    n_valid_co2_flux = sum(!is.na(co2_flux)),
    valid_percent = round(n_valid_co2_flux / n_records * 100, 2),
    co2_flux_daily_mean = ifelse(
      valid_percent >= 50,
      mean(co2_flux, na.rm = TRUE),
      NA_real_
    ),
    co2_flux_daily_sd = ifelse(
      valid_percent >= 50,
      sd(co2_flux, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

# =========================
# 7. Daily mean CO2 flux plot
# =========================

p_daily <- ggplot(
  flux_daily,
  aes(x = date_only, y = co2_flux_daily_mean)
) +
  geom_line(color = "red", linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "2016–2025 Daily Mean CO2 Flux",
    x = "Date",
    y = expression(Daily~mean~CO[2]~flux~(mu*mol~m^{-2}~s^{-1}))
  )

print(p_daily)

ggsave(
  fig_daily_file,
  plot = p_daily,
  width = 14,
  height = 6,
  dpi = 300
)

# =========================
# 8. Save daily dataset
# =========================

write_csv(
  flux_daily,
  output_daily_file,
  na = "NA"
)

# =========================
# 9. Finished
# =========================

cat("\nPreliminary flux visualization completed successfully.\n")
cat("Daily dataset saved as:\n")
cat(output_daily_file, "\n")
cat("Half-hourly figure saved as:\n")
cat(fig_halfhourly_file, "\n")
cat("Daily figure saved as:\n")
cat(fig_daily_file, "\n")
