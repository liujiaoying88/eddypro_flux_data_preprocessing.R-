# ============================================================
# 07_daily_nee_environment_response_2016_2025.R
# Daily Gap-filled NEE and Environmental Response Analysis
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

input_file <- "/Users/caixiaoliang/Documents/reddyproc_filled_2016_2025.csv"

output_daily_file <- "/Users/caixiaoliang/Documents/daily_gapfilled_NEE_environment_2016_2025.csv"

fig_daily_nee_file <- "/Users/caixiaoliang/Documents/07_daily_gapfilled_NEE_2016_2025.png"
fig_nee_vpd_file <- "/Users/caixiaoliang/Documents/07_NEE_vs_VPD_daily_2016_2025.png"
fig_nee_tair_file <- "/Users/caixiaoliang/Documents/07_NEE_vs_Tair_daily_2016_2025.png"
fig_nee_rg_file <- "/Users/caixiaoliang/Documents/07_NEE_vs_Rg_daily_2016_2025.png"

filled <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

required_cols <- c(
  "Year", "DoY", "Hour",
  "NEE_uStar_f", "VPD_f", "Tair_f", "Rg_f"
)

missing_cols <- setdiff(required_cols, names(filled))

if (length(missing_cols) > 0) {
  print(missing_cols)
  stop("Required columns are missing.")
}

filled <- filled %>%
  mutate(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),
    date = as.Date(paste0(Year, "-01-01")) + days(DoY - 1),
    NEE = as.numeric(NEE_uStar_f),
    VPD = as.numeric(VPD_f),
    Tair = as.numeric(Tair_f),
    Rg = as.numeric(Rg_f)
  ) %>%
  filter(
    Year >= 2016,
    Year <= 2025,
    !is.na(DoY),
    !is.na(Hour)
  )

cat("\n===== Records by year =====\n")
print(table(filled$Year))

cat("\n===== Half-hourly records per day check =====\n")
print(summary(filled %>% count(Year, DoY) %>% pull(n)))

filled_daily <- filled %>%
  group_by(Year, DoY) %>%
  summarise(
    date = first(date),
    Month = month(first(date)),
    n_records = n(),
    n_valid_NEE = sum(!is.na(NEE)),
    valid_percent = round(n_valid_NEE / 48 * 100, 2),
    NEE_daily = ifelse(valid_percent >= 50, mean(NEE, na.rm = TRUE), NA_real_),
    VPD_daily = mean(VPD, na.rm = TRUE),
    Tair_daily = mean(Tair, na.rm = TRUE),
    Rg_daily = mean(Rg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_records >= 40)

p_daily_nee <- ggplot(filled_daily, aes(x = date, y = NEE_daily)) +
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

p_nee_vpd <- ggplot(
  filled_daily %>% filter(!is.na(NEE_daily), !is.na(VPD_daily)),
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

p_nee_tair <- ggplot(
  filled_daily %>% filter(!is.na(NEE_daily), !is.na(Tair_daily)),
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

p_nee_rg <- ggplot(
  filled_daily %>% filter(!is.na(NEE_daily), !is.na(Rg_daily)),
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

write_csv(
  filled_daily,
  output_daily_file,
  na = "NA"
)

cat("\n07 completed successfully.\n")
cat("Daily dataset saved as:\n")
cat(output_daily_file, "\n")
