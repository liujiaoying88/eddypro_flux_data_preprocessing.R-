# ============================================================
# 06_visualize_gapfilled_nee_2016_2025.R
# Visualize gap-filled NEE for 2016–2025 Muka Head data
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/reddyproc_filled_2016_2025.csv"

output_daily_file <- "/Users/caixiaoliang/Documents/daily_gapfilled_NEE_2016_2025.csv"

fig_halfhourly_file <- "/Users/caixiaoliang/Documents/gapfilled_NEE_timeseries_2016_2025.png"

fig_daily_file <- "/Users/caixiaoliang/Documents/daily_gapfilled_NEE_2016_2025.png"

# =========================
# 2. Read gap-filled dataset
# =========================

filled <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 3. Check required columns
# =========================

required_cols <- c(
  "DateTime",
  "Year",
  "DoY",
  "Hour",
  "NEE_uStar_f"
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
    DateTime = ymd_hms(DateTime, tz = "Asia/Kuala_Lumpur"),
    date_only = as.Date(DateTime),
    Year = as.integer(Year),
    Month = month(DateTime),
    NEE_gapfilled = as.numeric(NEE_uStar_f)
  ) %>%
  filter(!is.na(DateTime)) %>%
  arrange(DateTime)

# =========================
# 5. Basic check
# =========================

cat("\n===== Data range =====\n")
print(range(filled$DateTime, na.rm = TRUE))

cat("\n===== Number of records =====\n")
print(nrow(filled))

cat("\n===== Missing NEE_gapfilled percentage =====\n")
print(mean(is.na(filled$NEE_gapfilled)) * 100)

cat("\n===== Records by year =====\n")
print(table(filled$Year))

cat("\n===== NEE summary =====\n")
print(summary(filled$NEE_gapfilled))

# =========================
# 6. Half-hourly gap-filled NEE time series
# =========================

p_halfhourly <- ggplot(
  filled,
  aes(x = DateTime, y = NEE_gapfilled)
) +
  geom_line(color = "darkgreen", linewidth = 0.15) +
  theme_minimal() +
  labs(
    title = "2016–2025 Gap-filled NEE Time Series",
    x = "Date",
    y = expression(NEE~(mu*mol~m^{-2}~s^{-1}))
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
# 7. Daily mean gap-filled NEE
# =========================

nee_daily <- filled %>%
  group_by(date_only) %>%
  summarise(
    Year = first(Year),
    Month = first(Month),
    n_records = n(),
    n_valid_NEE = sum(!is.na(NEE_gapfilled)),
    valid_percent = round(n_valid_NEE / n_records * 100, 2),
    NEE_daily_mean = ifelse(
      valid_percent >= 50,
      mean(NEE_gapfilled, na.rm = TRUE),
      NA_real_
    ),
    NEE_daily_sd = ifelse(
      valid_percent >= 50,
      sd(NEE_gapfilled, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

# =========================
# 8. Daily mean plot
# =========================

p_daily <- ggplot(
  nee_daily,
  aes(x = date_only, y = NEE_daily_mean)
) +
  geom_line(color = "red", linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "2016–2025 Daily Mean Gap-filled NEE",
    x = "Date",
    y = expression(Daily~mean~NEE~(mu*mol~m^{-2}~s^{-1}))
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
# 9. Save daily dataset
# =========================

write_csv(
  nee_daily,
  output_daily_file,
  na = "NA"
)

# =========================
# 10. Finished
# =========================

cat("\n06 visualization completed successfully.\n")
cat("Daily dataset saved as:\n")
cat(output_daily_file, "\n")
cat("Half-hourly figure saved as:\n")
cat(fig_halfhourly_file, "\n")
cat("Daily figure saved as:\n")
cat(fig_daily_file, "\n")
