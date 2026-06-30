# ============================================================
# 06_visualize_gapfilled_nee_2016_2025_UPDATED.R
# Visualize gap-filled NEE after updated 05 REddyProc workflow
# Site: MukaHead
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/reddyproc_filled_2016_2025.csv"

output_daily_file <- "/Users/caixiaoliang/Documents/06_daily_gapfilled_NEE_2016_2025.csv"

output_yearly_qc_file <- "/Users/caixiaoliang/Documents/06_gapfilled_NEE_yearly_QC_2016_2025.csv"

fig_halfhourly_file <- "/Users/caixiaoliang/Documents/06_gapfilled_NEE_timeseries_2016_2025.png"

fig_daily_file <- "/Users/caixiaoliang/Documents/06_daily_gapfilled_NEE_2016_2025.png"

# =========================
# 2. Read gap-filled dataset
# =========================

filled <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0", "")
)

# =========================
# 3. Auto-detect gap-filled NEE column
# =========================

nee_col <- if ("NEE_U50_f" %in% names(filled)) {
  "NEE_U50_f"
} else if ("NEE_uStar_f" %in% names(filled)) {
  "NEE_uStar_f"
} else if ("NEE_f" %in% names(filled)) {
  "NEE_f"
} else {
  stop("Cannot find gap-filled NEE column.")
}

cat("\nSelected gap-filled NEE column:\n")
cat(nee_col, "\n")

required_cols <- c(
  "DateTime",
  "Year",
  "DoY",
  "Hour",
  nee_col
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
    DateTime = as.POSIXct(DateTime, tz = "Asia/Kuala_Lumpur"),
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),
    
    date_only = as.Date(paste0(Year, "-01-01")) + days(DoY - 1),
    Month = month(date_only),
    
    NEE_gapfilled = as.numeric(.data[[nee_col]])
  ) %>%
  filter(
    !is.na(DateTime),
    Year >= 2016,
    Year <= 2025
  ) %>%
  arrange(DateTime)

# =========================
# 5. Basic check
# =========================

cat("\n===== Data range =====\n")
print(range(filled$DateTime, na.rm = TRUE))

cat("\n===== Number of records =====\n")
print(nrow(filled))

cat("\n===== Records by year =====\n")
print(table(filled$Year))

cat("\n===== Missing NEE_gapfilled percentage =====\n")
print(round(mean(is.na(filled$NEE_gapfilled)) * 100, 2))

cat("\n===== NEE summary =====\n")
print(summary(filled$NEE_gapfilled))

# =========================
# 6. Yearly QC table
# =========================

yearly_qc <- filled %>%
  group_by(Year) %>%
  summarise(
    rows = n(),
    valid_NEE = sum(!is.na(NEE_gapfilled)),
    missing_NEE = sum(is.na(NEE_gapfilled)),
    valid_percent = round(valid_NEE / rows * 100, 2),
    .groups = "drop"
  )

cat("\n===== Yearly NEE gap-filled QC =====\n")
print(yearly_qc)

write_csv(
  yearly_qc,
  output_yearly_qc_file,
  na = "NA"
)

# =========================
# 7. Half-hourly gap-filled NEE time series
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
# 8. Daily mean gap-filled NEE
# =========================

nee_daily <- filled %>%
  group_by(Year, DoY) %>%
  summarise(
    date_only = first(date_only),
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
# 9. Daily mean plot
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
# 10. Save daily dataset
# =========================

write_csv(
  nee_daily,
  output_daily_file,
  na = "NA"
)

# =========================
# 11. Finished
# =========================

cat("\n06 visualization completed successfully.\n")
cat("Selected NEE column:\n")
cat(nee_col, "\n")
cat("Daily dataset saved as:\n")
cat(output_daily_file, "\n")
cat("Yearly QC saved as:\n")
cat(output_yearly_qc_file, "\n")
cat("Half-hourly figure saved as:\n")
cat(fig_halfhourly_file, "\n")
cat("Daily figure saved as:\n")
cat(fig_daily_file, "\n")
