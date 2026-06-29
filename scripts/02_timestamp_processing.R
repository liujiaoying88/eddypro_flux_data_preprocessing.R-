# ============================================================
# 02_timestamp_processing.R
# Timestamp processing for 2016–2025 merged EddyPro dataset
# ============================================================

# =========================
# 1. Load packages
# =========================

library(tidyverse)
library(lubridate)

# =========================
# 2. Set file paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_MERGED_clean.csv"

output_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_datetime.csv"

# =========================
# 3. Read merged dataset
# =========================

flux <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 4. Basic column check
# =========================

cat("Column names in the dataset:\n")
print(names(flux)[1:20])

if (!"TIME_KEY" %in% names(flux)) {
  stop("TIME_KEY column was not found. Please check the merged dataset.")
}

# =========================
# 5. Convert timestamp
# =========================
# Current TIME_KEY format should be:
# 2016-01-01 00:30

flux <- flux %>%
  mutate(
    datetime = ymd_hm(TIME_KEY, tz = "Asia/Kuala_Lumpur")
  )

# =========================
# 6. Check timestamp conversion
# =========================

cat("\nDatetime summary:\n")
print(summary(flux$datetime))

cat("\nNumber of missing datetime values:\n")
print(sum(is.na(flux$datetime)))

if (sum(is.na(flux$datetime)) > 0) {
  warning("Some datetime values could not be converted. Please check TIME_KEY format.")
}

# =========================
# 7. Extract temporal variables
# =========================

flux <- flux %>%
  mutate(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime),
    minute = minute(datetime),
    decimal_hour = hour + minute / 60,
    date_only = as.Date(datetime),
    doy = yday(datetime),
    week = isoweek(datetime),
    month_name = month(datetime, label = TRUE, abbr = TRUE),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Northeast_monsoon",
      month %in% c(3, 4, 5) ~ "Inter_monsoon_1",
      month %in% c(6, 7, 8, 9) ~ "Southwest_monsoon",
      month %in% c(10, 11) ~ "Inter_monsoon_2",
      TRUE ~ NA_character_
    )
  )

# =========================
# 8. Check year range
# =========================

cat("\nYear range:\n")
print(range(flux$year, na.rm = TRUE))

cat("\nRecords by year:\n")
print(table(flux$year))

# =========================
# 9. Check duplicated datetime
# =========================

cat("\nDuplicated datetime records:\n")
print(sum(duplicated(flux$datetime)))

# =========================
# 10. Sort data by datetime
# =========================

flux <- flux %>%
  arrange(datetime)

# =========================
# 11. Save updated dataset
# =========================

write_csv(
  flux,
  output_file,
  na = "NA"
)

cat("\nTimestamp processing completed.\n")
cat("Output file saved to:\n")
cat(output_file, "\n")
