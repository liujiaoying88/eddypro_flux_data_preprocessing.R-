# ============================================================
# 03B_create_reddyproc_ready_dataset.R
# Create REddyProc-ready Dataset from QC-cleaned 2016–2025 data
# Site: Muka Head
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_qc.csv"

output_file <- "/Users/caixiaoliang/Documents/reddyproc_ready_mukahead_2016_2025.csv"

# =========================
# 2. Read QC-cleaned dataset
# =========================

flux <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 3. Check required columns
# =========================

required_cols <- c(
  "datetime",
  "year",
  "doy",
  "decimal_hour",
  "co2_flux",
  "Rg",
  "Tair",
  "VPD",
  "Ustar"
)

missing_cols <- setdiff(required_cols, names(flux))

if (length(missing_cols) > 0) {
  cat("\nMissing columns:\n")
  print(missing_cols)
  stop("Some required columns are missing from the QC dataset.")
}

# =========================
# 4. Create REddyProc-ready dataset
# =========================

reddyproc_ready <- flux %>%
  transmute(
    datetime = ymd_hms(datetime, tz = "Asia/Kuala_Lumpur"),
    
    Year = as.integer(year),
    DoY = as.integer(doy),
    Hour = round(as.numeric(decimal_hour), 1),
    
    NEE = as.numeric(co2_flux),
    Rg = as.numeric(Rg),
    Tair = as.numeric(Tair),
    VPD = as.numeric(VPD),
    Ustar = as.numeric(Ustar)
  ) %>%
  mutate(
    # Nighttime very small radiation values are set to 0
    Rg = ifelse(!is.na(Rg) & Rg < 1, 0, Rg)
  )

# =========================
# 5. Remove invalid time records
# =========================

reddyproc_ready <- reddyproc_ready %>%
  filter(
    !is.na(datetime),
    !is.na(Year),
    !is.na(DoY),
    !is.na(Hour)
  ) %>%
  arrange(datetime)

# =========================
# 6. Check timestamp continuity
# =========================

full_time <- tibble(
  datetime = seq(
    from = min(reddyproc_ready$datetime, na.rm = TRUE),
    to = max(reddyproc_ready$datetime, na.rm = TRUE),
    by = "30 min"
  )
)

missing_rows <- full_time %>%
  anti_join(
    reddyproc_ready,
    by = "datetime"
  )

cat("\n===== Missing timestamps =====\n")
cat("Missing timestamp count:", nrow(missing_rows), "\n")

if (nrow(missing_rows) > 0) {
  print(head(missing_rows, 20))
}

# =========================
# 7. Check duplicated timestamps
# =========================

duplicate_count <- sum(
  duplicated(reddyproc_ready$datetime)
)

cat("\n===== Duplicated timestamps =====\n")
cat("Duplicated timestamp count:", duplicate_count, "\n")

# =========================
# 8. Check Hour range
# =========================

cat("\n===== Hour range =====\n")
print(range(reddyproc_ready$Hour, na.rm = TRUE))

cat("\n===== Unique Hour values =====\n")
print(sort(unique(reddyproc_ready$Hour)))

# =========================
# 9. Final REddyProc export
# =========================

reddyproc_ready_export <- reddyproc_ready %>%
  transmute(
    Year = Year,
    DoY = DoY,
    Hour = Hour,
    NEE = round(NEE, 4),
    Rg = round(Rg, 2),
    Tair = round(Tair, 2),
    VPD = round(VPD, 2),
    Ustar = round(Ustar, 4)
  )

# =========================
# 10. Summary check
# =========================

cat("\n===== Dataset structure =====\n")
glimpse(reddyproc_ready_export)

cat("\n===== Summary =====\n")
print(summary(reddyproc_ready_export))

cat("\n===== Records by year =====\n")
print(table(reddyproc_ready_export$Year))

cat("\n===== Missing percentage in REddyProc-ready dataset =====\n")

missing_summary <- reddyproc_ready_export %>%
  summarise(
    across(
      everything(),
      ~ mean(is.na(.)) * 100
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_percent"
  )

print(missing_summary)

# =========================
# 11. Save dataset
# =========================

write_csv(
  reddyproc_ready_export,
  output_file,
  na = "NA"
)

# =========================
# 12. Finished
# =========================

cat("\nREddyProc-ready dataset created successfully.\n")
cat("Saved file:\n")
cat(output_file, "\n")
