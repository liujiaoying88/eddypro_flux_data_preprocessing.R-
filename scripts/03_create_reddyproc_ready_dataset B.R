# ============================================================
# 03_create_reddyproc_ready_dataset.R
# Create REddyProc-ready Dataset for 2016–2025 Muka Head data
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_datetime.csv"

output_file <- "/Users/caixiaoliang/Documents/reddyproc_ready_mukahead_2016_2025.csv"

# =========================
# 2. Read timestamp-processed dataset
# =========================

flux <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 3. Define variable mapping
# =========================

nee_col <- "co2_flux"
rg_col <- "RG_1_1_1"
tair_col <- "TA_1_1_1"
vpd_col <- "VPD"
ustar_col <- "u*"

required_cols <- c(
  "datetime",
  "year",
  "doy",
  "decimal_hour",
  nee_col,
  rg_col,
  tair_col,
  vpd_col,
  ustar_col
)

missing_cols <- setdiff(
  required_cols,
  names(flux)
)

if (length(missing_cols) > 0) {
  cat("\nMissing columns:\n")
  print(missing_cols)
  
  cat("\nPossible alternative columns:\n")
  print(
    names(flux)[
      grepl(
        "co2|FC|NEE|RG|SW|TA|air_temperature|VPD|USTAR|u\\*|Tair",
        names(flux),
        ignore.case = TRUE
      )
    ]
  )
  
  stop("Some required columns are missing.")
}

# =========================
# 4. Create REddyProc-ready dataset
# =========================

reddyproc_ready <- flux %>%
  transmute(
    Year = as.integer(year),
    DoY = as.numeric(doy),
    Hour = round(as.numeric(decimal_hour), 1),
    
    NEE = parse_number(as.character(.data[[nee_col]])),
    Rg = parse_number(as.character(.data[[rg_col]])),
    Tair = parse_number(as.character(.data[[tair_col]])),
    VPD = parse_number(as.character(.data[[vpd_col]])),
    Ustar = parse_number(as.character(.data[[ustar_col]])),
    
    datetime = ymd_hms(datetime, tz = "Asia/Kuala_Lumpur")
  )

# =========================
# 5. Unit conversion
# =========================

# Tair: Kelvin -> Celsius
if (median(reddyproc_ready$Tair, na.rm = TRUE) > 100) {
  reddyproc_ready <- reddyproc_ready %>%
    mutate(
      Tair = Tair - 273.15
    )
  
  cat("\nTair converted from Kelvin to Celsius.\n")
}

# VPD: Pa -> hPa
if (median(reddyproc_ready$VPD, na.rm = TRUE) > 100) {
  reddyproc_ready <- reddyproc_ready %>%
    mutate(
      VPD = VPD / 100
    )
  
  cat("\nVPD converted from Pa to hPa.\n")
}

# =========================
# 6. Clean invalid records
# =========================

reddyproc_ready <- reddyproc_ready %>%
  filter(
    !is.na(Year),
    !is.na(DoY),
    !is.na(Hour),
    !is.na(datetime)
  ) %>%
  arrange(datetime)

# =========================
# 7. Check timestamp continuity
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
# 8. Check duplicated timestamps
# =========================

duplicate_count <- sum(
  duplicated(reddyproc_ready$datetime)
)

cat("\n===== Duplicated timestamps =====\n")
cat("Duplicated timestamp count:", duplicate_count, "\n")

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
