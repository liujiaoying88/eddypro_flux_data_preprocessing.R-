# ============================================================
# 04_prepare_reddyproc_input_2016_2025.R
# Prepare complete half-hourly REddyProc input dataset
# Site: Muka Head
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

input_file <- "/Users/caixiaoliang/Documents/reddyproc_ready_mukahead_2016_2025.csv"

output_file <- "/Users/caixiaoliang/Documents/reddyproc_input_mukahead_2016_2025.csv"

missing_report_file <- "/Users/caixiaoliang/Documents/reddyproc_input_missing_report_2016_2025.csv"

flux <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

required_cols <- c("Year", "DoY", "Hour", "NEE", "Rg", "Tair", "VPD", "Ustar")

missing_cols <- setdiff(required_cols, names(flux))

if (length(missing_cols) > 0) {
  cat("\nMissing columns:\n")
  print(missing_cols)
  stop("Some required columns are missing.")
}

flux_core <- flux %>%
  transmute(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),
    NEE = as.numeric(NEE),
    Rg = as.numeric(Rg),
    Tair = as.numeric(Tair),
    VPD = as.numeric(VPD),
    Ustar = as.numeric(Ustar)
  ) %>%
  mutate(
    NEE = ifelse(NEE < -50 | NEE > 50, NA_real_, NEE),
    Rg = ifelse(Rg < 0, NA_real_, Rg),
    Rg = ifelse(!is.na(Rg) & Rg < 1, 0, Rg),
    Tair = ifelse(Tair < 10 | Tair > 45, NA_real_, Tair),
    VPD = ifelse(VPD < 0 | VPD > 60, NA_real_, VPD),
    Ustar = ifelse(Ustar < 0 | Ustar > 3, NA_real_, Ustar)
  ) %>%
  filter(
    Year >= 2016,
    Year <= 2025,
    DoY >= 1,
    DoY <= 366,
    Hour >= 0.5,
    Hour <= 24
  ) %>%
  arrange(Year, DoY, Hour)

dup_count <- flux_core %>%
  count(Year, DoY, Hour) %>%
  filter(n > 1) %>%
  nrow()

cat("\nDuplicated Year-DoY-Hour groups before cleaning:\n")
print(dup_count)

flux_core <- flux_core %>%
  group_by(Year, DoY, Hour) %>%
  summarise(
    across(
      c(NEE, Rg, Tair, VPD, Ustar),
      ~ ifelse(
        all(is.na(.)),
        NA_real_,
        mean(., na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )

years <- 2016:2025

time_grid <- map_dfr(
  years,
  function(y) {
    n_days <- ifelse(leap_year(y), 366, 365)
    
    expand_grid(
      Year = y,
      DoY = 1:n_days,
      Hour = seq(0.5, 24, by = 0.5)
    )
  }
) %>%
  arrange(Year, DoY, Hour)

flux_reddyproc <- time_grid %>%
  left_join(
    flux_core,
    by = c("Year", "DoY", "Hour")
  ) %>%
  arrange(Year, DoY, Hour)

missing_report <- flux_reddyproc %>%
  group_by(Year) %>%
  summarise(
    n_records = n(),
    missing_NEE_percent = mean(is.na(NEE)) * 100,
    missing_Rg_percent = mean(is.na(Rg)) * 100,
    missing_Tair_percent = mean(is.na(Tair)) * 100,
    missing_VPD_percent = mean(is.na(VPD)) * 100,
    missing_Ustar_percent = mean(is.na(Ustar)) * 100,
    .groups = "drop"
  )

write_csv(
  missing_report,
  missing_report_file,
  na = "NA"
)

cat("\n===== REddyProc input check =====\n")

cat("\nRows:\n")
print(nrow(flux_reddyproc))

expected_rows <- sum(
  ifelse(
    leap_year(years),
    366 * 48,
    365 * 48
  )
)

cat("\nExpected rows:\n")
print(expected_rows)

cat("\nYear table:\n")
print(table(flux_reddyproc$Year))

cat("\nDoY range by year:\n")
print(
  flux_reddyproc %>%
    group_by(Year) %>%
    summarise(
      min_DoY = min(DoY),
      max_DoY = max(DoY),
      n_rows = n(),
      .groups = "drop"
    )
)

cat("\nHour range:\n")
print(range(flux_reddyproc$Hour, na.rm = TRUE))

cat("\nUnique Hour values:\n")
print(sort(unique(flux_reddyproc$Hour)))

cat("\nDuplicated Year-DoY-Hour after grid merge:\n")
print(
  sum(
    duplicated(
      flux_reddyproc %>%
        select(Year, DoY, Hour)
    )
  )
)

cat("\nMissing values:\n")
print(colSums(is.na(flux_reddyproc)))

cat("\nMissing report by year:\n")
print(missing_report)

cat("\nVariable summary:\n")
print(summary(flux_reddyproc))

write_csv(
  flux_reddyproc,
  output_file,
  na = "NA"
)

cat("\n===== 04 completed successfully =====\n")
cat("Saved file:\n")
cat(output_file, "\n")
cat("\nMissing report saved file:\n")
cat(missing_report_file, "\n")
