# ============================================================
# 03A_flux_quality_control_2016_2025.R
# Flux Quality Control for Muka Head 2016–2025 merged dataset
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 1. File paths
# =========================

input_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_datetime.csv"

output_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_qc.csv"

qc_report_file <- "/Users/caixiaoliang/Documents/qc_report_2016_2025.csv"

fig_raw_file <- "/Users/caixiaoliang/Documents/co2_flux_raw_2016_2025.png"

fig_qc_file <- "/Users/caixiaoliang/Documents/co2_flux_qc_cleaned_2016_2025.png"

# =========================
# 2. Read dataset
# =========================

flux_raw <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 3. Variable mapping
# =========================

flux <- flux_raw %>%
  mutate(
    datetime = ymd_hms(datetime, tz = "Asia/Kuala_Lumpur"),
    
    co2_flux = parse_number(as.character(co2_flux)),
    LE = parse_number(as.character(LE)),
    H = parse_number(as.character(H)),
    Ustar = parse_number(as.character(`u*`)),
    Tair = parse_number(as.character(TA_1_1_1)),
    RH = parse_number(as.character(RH_1_1_1)),
    VPD = parse_number(as.character(VPD)),
    Rg = parse_number(as.character(RG_1_1_1))
  )

# =========================
# 4. Unit conversion
# =========================

# Tair: Kelvin to Celsius
if (median(flux$Tair, na.rm = TRUE) > 100) {
  flux <- flux %>%
    mutate(Tair = Tair - 273.15)
}

# VPD: Pa to hPa
if (median(flux$VPD, na.rm = TRUE) > 100) {
  flux <- flux %>%
    mutate(VPD = VPD / 100)
}

# =========================
# 5. Basic checks
# =========================

cat("===== Timestamp range =====\n")
print(range(flux$datetime, na.rm = TRUE))

cat("\n===== Duplicate timestamps =====\n")
print(sum(duplicated(flux$datetime)))

cat("\n===== Number of records =====\n")
print(nrow(flux))

cat("\n===== Records by year =====\n")
print(table(year(flux$datetime)))

# =========================
# 6. Missing percentage before QC
# =========================

qc_vars <- c("co2_flux", "LE", "H", "Ustar", "Tair", "RH", "VPD", "Rg")

missing_before <- tibble(
  Variable = qc_vars,
  Missing_before_percent = map_dbl(
    qc_vars,
    ~ mean(is.na(flux[[.x]])) * 100
  )
)

cat("\n===== Missing percentage before QC =====\n")
print(missing_before)

cat("\n===== Summary before QC =====\n")
print(summary(flux %>% select(all_of(qc_vars))))

# =========================
# 7. Apply QC thresholds
# =========================

flux_qc <- flux %>%
  mutate(
    co2_flux = case_when(
      co2_flux < -50 ~ NA_real_,
      co2_flux > 50 ~ NA_real_,
      TRUE ~ co2_flux
    ),
    
    LE = case_when(
      LE < -500 ~ NA_real_,
      LE > 800 ~ NA_real_,
      TRUE ~ LE
    ),
    
    H = case_when(
      H < -300 ~ NA_real_,
      H > 500 ~ NA_real_,
      TRUE ~ H
    ),
    
    Ustar = case_when(
      Ustar < 0 ~ NA_real_,
      Ustar > 3 ~ NA_real_,
      TRUE ~ Ustar
    ),
    
    Tair = case_when(
      Tair < 10 ~ NA_real_,
      Tair > 45 ~ NA_real_,
      TRUE ~ Tair
    ),
    
    RH = case_when(
      RH < 0 ~ NA_real_,
      RH > 100 ~ NA_real_,
      TRUE ~ RH
    ),
    
    VPD = case_when(
      VPD < 0 ~ NA_real_,
      VPD > 40 ~ NA_real_,
      TRUE ~ VPD
    ),
    
    Rg = case_when(
      Rg < 0 ~ NA_real_,
      Rg > 1200 ~ NA_real_,
      TRUE ~ Rg
    )
  )

# =========================
# 8. Missing percentage after QC
# =========================

missing_after <- tibble(
  Variable = qc_vars,
  Missing_after_percent = map_dbl(
    qc_vars,
    ~ mean(is.na(flux_qc[[.x]])) * 100
  )
)

cat("\n===== Missing percentage after QC =====\n")
print(missing_after)

cat("\n===== Summary after QC =====\n")
print(summary(flux_qc %>% select(all_of(qc_vars))))

# =========================
# 9. QC report
# =========================

qc_report <- missing_before %>%
  left_join(
    missing_after,
    by = "Variable"
  ) %>%
  mutate(
    Removed_by_QC_percent =
      Missing_after_percent - Missing_before_percent
  )

write_csv(
  qc_report,
  qc_report_file,
  na = "NA"
)

# =========================
# 10. Plot raw co2_flux
# =========================

p_raw <- ggplot(flux, aes(x = datetime, y = co2_flux)) +
  geom_line(color = "blue", linewidth = 0.2) +
  theme_minimal() +
  labs(
    title = "2016–2025 Raw CO2 Flux Time Series",
    x = "Date",
    y = "CO2 flux"
  )

print(p_raw)

ggsave(
  fig_raw_file,
  plot = p_raw,
  width = 14,
  height = 6,
  dpi = 300
)

# =========================
# 11. Plot QC-cleaned co2_flux
# =========================

p_qc <- ggplot(flux_qc, aes(x = datetime, y = co2_flux)) +
  geom_line(color = "red", linewidth = 0.2) +
  theme_minimal() +
  labs(
    title = "2016–2025 QC-cleaned CO2 Flux Time Series",
    x = "Date",
    y = "CO2 flux"
  )

print(p_qc)

ggsave(
  fig_qc_file,
  plot = p_qc,
  width = 14,
  height = 6,
  dpi = 300
)

# =========================
# 12. Save QC dataset
# =========================

write_csv(
  flux_qc,
  output_file,
  na = "NA"
)

# =========================
# 13. Finished
# =========================

cat("\nFlux QC completed successfully.\n")
cat("QC dataset saved as:\n")
cat(output_file, "\n")
cat("QC report saved as:\n")
cat(qc_report_file, "\n")
