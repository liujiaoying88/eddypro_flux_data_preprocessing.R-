# =========================================
# 14 Data Quality Assessment V3
# Site: MukaHead / CEMACS, Penang, Malaysia
# Author: Cai Xiaoliang
# Purpose:
#   Generate publication-level data quality assessment.
#
# Key outputs:
#   1. Main data quality summary table
#   2. EddyPro CO2 flux QC summary
#   3. Gap-filling summary
#   4. uStar summary
#   5. Publication-level data quality figure
#
# Important distinction:
#   Raw record completeness:
#     Whether the raw half-hourly records exist.
#
#   CO2 flux QC accepted ratio:
#     Percentage of EddyPro CO2 flux records with QC flag 0 or 1.
#
#   NEE gap-filled ratio:
#     Percentage of NEE records filled by REddyProc.
#
#   Final product completeness:
#     Whether final NEE / GPP / Reco products are complete after gap-filling.
# =========================================

library(tidyverse)
library(lubridate)
library(scales)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

full_output_file <- file.path(
  base_path,
  paste0("eddypro_muka_head01_full_output_", analysis_year, "_FINAL.csv")
)

reddyproc_input_file <- file.path(
  base_path,
  paste0("reddyproc_input_", analysis_year, ".csv")
)

reddyproc_filled_file <- file.path(
  base_path,
  paste0("reddyproc_filled_", analysis_year, ".csv")
)

reddyproc_partitioned_file <- file.path(
  base_path,
  paste0("reddyproc_partitioned_", analysis_year, ".csv")
)

ustar_file <- file.path(
  base_path,
  paste0("ustar_threshold_", analysis_year, ".csv")
)

quality_summary_file <- file.path(
  base_path,
  paste0("data_quality_summary_", analysis_year, ".csv")
)

quality_long_file <- file.path(
  base_path,
  paste0("data_quality_summary_long_", analysis_year, ".csv")
)

publication_quality_file <- file.path(
  base_path,
  paste0("publication_quality_indicators_", analysis_year, ".csv")
)

qc_co2_summary_file <- file.path(
  base_path,
  paste0("qc_co2_flux_summary_", analysis_year, ".csv")
)

gapfill_summary_file <- file.path(
  base_path,
  paste0("gapfill_summary_", analysis_year, ".csv")
)

ustar_summary_file <- file.path(
  base_path,
  paste0("ustar_summary_", analysis_year, ".csv")
)

# =========================================
# 1. Read data
# =========================================

full_output <- read_csv(
  full_output_file,
  show_col_types = FALSE
)

reddyproc_input <- read_csv(
  reddyproc_input_file,
  show_col_types = FALSE
)

reddyproc_filled <- read_csv(
  reddyproc_filled_file,
  show_col_types = FALSE
)

reddyproc_partitioned <- read_csv(
  reddyproc_partitioned_file,
  show_col_types = FALSE
)

ustar_threshold <- read_csv(
  ustar_file,
  show_col_types = FALSE
)

# =========================================
# 2. Basic record information
# =========================================

expected_rows <- ifelse(
  leap_year(analysis_year),
  366 * 48,
  365 * 48
)

raw_rows <- nrow(full_output)
input_rows <- nrow(reddyproc_input)
filled_rows <- nrow(reddyproc_filled)
partitioned_rows <- nrow(reddyproc_partitioned)

raw_record_completeness <- raw_rows / expected_rows

# =========================================
# 3. Check required columns
# =========================================

required_input_cols <- c(
  "NEE",
  "Rg",
  "Tair",
  "VPD",
  "Ustar"
)

missing_input_cols <- setdiff(
  required_input_cols,
  names(reddyproc_input)
)

if (length(missing_input_cols) > 0) {
  stop(
    paste(
      "Missing required columns in reddyproc_input:",
      paste(missing_input_cols, collapse = ", ")
    )
  )
}

if (!"qc_co2_flux" %in% names(full_output)) {
  stop("Column qc_co2_flux was not found in EddyPro full_output file.")
}

# =========================================
# 4. Missing-value summary
# =========================================

missing_summary <- reddyproc_input %>%
  summarise(
    NEE_missing_count = sum(is.na(NEE)),
    Rg_missing_count = sum(is.na(Rg)),
    Tair_missing_count = sum(is.na(Tair)),
    VPD_missing_count = sum(is.na(VPD)),
    Ustar_missing_count = sum(is.na(Ustar)),

    NEE_missing_ratio = NEE_missing_count / n(),
    Rg_missing_ratio = Rg_missing_count / n(),
    Tair_missing_ratio = Tair_missing_count / n(),
    VPD_missing_ratio = VPD_missing_count / n(),
    Ustar_missing_ratio = Ustar_missing_count / n()
  )

# =========================================
# 5. Practical outlier summary
# =========================================
# These thresholds are only for reporting.
# They do not modify the data.

outlier_summary <- reddyproc_input %>%
  summarise(
    NEE_outlier_count = sum(
      !is.na(NEE) &
        (
          NEE < -50 |
            NEE > 50
        )
    ),

    Rg_outlier_count = sum(
      !is.na(Rg) &
        (
          Rg < 0 |
            Rg > 1500
        )
    ),

    Tair_outlier_count = sum(
      !is.na(Tair) &
        (
          Tair < 0 |
            Tair > 45
        )
    ),

    VPD_outlier_count = sum(
      !is.na(VPD) &
        (
          VPD < 0 |
            VPD > 50
        )
    ),

    Ustar_outlier_count = sum(
      !is.na(Ustar) &
        (
          Ustar < 0 |
            Ustar > 5
        )
    ),

    NEE_outlier_ratio = NEE_outlier_count / n(),
    Rg_outlier_ratio = Rg_outlier_count / n(),
    Tair_outlier_ratio = Tair_outlier_count / n(),
    VPD_outlier_ratio = VPD_outlier_count / n(),
    Ustar_outlier_ratio = Ustar_outlier_count / n()
  )

# =========================================
# 6. EddyPro CO2 flux QC summary
# =========================================
# Common EddyPro interpretation:
#   0 = best quality
#   1 = acceptable quality
#   2 = poor quality
#   -9999 = missing / invalid

qc_co2_summary <- full_output %>%
  mutate(
    qc_co2_flux = as.numeric(qc_co2_flux),
    QC_category = case_when(
      qc_co2_flux == 0 ~ "Best quality",
      qc_co2_flux == 1 ~ "Acceptable quality",
      qc_co2_flux == 2 ~ "Poor quality",
      qc_co2_flux <= -9990 ~ "Missing or invalid",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(
    qc_co2_flux,
    QC_category
  ) %>%
  summarise(
    Count = n(),
    Ratio = Count / raw_rows,
    .groups = "drop"
  ) %>%
  arrange(
    qc_co2_flux
  )

write_csv(
  qc_co2_summary,
  qc_co2_summary_file
)

co2_qc_accepted_count <- full_output %>%
  mutate(
    qc_co2_flux = as.numeric(qc_co2_flux)
  ) %>%
  summarise(
    accepted = sum(
      qc_co2_flux %in% c(0, 1),
      na.rm = TRUE
    )
  ) %>%
  pull(
    accepted
  )

co2_qc_rejected_count <- full_output %>%
  mutate(
    qc_co2_flux = as.numeric(qc_co2_flux)
  ) %>%
  summarise(
    rejected = sum(
      qc_co2_flux == 2 |
        qc_co2_flux <= -9990,
      na.rm = TRUE
    )
  ) %>%
  pull(
    rejected
  )

co2_qc_accepted_ratio <- co2_qc_accepted_count / raw_rows
co2_qc_rejected_ratio <- co2_qc_rejected_count / raw_rows

# =========================================
# 7. Gap-filling summary
# =========================================

nee_fqc_candidates <- c(
  "NEE_U50_fqc",
  "NEE_uStar_fqc",
  "NEE_U05_fqc",
  "NEE_U95_fqc"
)

nee_fqc_col <- nee_fqc_candidates[
  nee_fqc_candidates %in% names(reddyproc_filled)
][1]

if (is.na(nee_fqc_col)) {
  nee_gapfilled_count <- NA_real_
  nee_gapfilled_ratio <- NA_real_
} else {
  nee_gapfilled_count <- sum(
    reddyproc_filled[[nee_fqc_col]] > 0,
    na.rm = TRUE
  )

  nee_gapfilled_ratio <- nee_gapfilled_count / filled_rows
}

gapfill_summary <- tibble(
  Year = analysis_year,
  Gapfill_QC_column = nee_fqc_col,
  NEE_gapfilled_count = nee_gapfilled_count,
  NEE_gapfilled_ratio = nee_gapfilled_ratio
)

write_csv(
  gapfill_summary,
  gapfill_summary_file
)

# =========================================
# 8. Final product completeness
# =========================================

nee_final_candidates <- c(
  "NEE_U50_f",
  "NEE_uStar_f",
  "NEE_U05_f",
  "NEE_U95_f"
)

gpp_final_candidates <- c(
  "GPP_DT_U50",
  "GPP_DT_uStar",
  "GPP_DT_U05",
  "GPP_DT_U95"
)

reco_final_candidates <- c(
  "Reco_DT_U50",
  "Reco_DT_uStar",
  "Reco_DT_U05",
  "Reco_DT_U95"
)

nee_final_col <- nee_final_candidates[
  nee_final_candidates %in% names(reddyproc_partitioned)
][1]

gpp_final_col <- gpp_final_candidates[
  gpp_final_candidates %in% names(reddyproc_partitioned)
][1]

reco_final_col <- reco_final_candidates[
  reco_final_candidates %in% names(reddyproc_partitioned)
][1]

if (
  is.na(nee_final_col) |
    is.na(gpp_final_col) |
    is.na(reco_final_col)
) {
  stop("Final NEE/GPP/Reco columns were not found in reddyproc_partitioned.")
}

final_complete_count <- reddyproc_partitioned %>%
  summarise(
    complete = sum(
      !is.na(.data[[nee_final_col]]) &
        !is.na(.data[[gpp_final_col]]) &
        !is.na(.data[[reco_final_col]])
    )
  ) %>%
  pull(
    complete
  )

final_product_completeness <- final_complete_count / partitioned_rows

# =========================================
# 9. uStar threshold summary
# =========================================

ustar_summary <- ustar_threshold %>%
  summarise(
    across(
      where(is.numeric),
      list(
        min = ~ min(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  )

write_csv(
  ustar_summary,
  ustar_summary_file
)

# =========================================
# 10. Main publication-level quality summary
# =========================================

data_quality_summary <- tibble(
  Year = analysis_year,

  Expected_half_hourly_records = expected_rows,
  EddyPro_raw_records = raw_rows,
  Raw_record_completeness = raw_record_completeness,

  REddyProc_input_records = input_rows,
  REddyProc_filled_records = filled_rows,
  REddyProc_partitioned_records = partitioned_rows,

  NEE_missing_count = missing_summary$NEE_missing_count,
  NEE_missing_ratio = missing_summary$NEE_missing_ratio,

  Rg_missing_count = missing_summary$Rg_missing_count,
  Rg_missing_ratio = missing_summary$Rg_missing_ratio,

  Tair_missing_count = missing_summary$Tair_missing_count,
  Tair_missing_ratio = missing_summary$Tair_missing_ratio,

  VPD_missing_count = missing_summary$VPD_missing_count,
  VPD_missing_ratio = missing_summary$VPD_missing_ratio,

  Ustar_missing_count = missing_summary$Ustar_missing_count,
  Ustar_missing_ratio = missing_summary$Ustar_missing_ratio,

  NEE_outlier_count = outlier_summary$NEE_outlier_count,
  NEE_outlier_ratio = outlier_summary$NEE_outlier_ratio,

  Rg_outlier_count = outlier_summary$Rg_outlier_count,
  Rg_outlier_ratio = outlier_summary$Rg_outlier_ratio,

  Tair_outlier_count = outlier_summary$Tair_outlier_count,
  Tair_outlier_ratio = outlier_summary$Tair_outlier_ratio,

  VPD_outlier_count = outlier_summary$VPD_outlier_count,
  VPD_outlier_ratio = outlier_summary$VPD_outlier_ratio,

  Ustar_outlier_count = outlier_summary$Ustar_outlier_count,
  Ustar_outlier_ratio = outlier_summary$Ustar_outlier_ratio,

  CO2_flux_QC_accepted_count = co2_qc_accepted_count,
  CO2_flux_QC_accepted_ratio = co2_qc_accepted_ratio,

  CO2_flux_QC_rejected_count = co2_qc_rejected_count,
  CO2_flux_QC_rejected_ratio = co2_qc_rejected_ratio,

  NEE_gapfilled_qc_column = nee_fqc_col,
  NEE_gapfilled_count = nee_gapfilled_count,
  NEE_gapfilled_ratio = nee_gapfilled_ratio,

  Final_NEE_column = nee_final_col,
  Final_GPP_column = gpp_final_col,
  Final_Reco_column = reco_final_col,

  Final_complete_count = final_complete_count,
  Final_product_completeness = final_product_completeness
)

write_csv(
  data_quality_summary,
  quality_summary_file
)

cat("\n===== Publication-level data quality summary =====\n")
print(data_quality_summary)

# =========================================
# 11. Full long-format quality table
# =========================================

quality_long <- tibble(
  Metric = c(
    "Raw record completeness",
    "CO2 flux QC accepted",
    "CO2 flux QC rejected",
    "NEE missing",
    "Rg missing",
    "Tair missing",
    "VPD missing",
    "Ustar missing",
    "NEE gap-filled",
    "Final product completeness"
  ),
  Ratio = c(
    raw_record_completeness,
    co2_qc_accepted_ratio,
    co2_qc_rejected_ratio,
    missing_summary$NEE_missing_ratio,
    missing_summary$Rg_missing_ratio,
    missing_summary$Tair_missing_ratio,
    missing_summary$VPD_missing_ratio,
    missing_summary$Ustar_missing_ratio,
    nee_gapfilled_ratio,
    final_product_completeness
  )
)

write_csv(
  quality_long,
  quality_long_file
)

# =========================================
# 12. Publication indicators only
# =========================================

publication_quality_indicators <- tibble(
  Indicator = c(
    "Raw record completeness",
    "CO2 flux QC accepted",
    "CO2 flux QC rejected",
    "NEE gap-filled",
    "Final product completeness"
  ),
  Ratio = c(
    raw_record_completeness,
    co2_qc_accepted_ratio,
    co2_qc_rejected_ratio,
    nee_gapfilled_ratio,
    final_product_completeness
  )
)

write_csv(
  publication_quality_indicators,
  publication_quality_file
)

# =========================================
# 13. Plot 1: Full data quality summary
# =========================================

p1 <- ggplot(
  quality_long,
  aes(
    x = reorder(Metric, Ratio),
    y = Ratio
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = paste0("Full Data Quality Summary (", analysis_year, ")"),
    x = "",
    y = "Ratio"
  )

print(p1)

ggsave(
  file.path(
    base_path,
    paste0("Full_Data_Quality_Summary_", analysis_year, ".png")
  ),
  p1,
  width = 10,
  height = 7,
  dpi = 600
)

# =========================================
# 14. Plot 2: Publication-level indicators
# =========================================

p2 <- ggplot(
  publication_quality_indicators,
  aes(
    x = reorder(Indicator, Ratio),
    y = Ratio
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = paste0("Publication-Level Data Quality Indicators (", analysis_year, ")"),
    x = "",
    y = "Ratio"
  )

print(p2)

ggsave(
  file.path(
    base_path,
    paste0("Publication_Data_Quality_Indicators_", analysis_year, ".png")
  ),
  p2,
  width = 10,
  height = 5,
  dpi = 600
)

# =========================================
# 15. Finished
# =========================================

saved_files <- c(
  quality_summary_file,
  quality_long_file,
  publication_quality_file,
  qc_co2_summary_file,
  gapfill_summary_file,
  ustar_summary_file,
  file.path(base_path, paste0("Full_Data_Quality_Summary_", analysis_year, ".png")),
  file.path(base_path, paste0("Publication_Data_Quality_Indicators_", analysis_year, ".png"))
)

cat("\n=========================================\n")
cat("14 data quality assessment V3 completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
