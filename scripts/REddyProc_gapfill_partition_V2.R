# ============================================================
# 05_REddyProc_gapfill_partition_V2.R
# Site: MukaHead
# Purpose:
#   Run REddyProc uStar estimation, MDS gap filling,
#   and flux partitioning for 2016–2025 data.
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)
library(REddyProc)

# =========================
# 1. File paths
# =========================

base_path <- "/Users/caixiaoliang/Documents"

input_file <- file.path(
  base_path,
  "reddyproc_input_mukahead_2016_2025.csv"
)

ustar_file <- file.path(
  base_path,
  "ustar_threshold_2016_2025_by_year.csv"
)

filled_file <- file.path(
  base_path,
  "reddyproc_filled_2016_2025.csv"
)

partitioned_file <- file.path(
  base_path,
  "reddyproc_partitioned_2016_2025_WITH_GPP_RECO.csv"
)

# =========================
# 2. Read REddyProc input
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
  "Year",
  "DoY",
  "Hour",
  "NEE",
  "Rg",
  "Tair",
  "VPD",
  "Ustar"
)

missing_cols <- setdiff(required_cols, names(flux))

if (length(missing_cols) > 0) {
  cat("\nMissing columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

# =========================
# 4. Clean and reconstruct DateTime
# =========================

flux <- flux %>%
  mutate(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),
    NEE = as.numeric(NEE),
    Rg = as.numeric(Rg),
    Tair = as.numeric(Tair),
    VPD = as.numeric(VPD),
    Ustar = as.numeric(Ustar),
    
    DateTime = as.POSIXct(
      as.Date(paste0(Year, "-01-01")) + days(DoY - 1),
      tz = "Asia/Kuala_Lumpur"
    ) + minutes(Hour * 60)
  ) %>%
  select(
    DateTime,
    Year,
    DoY,
    Hour,
    NEE,
    Rg,
    Tair,
    VPD,
    Ustar
  ) %>%
  arrange(DateTime)

# =========================
# 5. Basic QC before REddyProc
# =========================

cat("\n===== Input check =====\n")

cat("\nDateTime range:\n")
print(range(flux$DateTime, na.rm = TRUE))

cat("\nRows:\n")
print(nrow(flux))

cat("\nRows by year:\n")
print(table(flux$Year))

cat("\nDuplicated DateTime:\n")
print(sum(duplicated(flux$DateTime)))

cat("\nMissing values:\n")
print(colSums(is.na(flux)))

cat("\nSummary of key variables:\n")
print(summary(flux %>% select(NEE, Rg, Tair, VPD, Ustar)))

# =========================
# 6. Check equidistant timestamps
# =========================

time_diff <- diff(flux$DateTime)

bad_steps <- which(time_diff != minutes(30))

cat("\n===== Timestamp continuity check =====\n")
cat("Non-30-min intervals:", length(bad_steps), "\n")

if (length(bad_steps) > 0) {
  cat("\nFirst abnormal intervals:\n")
  print(
    tibble(
      row = bad_steps[1:min(20, length(bad_steps))],
      time_before = flux$DateTime[bad_steps[1:min(20, length(bad_steps))]],
      time_after = flux$DateTime[bad_steps[1:min(20, length(bad_steps))] + 1],
      diff = time_diff[bad_steps[1:min(20, length(bad_steps))]]
    )
  )
  
  stop("DateTime is not continuous. Please fix 04 before running 05.")
}

# =========================
# 7. Create REddyProc object
# =========================

EProc <- sEddyProc$new(
  ID = "MukaHead",
  Data = flux,
  ColNames = c("NEE", "Rg", "Tair", "VPD", "Ustar"),
  ColPOSIXTime = "DateTime",
  DTS = 48
)

EProc$sSetLocationInfo(
  LatDeg = 5.47,
  LongDeg = 100.20,
  TimeZoneHour = 8
)

# =========================
# 8. Estimate uStar scenarios
# =========================

cat("\n===== Estimating uStar scenarios =====\n")

ustar_ok <- try(
  EProc$sEstimateUstarScenarios(),
  silent = TRUE
)

if (inherits(ustar_ok, "try-error")) {
  cat("\nuStar scenario estimation failed.\n")
  cat("The workflow will continue with default uStar handling.\n")
} else {
  cat("\nuStar scenario estimation completed.\n")
}

ustar_result <- try(
  EProc$sGetEstimatedUstarThresholdDistribution(),
  silent = TRUE
)

if (!inherits(ustar_result, "try-error")) {
  write_csv(
    as_tibble(ustar_result),
    ustar_file,
    na = "NA"
  )
  
  cat("\nuStar threshold file saved:\n")
  cat(ustar_file, "\n")
} else {
  cat("\nCould not export uStar threshold table.\n")
}

# =========================
# 9. MDS gap filling
# =========================

cat("\n===== Running MDS gap filling =====\n")

gapfill_ok <- try(
  {
    EProc$sMDSGapFillUStarScens("NEE")
  },
  silent = TRUE
)

if (inherits(gapfill_ok, "try-error")) {
  cat("\nMDS gap filling with uStar scenarios failed.\n")
  cat("Trying normal MDS gap filling for NEE.\n")
  
  EProc$sMDSGapFill("NEE")
} else {
  cat("\nNEE gap filling with uStar scenarios completed.\n")
}

EProc$sMDSGapFill("Tair")
EProc$sMDSGapFill("VPD")
EProc$sMDSGapFill("Rg")

filled_data <- EProc$sExportResults()

filled_export <- bind_cols(
  flux,
  as_tibble(filled_data)
)

write_csv(
  filled_export,
  filled_file,
  na = "NA"
)

cat("\nGap-filled file saved:\n")
cat(filled_file, "\n")

cat("\nAvailable filled columns:\n")
print(names(filled_export)[grepl("NEE|Tair|VPD|Rg|Ustar", names(filled_export))])

# =========================
# 10. Flux partitioning
# =========================

cat("\n===== Running flux partitioning =====\n")

partition_method <- "not_run"

partition_dt_ok <- try(
  {
    EProc$sMRFluxPartitionUStarScens()
  },
  silent = TRUE
)

partitioned_data <- EProc$sExportResults()

gpp_cols <- grep("GPP", names(partitioned_data), value = TRUE)
reco_cols <- grep("Reco", names(partitioned_data), value = TRUE)

if (
  inherits(partition_dt_ok, "try-error") ||
  length(gpp_cols) == 0 ||
  length(reco_cols) == 0
) {
  cat("\nMR partitioning failed or did not generate GPP / Reco.\n")
  cat("Trying GL partitioning.\n")
  
  partition_gl_ok <- try(
    {
      EProc$sGLFluxPartitionUStarScens()
    },
    silent = TRUE
  )
  
  partitioned_data <- EProc$sExportResults()
  
  gpp_cols <- grep("GPP", names(partitioned_data), value = TRUE)
  reco_cols <- grep("Reco", names(partitioned_data), value = TRUE)
  
  if (
    inherits(partition_gl_ok, "try-error") ||
    length(gpp_cols) == 0 ||
    length(reco_cols) == 0
  ) {
    partition_method <- "failed"
    cat("\nFlux partitioning failed.\n")
    cat("The workflow will still save the gap-filled NEE result.\n")
  } else {
    partition_method <- "GL"
  }
  
} else {
  partition_method <- "MR"
}

partitioned_data <- EProc$sExportResults()

partitioned_export <- bind_cols(
  flux,
  as_tibble(partitioned_data)
)

write_csv(
  partitioned_export,
  partitioned_file,
  na = "NA"
)

cat("\nPartitioned file saved:\n")
cat(partitioned_file, "\n")

# =========================
# 11. Output QC
# =========================

cat("\n===== Output QC =====\n")

cat("\nPartition method:\n")
print(partition_method)

cat("\nRows in final output:\n")
print(nrow(partitioned_export))

cat("\nRows by year:\n")
print(table(partitioned_export$Year))

cat("\nGPP columns:\n")
print(grep("GPP", names(partitioned_export), value = TRUE))

cat("\nReco columns:\n")
print(grep("Reco", names(partitioned_export), value = TRUE))

cat("\nNEE filled columns:\n")
print(grep("NEE.*_f$|NEE_uStar_f|NEE_U50_f", names(partitioned_export), value = TRUE))

cat("\nCheck NEE_U50_f by year if available:\n")

if ("NEE_U50_f" %in% names(partitioned_export)) {
  print(
    partitioned_export %>%
      group_by(Year) %>%
      summarise(
        rows = n(),
        valid_NEE_U50_f = sum(!is.na(NEE_U50_f)),
        missing_NEE_U50_f = sum(is.na(NEE_U50_f)),
        valid_percent = round(valid_NEE_U50_f / rows * 100, 2),
        .groups = "drop"
      )
  )
} else if ("NEE_uStar_f" %in% names(partitioned_export)) {
  print(
    partitioned_export %>%
      group_by(Year) %>%
      summarise(
        rows = n(),
        valid_NEE_uStar_f = sum(!is.na(NEE_uStar_f)),
        missing_NEE_uStar_f = sum(is.na(NEE_uStar_f)),
        valid_percent = round(valid_NEE_uStar_f / rows * 100, 2),
        .groups = "drop"
      )
  )
} else {
  cat("No standard filled NEE column found.\n")
}

# =========================
# 12. Finished
# =========================

cat("\n=========================================\n")
cat("05 REddyProc V2 completed.\n")
cat("=========================================\n")

cat("\nSaved files:\n")
cat(ustar_file, "\n")
cat(filled_file, "\n")
cat(partitioned_file, "\n")
