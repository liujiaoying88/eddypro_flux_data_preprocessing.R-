# ============================================================
# 05_reddyproc_gapfill_ustar_partition_2016_2025.R
# REddyProc Gap-filling and Flux Partitioning
# Site: Muka Head
# ============================================================

library(tidyverse)
library(lubridate)
library(REddyProc)
library(readr)

base_path <- "/Users/caixiaoliang/Documents"

input_file <- file.path(base_path, "reddyproc_input_mukahead_2016_2025.csv")

ustar_file <- file.path(base_path, "ustar_threshold_2016_2025.csv")
filled_file <- file.path(base_path, "reddyproc_filled_2016_2025.csv")
partitioned_file <- file.path(base_path, "reddyproc_partitioned_2016_2025.csv")

# =========================
# 1. Read input
# =========================

flux_reddyproc <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

required_cols <- c("Year", "DoY", "Hour", "NEE", "Rg", "Tair", "VPD", "Ustar")

missing_cols <- setdiff(required_cols, names(flux_reddyproc))

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# =========================
# 2. Reconstruct DateTime
# =========================
# REddyProc convention:
# Hour = 0.5 means 00:30
# Hour = 23.5 means 23:30

flux_reddyproc <- flux_reddyproc %>%
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
  select(DateTime, Year, DoY, Hour, NEE, Rg, Tair, VPD, Ustar) %>%
  arrange(DateTime)

# =========================
# 3. Input QC
# =========================

cat("\n===== REddyProc input check =====\n")
glimpse(flux_reddyproc)

cat("\nDateTime range:\n")
print(range(flux_reddyproc$DateTime, na.rm = TRUE))

cat("\nRows:\n")
print(nrow(flux_reddyproc))

cat("\nDuplicated DateTime:\n")
print(sum(duplicated(flux_reddyproc$DateTime)))

cat("\nMissing values:\n")
print(colSums(is.na(flux_reddyproc)))

cat("\nRecords by year:\n")
print(table(flux_reddyproc$Year))

cat("\nVariable summary:\n")
print(summary(flux_reddyproc %>% select(NEE, Rg, Tair, VPD, Ustar)))

cat("\nUnit QC:\n")

cat("\nTair range, should be Celsius:\n")
print(range(flux_reddyproc$Tair, na.rm = TRUE))

cat("\nVPD range, should be hPa:\n")
print(range(flux_reddyproc$VPD, na.rm = TRUE))

cat("\nRg negative count:\n")
print(sum(flux_reddyproc$Rg < 0, na.rm = TRUE))

cat("\nExtreme NEE count:\n")
print(sum(flux_reddyproc$NEE < -50 | flux_reddyproc$NEE > 50, na.rm = TRUE))

cat("\nUstar negative count:\n")
print(sum(flux_reddyproc$Ustar < 0, na.rm = TRUE))

# =========================
# 4. Create REddyProc object
# =========================

EProc <- sEddyProc$new(
  "MukaHead",
  flux_reddyproc,
  c("NEE", "Rg", "Tair", "VPD", "Ustar"),
  ColPOSIXTime = "DateTime",
  DTS = 48
)

EProc$sSetLocationInfo(
  LatDeg = 5.47,
  LongDeg = 100.20,
  TimeZoneHour = 8
)

# =========================
# 5. uStar estimation
# =========================

cat("\n===== Estimating uStar scenarios =====\n")

EProc$sEstimateUstarScenarios()

ustar_result <- EProc$sGetEstimatedUstarThresholdDistribution()

write_csv(
  as_tibble(ustar_result),
  ustar_file,
  na = "NA"
)

cat("\nSaved uStar threshold file:\n")
cat(ustar_file, "\n")

# =========================
# 6. MDS gap filling
# =========================

cat("\n===== Running MDS gap filling =====\n")

EProc$sMDSGapFillUStarScens("NEE")

EProc$sMDSGapFill("Tair")
EProc$sMDSGapFill("VPD")
EProc$sMDSGapFill("Rg")

filled_data <- as_tibble(EProc$sExportResults())

filled_output <- bind_cols(
  flux_reddyproc,
  filled_data
)

write_csv(
  filled_output,
  filled_file,
  na = "NA"
)

cat("\nSaved gap-filled file:\n")
cat(filled_file, "\n")

cat("\nFilled variables:\n")
print(names(filled_output))

# =========================
# 7. Flux partitioning
# =========================

cat("\n===== Trying flux partitioning =====\n")

partition_method <- NA_character_

mr_result <- try(
  EProc$sMRFluxPartitionUStarScens(),
  silent = TRUE
)

partitioned_data <- as_tibble(EProc$sExportResults())

partition_cols <- grep(
  "GPP|Reco|R_ref|E_0",
  names(partitioned_data),
  value = TRUE,
  ignore.case = TRUE
)

if (inherits(mr_result, "try-error") || length(partition_cols) == 0) {
  
  cat("\nMR partitioning did not generate GPP / Reco.\n")
  cat("Trying GL partitioning...\n")
  
  gl_result <- try(
    EProc$sGLFluxPartitionUStarScens(),
    silent = TRUE
  )
  
  partitioned_data <- as_tibble(EProc$sExportResults())
  
  partition_cols <- grep(
    "GPP|Reco|R_ref|E_0",
    names(partitioned_data),
    value = TRUE,
    ignore.case = TRUE
  )
  
  if (inherits(gl_result, "try-error") || length(partition_cols) == 0) {
    partition_method <- "failed"
    cat("\nFlux partitioning did not generate GPP / Reco.\n")
    cat("This is not treated as a workflow failure.\n")
  } else {
    partition_method <- "GL"
    cat("\nGL partitioning generated partitioning variables:\n")
    print(partition_cols)
  }
  
} else {
  partition_method <- "MR"
  cat("\nMR partitioning generated partitioning variables:\n")
  print(partition_cols)
}

partitioned_output <- bind_cols(
  flux_reddyproc,
  partitioned_data
)

write_csv(
  partitioned_output,
  partitioned_file,
  na = "NA"
)

cat("\nSaved partitioned file:\n")
cat(partitioned_file, "\n")

# =========================
# 8. Output QC
# =========================

cat("\n===== Output check =====\n")

cat("\nFilled rows:\n")
print(nrow(filled_output))

cat("\nPartitioned rows:\n")
print(nrow(partitioned_output))

cat("\nPartitioning method:\n")
print(partition_method)

cat("\nKey output variables:\n")
print(
  names(partitioned_output)[
    grepl(
      "NEE|GPP|Reco|R_ref|E_0|Ustar|U",
      names(partitioned_output),
      ignore.case = TRUE
    )
  ]
)

cat("\nMissing values in key output columns:\n")

key_cols <- names(partitioned_output)[
  grepl(
    "NEE|GPP|Reco|Rg|Tair|VPD|Ustar",
    names(partitioned_output),
    ignore.case = TRUE
  )
]

print(
  colSums(
    is.na(
      partitioned_output[, key_cols]
    )
  )
)

# =========================
# 9. Finished
# =========================

cat("\n=========================================\n")
cat("05 REddyProc gap-filling workflow completed\n")
cat("=========================================\n")

cat("\nSaved files:\n")
cat(ustar_file, "\n")
cat(filled_file, "\n")
cat(partitioned_file, "\n")

if (partition_method == "failed") {
  cat("\nNote:\n")
  cat("Flux partitioning did not generate GPP / Reco.\n")
  cat("For now, use the gap-filled NEE output first.\n")
}
