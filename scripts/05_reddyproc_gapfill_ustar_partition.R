# =========================================
# 05 REddyProc Gap-filling and Optional Partitioning
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Run uStar estimation and MDS gap filling.
#   Try flux partitioning, but do not stop the workflow
#   if GPP / Reco are not generated.
# =========================================

library(tidyverse)
library(lubridate)
library(REddyProc)

analysis_year <- 2016
base_path <- "~/Documents"

input_file <- file.path(base_path, paste0("reddyproc_input_", analysis_year, ".csv"))
ustar_file <- file.path(base_path, paste0("ustar_threshold_", analysis_year, ".csv"))
filled_file <- file.path(base_path, paste0("reddyproc_filled_", analysis_year, ".csv"))
partitioned_file <- file.path(base_path, paste0("reddyproc_partitioned_", analysis_year, ".csv"))

# =========================================
# 1. Read input
# =========================================

flux_reddyproc <- read_csv(input_file, show_col_types = FALSE)

required_cols <- c("Year", "DoY", "Hour", "NEE", "Rg", "Tair", "VPD", "Ustar")
missing_cols <- setdiff(required_cols, names(flux_reddyproc))

if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# =========================================
# 2. Reconstruct DateTime
# =========================================

flux_reddyproc <- flux_reddyproc %>%
  mutate(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),
    NEE = suppressWarnings(as.numeric(NEE)),
    Rg = suppressWarnings(as.numeric(Rg)),
    Tair = suppressWarnings(as.numeric(Tair)),
    VPD = suppressWarnings(as.numeric(VPD)),
    Ustar = suppressWarnings(as.numeric(Ustar)),
    DateTime = as.POSIXct(
      as.Date(paste0(Year, "-01-01")) + days(DoY - 1),
      tz = "Asia/Kuala_Lumpur"
    ) + minutes(Hour * 60)
  ) %>%
  select(DateTime, Year, DoY, Hour, NEE, Rg, Tair, VPD, Ustar) %>%
  arrange(DateTime)

# =========================================
# 3. Input QC
# =========================================

cat("\n===== REddyProc input check =====\n")
glimpse(flux_reddyproc)

cat("\nDateTime range:\n")
print(range(flux_reddyproc$DateTime, na.rm = TRUE))

cat("\nDuplicated DateTime:\n")
print(sum(duplicated(flux_reddyproc$DateTime)))

cat("\nMissing values:\n")
print(colSums(is.na(flux_reddyproc)))

cat("\nVariable summary:\n")
print(summary(flux_reddyproc %>% select(NEE, Rg, Tair, VPD, Ustar)))

cat("\nUnit QC:\n")
cat("Tair range:\n")
print(range(flux_reddyproc$Tair, na.rm = TRUE))
cat("VPD range:\n")
print(range(flux_reddyproc$VPD, na.rm = TRUE))
cat("Rg negative count:\n")
print(sum(flux_reddyproc$Rg < 0, na.rm = TRUE))
cat("Extreme NEE count:\n")
print(sum(flux_reddyproc$NEE < -50 | flux_reddyproc$NEE > 50, na.rm = TRUE))

# =========================================
# 4. Create REddyProc object
# =========================================

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

# =========================================
# 5. uStar estimation
# =========================================

cat("\n===== Estimating uStar scenarios =====\n")

EProc$sEstimateUstarScenarios()

ustar_result <- EProc$sGetEstimatedUstarThresholdDistribution()

write_csv(ustar_result, ustar_file)

cat("Saved uStar file:\n")
cat(ustar_file, "\n")

# =========================================
# 6. Gap filling
# =========================================

cat("\n===== Running MDS gap filling =====\n")

EProc$sMDSGapFillUStarScens("NEE")
EProc$sMDSGapFill("Tair")
EProc$sMDSGapFill("VPD")
EProc$sMDSGapFill("Rg")

filled_data <- EProc$sExportResults()

write_csv(filled_data, filled_file)

cat("Saved filled file:\n")
cat(filled_file, "\n")

cat("\nFilled variables:\n")
print(names(filled_data))

# =========================================
# 7. Optional Flux Partitioning
# =========================================

cat("\n===== Trying flux partitioning =====\n")

partition_method <- NA_character_

mr_result <- try(
  EProc$sMRFluxPartitionUStarScens(),
  silent = TRUE
)

partitioned_data <- EProc$sExportResults()

partition_cols <- grep(
  "GPP|Reco",
  names(partitioned_data),
  value = TRUE,
  ignore.case = TRUE
)

if (
  inherits(mr_result, "try-error") ||
  length(partition_cols) == 0
) {
  cat("\nMR partitioning did not generate GPP / Reco.\n")
  cat("Trying GL partitioning...\n")
  
  gl_result <- try(
    EProc$sGLFluxPartitionUStarScens(),
    silent = TRUE
  )
  
  partitioned_data <- EProc$sExportResults()
  
  partition_cols <- grep(
    "GPP|Reco",
    names(partitioned_data),
    value = TRUE,
    ignore.case = TRUE
  )
  
  if (
    inherits(gl_result, "try-error") ||
    length(partition_cols) == 0
  ) {
    partition_method <- "failed"
    cat("\nFlux partitioning did not generate GPP / Reco.\n")
    cat("This is not treated as a workflow failure.\n")
    cat("Continue downstream analysis with reddyproc_filled_", analysis_year, ".csv\n", sep = "")
  } else {
    partition_method <- "GL"
    cat("\nGL partitioning generated GPP / Reco:\n")
    print(partition_cols)
  }
  
} else {
  partition_method <- "MR"
  cat("\nMR partitioning generated GPP / Reco:\n")
  print(partition_cols)
}

write_csv(partitioned_data, partitioned_file)

cat("\nSaved partitioning attempt file:\n")
cat(partitioned_file, "\n")

# =========================================
# 8. Output QC
# =========================================

cat("\n===== Output check =====\n")
cat("Filled rows:", nrow(filled_data), "\n")
cat("Partitioned attempt rows:", nrow(partitioned_data), "\n")
cat("Partitioning method:", partition_method, "\n")

cat("\nKey variables in partitioned attempt:\n")
print(
  names(partitioned_data)[
    grepl(
      "NEE|GPP|Reco|R_ref|E_0|U",
      names(partitioned_data),
      ignore.case = TRUE
    )
  ]
)

cat("\n=========================================\n")
cat("05 REddyProc gap-filling workflow completed\n")
cat("=========================================\n")
cat("Saved files:\n")
cat(ustar_file, "\n")
cat(filled_file, "\n")
cat(partitioned_file, "\n")

if (partition_method == "failed") {
  cat("\nNote:\n")
  cat("Flux partitioning did not generate GPP / Reco.\n")
  cat("For the current workflow, use reddyproc_filled_", analysis_year, ".csv for NEE response and monthly NEE analysis.\n", sep = "")
}
