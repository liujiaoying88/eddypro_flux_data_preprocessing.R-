# =========================================
# 05 REddyProc Gap-filling, uStar and Partitioning
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Run uStar estimation, gap filling, and flux partitioning.
#   Export both filled and partitioned REddyProc results.
# =========================================

library(tidyverse)
library(lubridate)
library(REddyProc)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

input_file <- file.path(
  base_path,
  paste0("reddyproc_input_", analysis_year, ".csv")
)

ustar_file <- file.path(
  base_path,
  paste0("ustar_threshold_", analysis_year, ".csv")
)

filled_file <- file.path(
  base_path,
  paste0("reddyproc_filled_", analysis_year, ".csv")
)

partitioned_file <- file.path(
  base_path,
  paste0("reddyproc_partitioned_", analysis_year, ".csv")
)

# =========================================
# 1. Read REddyProc input dataset
# =========================================

cat("\n===== Reading REddyProc input dataset =====\n")
cat("Input file:\n")
cat(input_file, "\n")

flux_reddyproc <- read_csv(
  input_file,
  show_col_types = FALSE
)

# =========================================
# 2. Check required columns
# =========================================

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

missing_cols <- setdiff(
  required_cols,
  names(flux_reddyproc)
)

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

cat("\nRequired columns check passed.\n")

# =========================================
# 3. Reconstruct DateTime column
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

# =========================================
# 4. Input QC
# =========================================

cat("\n===== REddyProc input structure =====\n")
glimpse(flux_reddyproc)

cat("\n===== DateTime range =====\n")
print(
  range(
    flux_reddyproc$DateTime,
    na.rm = TRUE
  )
)

cat("\n===== Expected vs actual rows =====\n")
expected_rows <- ifelse(
  leap_year(analysis_year),
  366 * 48,
  365 * 48
)

cat("Expected rows:", expected_rows, "\n")
cat("Actual rows:", nrow(flux_reddyproc), "\n")

if (nrow(flux_reddyproc) != expected_rows) {
  warning(
    paste(
      "Row count does not match expected half-hourly records for",
      analysis_year
    )
  )
}

cat("\n===== Duplicated DateTime =====\n")
duplicate_count <- sum(
  duplicated(
    flux_reddyproc$DateTime
  )
)

print(duplicate_count)

if (duplicate_count > 0) {
  stop("Duplicated DateTime detected. Stop before REddyProc.")
}

cat("\n===== Missing values summary =====\n")
print(
  colSums(
    is.na(flux_reddyproc)
  )
)

cat("\n===== Variable summaries =====\n")
print(
  summary(
    flux_reddyproc %>%
      select(
        NEE,
        Rg,
        Tair,
        VPD,
        Ustar
      )
  )
)

cat("\n===== Radiation QC =====\n")
cat("Rg NA count:", sum(is.na(flux_reddyproc$Rg)), "\n")
cat("Rg < -20 count:", sum(flux_reddyproc$Rg < -20, na.rm = TRUE), "\n")
cat("Rg max:", max(flux_reddyproc$Rg, na.rm = TRUE), "\n")

# =========================================
# 5. Create REddyProc object
# =========================================

cat("\n===== Creating REddyProc object =====\n")

EProc <- sEddyProc$new(
  'MukaHead',
  flux_reddyproc,
  c(
    'NEE',
    'Rg',
    'Tair',
    'VPD',
    'Ustar'
  ),
  ColPOSIXTime = 'DateTime',
  DTS = 48
)

# =========================================
# 6. Set site location information
# Muka Head, Penang, Malaysia
# =========================================

EProc$sSetLocationInfo(
  LatDeg = 5.47,
  LongDeg = 100.20,
  TimeZoneHour = 8
)

# =========================================
# 7. Estimate u* threshold
# =========================================

cat("\n===== Estimating uStar scenarios =====\n")

EProc$sEstimateUstarScenarios()

ustar_result <- EProc$sGetEstimatedUstarThresholdDistribution()

write_csv(
  ustar_result,
  ustar_file
)

cat("uStar threshold file saved:\n")
cat(ustar_file, "\n")

# =========================================
# 8. Gap filling
# =========================================

cat("\n===== Running MDS gap filling =====\n")

EProc$sMDSGapFillUStarScens('NEE')
EProc$sMDSGapFill('Tair')
EProc$sMDSGapFill('VPD')
EProc$sMDSGapFill('Rg')

filled_data <- EProc$sExportResults()

write_csv(
  filled_data,
  filled_file
)

cat("Filled file saved:\n")
cat(filled_file, "\n")

cat("\n===== Filled data variables =====\n")
print(names(filled_data))

# =========================================
# 9. Flux partitioning
# =========================================

cat("\n===== Running flux partitioning =====\n")

EProc$sMRFluxPartitionUStarScens()

partitioned_data <- EProc$sExportResults()

write_csv(
  partitioned_data,
  partitioned_file
)

cat("Partitioned file saved:\n")
cat(partitioned_file, "\n")

cat("\n===== Partitioned data variables =====\n")
print(names(partitioned_data))

# =========================================
# 10. Output QC
# =========================================

cat("\n===== Output file check =====\n")
cat("Filled rows:", nrow(filled_data), "\n")
cat("Partitioned rows:", nrow(partitioned_data), "\n")

cat("\n===== Key partitioning variables =====\n")
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
cat("05 REddyProc workflow completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
cat(ustar_file, "\n")
cat(filled_file, "\n")
cat(partitioned_file, "\n")
