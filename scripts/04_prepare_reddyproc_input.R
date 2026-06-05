# =========================================
# 04 Prepare REddyProc Input Dataset
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Convert cleaned analysis_YEAR.csv into REddyProc-ready input.
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

input_file <- file.path(
  base_path,
  paste0("analysis_", analysis_year, ".csv")
)

output_file <- file.path(
  base_path,
  paste0("reddyproc_input_", analysis_year, ".csv")
)

# =========================================
# 1. Read cleaned analysis dataset
# =========================================

cat("\n===== Reading analysis dataset =====\n")
cat("Input file:\n")
cat(input_file, "\n")

flux <- read_csv(
  input_file,
  show_col_types = FALSE
)

# =========================================
# 2. Check required columns
# =========================================

required_cols <- c(
  "DateTime",
  "Year",
  "DoY",
  "Hour",
  "FC",
  "Rg",
  "TA_EP",
  "VPD_EP",
  "USTAR"
)

missing_cols <- setdiff(
  required_cols,
  names(flux)
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
# 3. Create REddyProc standard input dataset
# =========================================

flux_reddyproc <- flux %>%
  mutate(
    DateTime = ymd_hms(
      DateTime,
      tz = "Asia/Kuala_Lumpur"
    ),
    
    Year = as.integer(Year),
    
    DoY = as.integer(DoY),
    
    Hour = as.numeric(Hour),
    
    NEE = suppressWarnings(
      as.numeric(FC)
    ),
    
    Rg = suppressWarnings(
      as.numeric(Rg)
    ),
    
    Tair = suppressWarnings(
      as.numeric(TA_EP)
    ),
    
    VPD = suppressWarnings(
      as.numeric(VPD_EP)
    ),
    
    Ustar = suppressWarnings(
      as.numeric(USTAR)
    )
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
# 4. Basic QC
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

cat("\n===== Duplicate DateTime =====\n")
duplicate_count <- sum(
  duplicated(
    flux_reddyproc$DateTime
  )
)

print(duplicate_count)

if (duplicate_count > 0) {
  warning("Duplicated DateTime detected.")
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

cat("\n===== Rg QC =====\n")
cat("Rg NA count:", sum(is.na(flux_reddyproc$Rg)), "\n")
cat("Rg < -20 count:", sum(flux_reddyproc$Rg < -20, na.rm = TRUE), "\n")
cat("Rg max:", max(flux_reddyproc$Rg, na.rm = TRUE), "\n")

cat("\n===== NEE QC =====\n")
cat("NEE NA count:", sum(is.na(flux_reddyproc$NEE)), "\n")
cat("NEE min:", min(flux_reddyproc$NEE, na.rm = TRUE), "\n")
cat("NEE max:", max(flux_reddyproc$NEE, na.rm = TRUE), "\n")

# =========================================
# 5. Save REddyProc input file
# =========================================

write_csv(
  flux_reddyproc,
  output_file
)

cat("\n=========================================\n")
cat("04 REddyProc input dataset created successfully\n")
cat("=========================================\n")
cat("Saved file:\n")
cat(output_file, "\n")
