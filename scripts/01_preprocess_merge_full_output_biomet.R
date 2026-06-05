# =========================================
# 01 Merge Full Output + Biomet
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Merge annual EddyPro full_output and biomet datasets,
#   clean missing values and generate analysis_YEAR.csv
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

full_output_file <- file.path(
  base_path,
  paste0(
    "eddypro_muka_head01_full_output_",
    analysis_year,
    "_FINAL.csv"
  )
)

biomet_file <- file.path(
  base_path,
  paste0(
    "eddypro_muka_head01_biomet_",
    analysis_year,
    "_FINAL.csv"
  )
)

output_file <- file.path(
  base_path,
  paste0(
    "analysis_",
    analysis_year,
    ".csv"
  )
)

# =========================================
# 1. Read datasets
# =========================================

cat("\n===== Reading input files =====\n")
cat("Full output file:\n", full_output_file, "\n")
cat("Biomet file:\n", biomet_file, "\n")

full_output <- read_csv(
  full_output_file,
  show_col_types = FALSE
)

biomet <- read_csv(
  biomet_file,
  show_col_types = FALSE
)

# =========================================
# 2. Check required columns
# =========================================

required_full_cols <- c(
  "TIMESTAMP_START",
  "co2_flux",
  "LE",
  "H",
  "u*",
  "air_temperature",
  "RH",
  "VPD"
)

required_biomet_cols <- c(
  "TIMESTAMP_START",
  "RG_1_1_1"
)

missing_full_cols <- setdiff(
  required_full_cols,
  names(full_output)
)

missing_biomet_cols <- setdiff(
  required_biomet_cols,
  names(biomet)
)

if (length(missing_full_cols) > 0) {
  stop(
    paste(
      "Missing required full_output columns:",
      paste(missing_full_cols, collapse = ", ")
    )
  )
}

if (length(missing_biomet_cols) > 0) {
  stop(
    paste(
      "Missing required biomet columns:",
      paste(missing_biomet_cols, collapse = ", ")
    )
  )
}

cat("\nRequired columns check passed.\n")

# =========================================
# 3. Standardize TIMESTAMP_START
# =========================================

full_output <- full_output %>%
  mutate(
    TIMESTAMP_START = as.character(TIMESTAMP_START)
  )

biomet <- biomet %>%
  mutate(
    TIMESTAMP_START = as.character(TIMESTAMP_START)
  )

# =========================================
# 4. Prepare biomet radiation variable
# =========================================

biomet_clean <- biomet %>%
  select(
    TIMESTAMP_START,
    RG_1_1_1
  ) %>%
  mutate(
    RG_1_1_1 = suppressWarnings(
      as.numeric(RG_1_1_1)
    ),
    RG_1_1_1 = ifelse(
      RG_1_1_1 <= -9990,
      NA,
      RG_1_1_1
    ),
    # Radiation QC:
    # Small negative nighttime radiation can occur,
    # but very negative values are physically unreasonable.
    RG_1_1_1 = ifelse(
      RG_1_1_1 < -20,
      NA,
      RG_1_1_1
    )
  )

# =========================================
# 5. Merge full_output and biomet
# =========================================

merged <- full_output %>%
  left_join(
    biomet_clean,
    by = "TIMESTAMP_START"
  )

cat("\n===== Merge check =====\n")
cat("Rows in full_output:", nrow(full_output), "\n")
cat("Rows in biomet:", nrow(biomet_clean), "\n")
cat("Rows after merge:", nrow(merged), "\n")
cat("Missing RG_1_1_1 after merge:", sum(is.na(merged$RG_1_1_1)), "\n")

# =========================================
# 6. Build analysis dataset
# =========================================

analysis <- merged %>%
  mutate(
    DateTime = ymd_hm(
      TIMESTAMP_START,
      tz = "Asia/Kuala_Lumpur"
    ),
    
    Year = year(DateTime),
    
    DoY = yday(DateTime),
    
    Hour = hour(DateTime) +
      minute(DateTime) / 60,
    
    FC = suppressWarnings(
      as.numeric(co2_flux)
    ),
    
    LE = suppressWarnings(
      as.numeric(LE)
    ),
    
    H = suppressWarnings(
      as.numeric(H)
    ),
    
    USTAR = suppressWarnings(
      as.numeric(`u*`)
    ),
    
    TA_EP = suppressWarnings(
      as.numeric(air_temperature)
    ),
    
    RH_EP = suppressWarnings(
      as.numeric(RH)
    ),
    
    VPD_EP = suppressWarnings(
      as.numeric(VPD)
    ),
    
    Rg = suppressWarnings(
      as.numeric(RG_1_1_1)
    )
  ) %>%
  mutate(
    across(
      c(
        FC,
        LE,
        H,
        USTAR,
        TA_EP,
        RH_EP,
        VPD_EP,
        Rg
      ),
      ~ ifelse(
        . <= -9990,
        NA,
        .
      )
    ),
    
    # Additional radiation QC after final renaming
    Rg = ifelse(
      Rg < -20,
      NA,
      Rg
    )
  ) %>%
  select(
    DateTime,
    Year,
    DoY,
    Hour,
    FC,
    LE,
    H,
    USTAR,
    TA_EP,
    RH_EP,
    VPD_EP,
    Rg
  ) %>%
  arrange(DateTime)

# =========================================
# 7. Basic QC
# =========================================

cat("\n===== Analysis dataset structure =====\n")
glimpse(analysis)

cat("\n===== DateTime range =====\n")
print(
  range(
    analysis$DateTime,
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
cat("Actual rows:", nrow(analysis), "\n")

if (nrow(analysis) != expected_rows) {
  warning(
    paste(
      "Row count is not equal to expected half-hourly records for",
      analysis_year
    )
  )
}

cat("\n===== Duplicate DateTime =====\n")
duplicate_count <- sum(
  duplicated(
    analysis$DateTime
  )
)

print(duplicate_count)

if (duplicate_count > 0) {
  warning("Duplicated DateTime detected.")
}

cat("\n===== Missing values summary =====\n")
missing_summary <- colSums(
  is.na(analysis)
)

print(missing_summary)

cat("\n===== Variable summaries =====\n")
print(
  summary(
    analysis %>%
      select(
        FC,
        LE,
        H,
        USTAR,
        TA_EP,
        RH_EP,
        VPD_EP,
        Rg
      )
  )
)

cat("\n===== Rg QC =====\n")
cat("Rg NA count:", sum(is.na(analysis$Rg)), "\n")
cat("Rg < -20 count:", sum(analysis$Rg < -20, na.rm = TRUE), "\n")
cat("Rg max:", max(analysis$Rg, na.rm = TRUE), "\n")

cat("\n===== FC QC =====\n")
cat("FC NA count:", sum(is.na(analysis$FC)), "\n")
cat("FC min:", min(analysis$FC, na.rm = TRUE), "\n")
cat("FC max:", max(analysis$FC, na.rm = TRUE), "\n")

# =========================================
# 8. Save analysis dataset
# =========================================

write_csv(
  analysis,
  output_file
)

cat("\n=========================================\n")
cat("01 preprocessing completed successfully\n")
cat("=========================================\n")
cat("Saved file:\n")
cat(output_file, "\n")
