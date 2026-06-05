# =========================================
# 04 Prepare REddyProc Input Dataset
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Create complete half-hourly REddyProc input dataset.
#   REddyProc convention:
#   - Half-hourly timestamp uses end time.
#   - First record should be Hour = 0.5.
#   - Last record should be Hour = 24.
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
# 1. Read analysis dataset
# =========================================

flux <- read_csv(
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

# =========================================
# 3. Map variables to REddyProc format
# =========================================

flux_core <- flux %>%
  transmute(
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
  filter(
    Year == analysis_year
  ) %>%
  mutate(
    # EddyPro air_temperature may be Kelvin.
    # REddyProc requires degree Celsius.
    Tair = ifelse(
      Tair > 100,
      Tair - 273.15,
      Tair
    ),

    # EddyPro VPD is often Pa.
    # REddyProc expects hPa.
    VPD = ifelse(
      VPD > 100,
      VPD / 100,
      VPD
    ),

    # Remove physically unreasonable VPD values.
    # Values above 60 hPa are treated as invalid for this site-level workflow.
    VPD = ifelse(
      VPD > 60,
      NA,
      VPD
    ),

    # Very small negative nighttime radiation values
    # are physically treated as zero for REddyProc.
    Rg = ifelse(
      Rg < 0,
      0,
      Rg
    ),

    # Basic missing-value safeguard
    across(
      c(
        NEE,
        Rg,
        Tair,
        VPD,
        Ustar
      ),
      ~ ifelse(
        . <= -9990,
        NA,
        .
      )
    )
  ) %>%
  arrange(
    DoY,
    Hour
  )

# =========================================
# 4. Create complete half-hourly time grid
# =========================================

n_days <- ifelse(
  leap_year(analysis_year),
  366,
  365
)

time_grid <- expand_grid(
  Year = analysis_year,
  DoY = 1:n_days,
  Hour = seq(
    0.5,
    24,
    by = 0.5
  )
) %>%
  arrange(
    DoY,
    Hour
  )

# =========================================
# 5. Merge data with complete time grid
# =========================================

flux_reddyproc <- time_grid %>%
  left_join(
    flux_core,
    by = c(
      "Year",
      "DoY",
      "Hour"
    )
  ) %>%
  arrange(
    DoY,
    Hour
  )

# =========================================
# 6. QC
# =========================================

cat("\n===== REddyProc input check =====\n")
glimpse(flux_reddyproc)

cat("\nRows:\n")
print(nrow(flux_reddyproc))

cat("\nExpected rows:\n")
print(n_days * 48)

cat("\nYear table:\n")
print(table(flux_reddyproc$Year))

cat("\nDoY range:\n")
print(range(flux_reddyproc$DoY, na.rm = TRUE))

cat("\nHour range:\n")
print(range(flux_reddyproc$Hour, na.rm = TRUE))

cat("\nDuplicated Year-DoY-Hour:\n")
print(
  sum(
    duplicated(
      flux_reddyproc %>%
        select(
          Year,
          DoY,
          Hour
        )
    )
  )
)

cat("\nMissing values:\n")
print(
  colSums(
    is.na(flux_reddyproc)
  )
)

cat("\nVariable summary:\n")
print(
  summary(
    flux_reddyproc
  )
)

cat("\nUnit QC:\n")

cat("Tair range should be Celsius:\n")
print(
  range(
    flux_reddyproc$Tair,
    na.rm = TRUE
  )
)

cat("VPD range should be hPa and <= 60:\n")
print(
  range(
    flux_reddyproc$VPD,
    na.rm = TRUE
  )
)

cat("VPD > 60 count should be 0:\n")
print(
  sum(
    flux_reddyproc$VPD > 60,
    na.rm = TRUE
  )
)

cat("Rg negative count should be 0:\n")
print(
  sum(
    flux_reddyproc$Rg < 0,
    na.rm = TRUE
  )
)

# =========================================
# 7. Save
# =========================================

write_csv(
  flux_reddyproc,
  output_file
)

cat("\n===== 04 completed successfully =====\n")
cat("Saved file:\n")
cat(output_file, "\n")
