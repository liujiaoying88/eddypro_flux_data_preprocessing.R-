# =========================================
# 05 REddyProc Gap-filling and uStar Workflow
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)
library(REddyProc)

analysis_year <- 2016

# =========================================
# 1. Read REddyProc input dataset
# =========================================

flux_reddyproc <- read_csv(
  paste0("/Users/caixiaoliang/Documents/reddyproc_input_", analysis_year, ".csv")
)

# =========================================
# 2. Reconstruct DateTime column
# =========================================

flux_reddyproc <- flux_reddyproc %>%
  mutate(
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
  )

# =========================================
# 3. Check input dataset
# =========================================

glimpse(flux_reddyproc)
summary(flux_reddyproc)

cat("\n===== DateTime range =====\n")
print(range(flux_reddyproc$DateTime, na.rm = TRUE))

cat("\n===== Duplicated DateTime =====\n")
print(sum(duplicated(flux_reddyproc$DateTime)))

# =========================================
# 4. Create REddyProc object
# =========================================

EProc <- sEddyProc$new(
  'MukaHead',
  flux_reddyproc,
  c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'),
  ColPOSIXTime = 'DateTime',
  DTS = 48
)

# =========================================
# 5. Set site location information
# Muka Head, Penang, Malaysia
# =========================================

EProc$sSetLocationInfo(
  LatDeg = 5.47,
  LongDeg = 100.20,
  TimeZoneHour = 8
)

# =========================================
# 6. Estimate u* threshold
# =========================================

EProc$sEstimateUstarScenarios()

ustar_result <- EProc$sGetEstimatedUstarThresholdDistribution()

write_csv(
  ustar_result,
  paste0("/Users/caixiaoliang/Documents/ustar_threshold_", analysis_year, ".csv")
)

# =========================================
# 7. Gap filling
# =========================================

EProc$sMDSGapFillUStarScens('NEE')
EProc$sMDSGapFill('Tair')
EProc$sMDSGapFill('VPD')
EProc$sMDSGapFill('Rg')

# =========================================
# 8. Export REddyProc results
# =========================================

filled_data <- EProc$sExportResults()

write_csv(
  filled_data,
  paste0("/Users/caixiaoliang/Documents/reddyproc_filled_", analysis_year, ".csv")
)

# =========================================
# 9. Check exported variables
# =========================================

cat("\n===== Exported variables =====\n")
print(names(filled_data))

cat("\n===== REddyProc workflow completed successfully =====\n")
cat(
  paste0(
    "Saved file: /Users/caixiaoliang/Documents/reddyproc_filled_",
    analysis_year,
    ".csv\n"
  )
)
