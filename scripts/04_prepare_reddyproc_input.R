# =========================================
# 04 Prepare REddyProc Input Dataset (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

analysis_year <- 2016

# =========================================
# 1. Read QC-cleaned dataset
# =========================================

flux <- read_csv(
  paste0("/Users/caixiaoliang/Documents/analysis_", analysis_year, "_qc.csv")
)

# =========================================
# 2. Create REddyProc standard input dataset
# =========================================

flux_reddyproc <- flux %>%
  mutate(
    DateTime = ymd_hm(TIMESTAMP_START),
    Year = year(DateTime),
    DoY = yday(DateTime),
    Hour = hour(DateTime) + minute(DateTime) / 60
  ) %>%
  transmute(
    Year = Year,
    DoY = DoY,
    Hour = Hour,
    NEE = FC,
    Rg = SW_IN_POT,
    Tair = TA_EP,
    VPD = VPD_EP,
    Ustar = USTAR
  )

# =========================================
# 3. Check REddyProc input dataset
# =========================================

glimpse(flux_reddyproc)
summary(flux_reddyproc)
head(flux_reddyproc)

# =========================================
# 4. Save REddyProc input file
# =========================================

write_csv(
  flux_reddyproc,
  paste0("/Users/caixiaoliang/Documents/reddyproc_input_", analysis_year, ".csv")
)

print("REddyProc input dataset created successfully.")
