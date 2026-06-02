# =========================================
# Prepare REddyProc Input Dataset (2016)
# Author: Cai Xiaoliang
# =========================================

# =========================================
# 1. Load packages
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 2. Read cleaned dataset
# =========================================

flux <- read_csv(
  "data/analysis_2016.csv"
)

# =========================================
# 3. Convert timestamp to datetime
# =========================================

flux <- flux %>%
  mutate(
    datetime = ymd_hm(TIMESTAMP_START)
  )

# =========================================
# 4. Replace invalid values with NA
# =========================================

flux <- flux %>%
  mutate(
    across(
      c(FC, LE, H, USTAR, TA_EP, RH_EP, VPD_EP, SW_IN_POT),
      ~ ifelse(. < -9990, NA, .)
    )
  )

# =========================================
# 5. Create REddyProc input dataset
# =========================================

flux_reddyproc <- flux %>%
  transmute(
    Year  = year(datetime),
    DoY   = yday(datetime),
    Hour  = hour(datetime) + minute(datetime) / 60,
    NEE   = FC,
    Rg    = SW_IN_POT,
    Tair  = TA_EP,
    VPD   = VPD_EP,
    Ustar = USTAR
  )

# =========================================
# 6. Check REddyProc input dataset
# =========================================

glimpse(flux_reddyproc)

summary(flux_reddyproc)

head(flux_reddyproc)

# =========================================
# 7. Save REddyProc input file
# =========================================

write_csv(
  flux_reddyproc,
  "output/reddyproc_input_2016.csv"
)

# =========================================
# 8. Finished
# =========================================

print("REddyProc input dataset created successfully.")
