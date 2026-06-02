# =========================================
# Prepare REddyProc Input Dataset (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 1. Read Level-1 analysis dataset
# =========================================

flux <- read_csv(
  "data/analysis_2016.csv"
)

# =========================================
# 2. Create REddyProc input dataset
# =========================================

flux_reddyproc <- flux %>%
  mutate(
    DateTime = ymd_hm(TIMESTAMP_START) + minutes(30)
  ) %>%
  mutate(
    across(
      c(FC, LE, H, USTAR, TA_EP, RH_EP, VPD_EP, SW_IN_POT),
      ~ ifelse(. < -9990, NA, .)
    )
  ) %>%
  mutate(
    FC = ifelse(FC < -50 | FC > 50, NA, FC)
  ) %>%
  transmute(
    DateTime = DateTime,
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
  "output/reddyproc_input_2016.csv"
)

print("REddyProc input dataset created successfully.")
