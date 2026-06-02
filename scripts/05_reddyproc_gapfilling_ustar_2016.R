# =========================================

# REddyProc Workflow for 2016

# Site: MukaHead

# =========================================



library(tidyverse)

library(lubridate)

library(REddyProc)



# =========================================

# 1. Read Level-1 analysis dataset

# =========================================



flux <- read_csv(

  "/Users/caixiaoliang/Documents/analysis_2016.csv"

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



glimpse(flux_reddyproc)

summary(flux_reddyproc)



# =========================================

# 3. Create REddyProc object

# =========================================



EProc <- sEddyProc$new(

  'MukaHead',

  flux_reddyproc,

  c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar')

)



# =========================================

# 4. Estimate u* threshold

# =========================================



EProc$sEstimateUstarScenarios()



ustar_result <- EProc$sGetEstimatedUstarThresholdDistribution()



write_csv(

  ustar_result,

  "/Users/caixiaoliang/Documents/ustar_threshold_2016.csv"

)



# =========================================

# 5. Gap filling

# =========================================



EProc$sMDSGapFillUStarScens('NEE')

EProc$sMDSGapFill('Tair')

EProc$sMDSGapFill('VPD')

EProc$sMDSGapFill('Rg')



# =========================================

# 6. Export REddyProc results

# =========================================



filled_data <- EProc$sExportResults()



write_csv(

  filled_data,

  "/Users/caixiaoliang/Documents/reddyproc_filled_2016.csv"

)



# =========================================

# 7. Finished

# =========================================



print("REddyProc workflow for 2016 completed successfully.")
