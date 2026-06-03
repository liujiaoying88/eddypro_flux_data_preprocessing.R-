# =========================================
# 03B Create REddyProc-ready Dataset
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 1. Set analysis year
# =========================================

analysis_year <- 2016

# =========================================
# 2. Read QC-cleaned dataset
# =========================================

input_file <- paste0(
  "/Users/caixiaoliang/Documents/analysis_",
  analysis_year,
  "_qc.csv"
)

flux <- read_csv(input_file)

# =========================================
# 3. Create datetime
# =========================================

flux <- flux %>%
  mutate(
    DateTime = ymd_hm(TIMESTAMP_START)
  )

# =========================================
# 4. Generate REddyProc time variables
# =========================================

flux <- flux %>%
  mutate(
    Year = year(DateTime),
    DoY = yday(DateTime),
    Hour =
      hour(DateTime) +
      minute(DateTime) / 60
  )

# =========================================
# 5. Check timestamp continuity
# =========================================

full_time <- tibble(
  DateTime = seq(
    from = min(flux$DateTime, na.rm = TRUE),
    to   = max(flux$DateTime, na.rm = TRUE),
    by   = "30 min"
  )
)

missing_rows <- full_time %>%
  anti_join(flux, by = "DateTime")

cat("\n===== Missing timestamps =====\n")
print(nrow(missing_rows))

if(nrow(missing_rows) > 0){
  print(head(missing_rows))
}

# =========================================
# 6. Check duplicated timestamps
# =========================================

duplicate_count <- sum(duplicated(flux$DateTime))

cat("\n===== Duplicated timestamps =====\n")
print(duplicate_count)

# =========================================
# 7. Create REddyProc-ready dataset
# =========================================

reddyproc_ready <- flux %>%
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
# 8. Summary check
# =========================================

cat("\n===== Dataset structure =====\n")
glimpse(reddyproc_ready)

cat("\n===== Summary =====\n")
print(summary(reddyproc_ready))

# =========================================
# 9. Save dataset
# =========================================

output_file <- paste0(
  "/Users/caixiaoliang/Documents/reddyproc_ready_",
  analysis_year,
  ".csv"
)

write_csv(
  reddyproc_ready,
  output_file
)

# =========================================
# 10. Finished
# =========================================

cat("\nREddyProc-ready dataset created successfully.\n")

cat("\nSaved file:\n")
cat(output_file)
