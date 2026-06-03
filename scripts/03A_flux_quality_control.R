# =========================================
# 03A Flux Quality Control
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Create QC-cleaned flux dataset before REddyProc input preparation
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 1. Set analysis year
# =========================================

analysis_year <- 2016

# =========================================
# 2. Read input dataset
# =========================================

input_file <- paste0(
  "/Users/caixiaoliang/Documents/analysis_",
  analysis_year,
  ".csv"
)

flux <- read_csv(input_file)

# =========================================
# 3. Convert timestamp
# =========================================

flux <- flux %>%
  mutate(
    datetime = ymd_hm(TIMESTAMP_START)
  )

# =========================================
# 4. Basic timestamp checking
# =========================================

cat("===== Timestamp range =====\n")
print(range(flux$datetime, na.rm = TRUE))

cat("\n===== Duplicate timestamps =====\n")
print(sum(duplicated(flux$datetime)))

cat("\n===== Number of records =====\n")
print(nrow(flux))

# =========================================
# 5. Missing value checking before QC
# =========================================

missing_before <- tibble(
  Variable = c("FC", "LE", "H", "USTAR", "TA_EP", "VPD_EP", "SW_IN_POT"),
  Missing_percent = c(
    mean(is.na(flux$FC)) * 100,
    mean(is.na(flux$LE)) * 100,
    mean(is.na(flux$H)) * 100,
    mean(is.na(flux$USTAR)) * 100,
    mean(is.na(flux$TA_EP)) * 100,
    mean(is.na(flux$VPD_EP)) * 100,
    mean(is.na(flux$SW_IN_POT)) * 100
  )
)

cat("\n===== Missing percentage before QC =====\n")
print(missing_before)

# =========================================
# 6. Summary before QC
# =========================================

cat("\n===== Summary before QC =====\n")
print(summary(flux %>% select(FC, LE, H, USTAR, TA_EP, VPD_EP, SW_IN_POT)))

# =========================================
# 7. Physical range filtering
# =========================================

flux_qc <- flux %>%
  mutate(
    FC = ifelse(FC < -9990, NA, FC),
    LE = ifelse(LE < -9990, NA, LE),
    H = ifelse(H < -9990, NA, H),
    USTAR = ifelse(USTAR < -9990, NA, USTAR),
    TA_EP = ifelse(TA_EP < -9990, NA, TA_EP),
    VPD_EP = ifelse(VPD_EP < -9990, NA, VPD_EP),
    SW_IN_POT = ifelse(SW_IN_POT < -9990, NA, SW_IN_POT)
  ) %>%
  mutate(
    FC = case_when(
      FC < -50 ~ NA_real_,
      FC > 50 ~ NA_real_,
      TRUE ~ FC
    ),
    USTAR = case_when(
      USTAR < 0 ~ NA_real_,
      USTAR > 3 ~ NA_real_,
      TRUE ~ USTAR
    ),
    SW_IN_POT = case_when(
      SW_IN_POT < 0 ~ NA_real_,
      SW_IN_POT > 1200 ~ NA_real_,
      TRUE ~ SW_IN_POT
    ),
    VPD_EP = case_when(
      VPD_EP < 0 ~ NA_real_,
      VPD_EP > 40 ~ NA_real_,
      TRUE ~ VPD_EP
    ),
    TA_EP = case_when(
      TA_EP < 10 ~ NA_real_,
      TA_EP > 45 ~ NA_real_,
      TRUE ~ TA_EP
    )
  )

# =========================================
# 8. Missing value checking after QC
# =========================================

missing_after <- tibble(
  Variable = c("FC", "LE", "H", "USTAR", "TA_EP", "VPD_EP", "SW_IN_POT"),
  Missing_percent = c(
    mean(is.na(flux_qc$FC)) * 100,
    mean(is.na(flux_qc$LE)) * 100,
    mean(is.na(flux_qc$H)) * 100,
    mean(is.na(flux_qc$USTAR)) * 100,
    mean(is.na(flux_qc$TA_EP)) * 100,
    mean(is.na(flux_qc$VPD_EP)) * 100,
    mean(is.na(flux_qc$SW_IN_POT)) * 100
  )
)

cat("\n===== Missing percentage after QC =====\n")
print(missing_after)

# =========================================
# 9. Summary after QC
# =========================================

cat("\n===== Summary after QC =====\n")
print(summary(flux_qc %>% select(FC, LE, H, USTAR, TA_EP, VPD_EP, SW_IN_POT)))

# =========================================
# 10. Save QC report
# =========================================

qc_report <- missing_before %>%
  rename(Missing_before_percent = Missing_percent) %>%
  left_join(
    missing_after %>%
      rename(Missing_after_percent = Missing_percent),
    by = "Variable"
  ) %>%
  mutate(
    Removed_by_QC_percent = Missing_after_percent - Missing_before_percent
  )

write_csv(
  qc_report,
  paste0(
    "/Users/caixiaoliang/Documents/qc_report_",
    analysis_year,
    ".csv"
  )
)

# =========================================
# 11. Plot FC before QC
# =========================================

p_raw <- ggplot(flux, aes(x = datetime, y = FC)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(
    title = paste0(analysis_year, " Raw FC Time Series"),
    x = "Date",
    y = "FC"
  )

print(p_raw)

ggsave(
  paste0(
    "/Users/caixiaoliang/Documents/FC_raw_",
    analysis_year,
    ".png"
  ),
  plot = p_raw,
  width = 12,
  height = 6,
  dpi = 300
)

# =========================================
# 12. Plot FC after QC
# =========================================

p_qc <- ggplot(flux_qc, aes(x = datetime, y = FC)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(
    title = paste0(analysis_year, " QC-cleaned FC Time Series"),
    x = "Date",
    y = "FC"
  )

print(p_qc)

ggsave(
  paste0(
    "/Users/caixiaoliang/Documents/FC_qc_cleaned_",
    analysis_year,
    ".png"
  ),
  plot = p_qc,
  width = 12,
  height = 6,
  dpi = 300
)

# =========================================
# 13. Save QC-cleaned dataset
# =========================================

write_csv(
  flux_qc,
  paste0(
    "/Users/caixiaoliang/Documents/analysis_",
    analysis_year,
    "_qc.csv"
  )
)

# =========================================
# 14. Finished
# =========================================

cat("\nFlux QC completed successfully.\n")
cat("QC dataset saved as:\n")
cat(
  paste0(
    "/Users/caixiaoliang/Documents/analysis_",
    analysis_year,
    "_qc.csv\n"
  )
)
