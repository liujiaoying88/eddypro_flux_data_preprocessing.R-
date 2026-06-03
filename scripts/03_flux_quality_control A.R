# =========================================
# 03A Flux Quality Control
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

analysis_year <- 2016

input_file <- paste0(
  "/Users/caixiaoliang/Documents/analysis_",
  analysis_year,
  ".csv"
)

flux_raw <- read_csv(input_file)

flux <- flux_raw %>%
  mutate(datetime = ymd_hm(TIMESTAMP_START)) %>%
  mutate(
    across(
      c(FC, LE, H, USTAR, TA_EP, RH_EP, VPD_EP, SW_IN_POT),
      ~ ifelse(. < -9990, NA, .)
    )
  )

cat("===== Timestamp range =====\n")
print(range(flux$datetime, na.rm = TRUE))

cat("\n===== Duplicate timestamps =====\n")
print(sum(duplicated(flux$datetime)))

cat("\n===== Number of records =====\n")
print(nrow(flux))

missing_before <- tibble(
  Variable = c("FC", "LE", "H", "USTAR", "TA_EP", "RH_EP", "VPD_EP", "SW_IN_POT"),
  Missing_percent = c(
    mean(is.na(flux$FC)) * 100,
    mean(is.na(flux$LE)) * 100,
    mean(is.na(flux$H)) * 100,
    mean(is.na(flux$USTAR)) * 100,
    mean(is.na(flux$TA_EP)) * 100,
    mean(is.na(flux$RH_EP)) * 100,
    mean(is.na(flux$VPD_EP)) * 100,
    mean(is.na(flux$SW_IN_POT)) * 100
  )
)

cat("\n===== Missing percentage before QC =====\n")
print(missing_before)

cat("\n===== Summary before QC =====\n")
print(summary(flux %>% select(FC, LE, H, USTAR, TA_EP, RH_EP, VPD_EP, SW_IN_POT)))

flux_qc <- flux %>%
  mutate(
    FC = case_when(
      FC < -50 ~ NA_real_,
      FC > 50 ~ NA_real_,
      TRUE ~ FC
    ),
    LE = case_when(
      LE < -500 ~ NA_real_,
      LE > 800 ~ NA_real_,
      TRUE ~ LE
    ),
    H = case_when(
      H < -300 ~ NA_real_,
      H > 500 ~ NA_real_,
      TRUE ~ H
    ),
    USTAR = case_when(
      USTAR < 0 ~ NA_real_,
      USTAR > 3 ~ NA_real_,
      TRUE ~ USTAR
    ),
    TA_EP = case_when(
      TA_EP < 10 ~ NA_real_,
      TA_EP > 45 ~ NA_real_,
      TRUE ~ TA_EP
    ),
    RH_EP = case_when(
      RH_EP < 0 ~ NA_real_,
      RH_EP > 100 ~ NA_real_,
      TRUE ~ RH_EP
    ),
    VPD_EP = case_when(
      VPD_EP < 0 ~ NA_real_,
      VPD_EP > 40 ~ NA_real_,
      TRUE ~ VPD_EP
    ),
    SW_IN_POT = case_when(
      SW_IN_POT < 0 ~ NA_real_,
      SW_IN_POT > 1200 ~ NA_real_,
      TRUE ~ SW_IN_POT
    )
  )

missing_after <- tibble(
  Variable = c("FC", "LE", "H", "USTAR", "TA_EP", "RH_EP", "VPD_EP", "SW_IN_POT"),
  Missing_percent = c(
    mean(is.na(flux_qc$FC)) * 100,
    mean(is.na(flux_qc$LE)) * 100,
    mean(is.na(flux_qc$H)) * 100,
    mean(is.na(flux_qc$USTAR)) * 100,
    mean(is.na(flux_qc$TA_EP)) * 100,
    mean(is.na(flux_qc$RH_EP)) * 100,
    mean(is.na(flux_qc$VPD_EP)) * 100,
    mean(is.na(flux_qc$SW_IN_POT)) * 100
  )
)

cat("\n===== Missing percentage after QC =====\n")
print(missing_after)

cat("\n===== Summary after QC =====\n")
print(summary(flux_qc %>% select(FC, LE, H, USTAR, TA_EP, RH_EP, VPD_EP, SW_IN_POT)))

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
  paste0("/Users/caixiaoliang/Documents/qc_report_", analysis_year, ".csv")
)

p_raw <- ggplot(flux, aes(x = datetime, y = FC)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(
    title = paste0(analysis_year, " Raw FC Time Series (-9999 removed)"),
    x = "Date",
    y = "FC"
  )

print(p_raw)

ggsave(
  paste0("/Users/caixiaoliang/Documents/FC_raw_", analysis_year, ".png"),
  plot = p_raw,
  width = 12,
  height = 6,
  dpi = 300
)

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
  paste0("/Users/caixiaoliang/Documents/FC_qc_cleaned_", analysis_year, ".png"),
  plot = p_qc,
  width = 12,
  height = 6,
  dpi = 300
)

write_csv(
  flux_qc,
  paste0("/Users/caixiaoliang/Documents/analysis_", analysis_year, "_qc.csv")
)

cat("\nFlux QC completed successfully.\n")
cat("QC dataset saved as:\n")
cat(paste0("/Users/caixiaoliang/Documents/analysis_", analysis_year, "_qc.csv\n"))
