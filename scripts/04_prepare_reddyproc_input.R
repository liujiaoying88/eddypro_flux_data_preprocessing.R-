# =========================================
# 04 Prepare REddyProc Input Dataset
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Create complete half-hourly REddyProc input dataset.
# =========================================

library(tidyverse)
library(lubridate)

analysis_year <- 2016
base_path <- "~/Documents"

input_file <- file.path(base_path, paste0("analysis_", analysis_year, ".csv"))
output_file <- file.path(base_path, paste0("reddyproc_input_", analysis_year, ".csv"))

flux <- read_csv(input_file, show_col_types = FALSE)

required_cols <- c("Year", "DoY", "Hour", "FC", "Rg", "TA_EP", "VPD_EP", "USTAR")
missing_cols <- setdiff(required_cols, names(flux))

if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# 原始数据映射为 REddyProc 标准变量
flux_core <- flux %>%
  transmute(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),
    NEE = suppressWarnings(as.numeric(FC)),
    Rg = suppressWarnings(as.numeric(Rg)),
    Tair = suppressWarnings(as.numeric(TA_EP)),
    VPD = suppressWarnings(as.numeric(VPD_EP)),
    Ustar = suppressWarnings(as.numeric(USTAR))
  ) %>%
  filter(Year == analysis_year) %>%
  arrange(DoY, Hour)

# 生成完整半小时骨架
n_days <- ifelse(leap_year(analysis_year), 366, 365)

time_grid <- expand_grid(
  Year = analysis_year,
  DoY = 1:n_days,
  Hour = seq(0, 23.5, by = 0.5)
) %>%
  arrange(DoY, Hour)

# 合并，补齐缺失时间点
flux_reddyproc <- time_grid %>%
  left_join(
    flux_core,
    by = c("Year", "DoY", "Hour")
  ) %>%
  arrange(DoY, Hour)

# QC
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
print(sum(duplicated(flux_reddyproc %>% select(Year, DoY, Hour))))

cat("\nMissing values:\n")
print(colSums(is.na(flux_reddyproc)))

cat("\nSummary:\n")
print(summary(flux_reddyproc))

# 保存
write_csv(flux_reddyproc, output_file)

cat("\n===== 04 completed successfully =====\n")
cat("Saved file:\n")
cat(output_file, "\n")
