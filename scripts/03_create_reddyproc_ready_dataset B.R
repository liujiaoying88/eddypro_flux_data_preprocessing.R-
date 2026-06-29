# ============================================================
# 03_create_reddyproc_ready_dataset.R
# Create REddyProc-ready Dataset for 2016–2025 Muka Head data
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

input_file <- "/Users/caixiaoliang/Documents/eddypro_muka_head01_fulloutput_biomet_2016_2025_datetime.csv"

output_file <- "/Users/caixiaoliang/Documents/reddyproc_ready_mukahead_2016_2025.csv"

flux <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# 变量映射
nee_col <- "co2_flux"
rg_col <- "RG_1_1_1"
tair_col <- "TA_1_1_1"
vpd_col <- "VPD"
ustar_col <- "u*"

required_cols <- c(
  "datetime", "year", "doy", "decimal_hour",
  nee_col, rg_col, tair_col, vpd_col, ustar_col
)

missing_cols <- setdiff(required_cols, names(flux))

if (length(missing_cols) > 0) {
  print(missing_cols)
  stop("Some required columns are missing.")
}

# 创建 REddyProc-ready 数据
reddyproc_ready <- flux %>%
  transmute(
    Year = as.integer(year),
    DoY = as.numeric(doy),
    Hour = as.numeric(decimal_hour),
    NEE = parse_number(as.character(.data[[nee_col]])),
    Rg = parse_number(as.character(.data[[rg_col]])),
    Tair = parse_number(as.character(.data[[tair_col]])),
    VPD = parse_number(as.character(.data[[vpd_col]])),
    Ustar = parse_number(as.character(.data[[ustar_col]])),
    datetime = ymd_hms(datetime, tz = "Asia/Kuala_Lumpur")
  )

# Tair: Kelvin -> Celsius
if (median(reddyproc_ready$Tair, na.rm = TRUE) > 100) {
  reddyproc_ready <- reddyproc_ready %>%
    mutate(Tair = Tair - 273.15)
  cat("Tair converted from Kelvin to Celsius.\n")
}

# VPD: Pa -> hPa
if (median(reddyproc_ready$VPD, na.rm = TRUE) > 100) {
  reddyproc_ready <- reddyproc_ready %>%
    mutate(VPD = VPD / 100)
  cat("VPD converted from Pa to hPa.\n")
}

# 删除时间变量缺失行
reddyproc_ready <- reddyproc_ready %>%
  filter(
    !is.na(Year),
    !is.na(DoY),
    !is.na(Hour)
  ) %>%
  arrange(datetime)

# 检查缺失时间
full_time <- tibble(
  datetime = seq(
    from = min(reddyproc_ready$datetime, na.rm = TRUE),
    to = max(reddyproc_ready$datetime, na.rm = TRUE),
    by = "30 min"
  )
)

missing_rows <- full_time %>%
  anti_join(reddyproc_ready, by = "datetime")

cat("\n===== Missing timestamps =====\n")
cat("Missing timestamp count:", nrow(missing_rows), "\n")

cat("\n===== Duplicated timestamps =====\n")
cat("Duplicated timestamp count:", sum(duplicated(reddyproc_ready$datetime)), "\n")

cat("\n===== Records by year =====\n")
print(table(reddyproc_ready$Year))

# 输出 REddyProc 输入文件
reddyproc_ready_export <- reddyproc_ready %>%
  select(
    Year,
    DoY,
    Hour,
    NEE,
    Rg,
    Tair,
    VPD,
    Ustar
  )

cat("\n===== Summary =====\n")
print(summary(reddyproc_ready_export))

write_csv(
  reddyproc_ready_export,
  output_file,
  na = "NA"
)

cat("\nREddyProc-ready dataset created successfully.\n")
cat("Saved file:\n")
cat(output_file, "\n")
