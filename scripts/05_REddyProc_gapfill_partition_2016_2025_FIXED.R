# ============================================================
# 05_REddyProc_gapfill_partition_2016_2025_FIXED.R
# Site: MukaHead
# Purpose:
#   Run REddyProc gap-filling and flux partitioning by year.
#   If uStar scenario gap filling fails, fallback to normal NEE gap filling.
# ============================================================

library(tidyverse)
library(lubridate)
library(REddyProc)
library(readr)

base_path <- "/Users/caixiaoliang/Documents"

input_file <- file.path(
  base_path,
  "reddyproc_input_mukahead_2016_2025.csv"
)

output_combined_file <- file.path(
  base_path,
  "reddyproc_partitioned_2016_2025_WITH_GPP_RECO.csv"
)

ustar_combined_file <- file.path(
  base_path,
  "ustar_threshold_2016_2025_by_year.csv"
)

flux_all <- read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

required_cols <- c("Year", "DoY", "Hour", "NEE", "Rg", "Tair", "VPD", "Ustar")
missing_cols <- setdiff(required_cols, names(flux_all))

if (length(missing_cols) > 0) {
  print(missing_cols)
  stop("Missing required columns.")
}

flux_all <- flux_all %>%
  mutate(
    Year = as.integer(Year),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour),
    NEE = as.numeric(NEE),
    Rg = as.numeric(Rg),
    Tair = as.numeric(Tair),
    VPD = as.numeric(VPD),
    Ustar = as.numeric(Ustar)
  )

run_one_year <- function(this_year) {
  
  cat("\n====================================\n")
  cat("Running year:", this_year, "\n")
  cat("====================================\n")
  
  dat <- flux_all %>%
    filter(Year == this_year) %>%
    arrange(DoY, Hour) %>%
    mutate(
      DateTime = as.POSIXct(
        as.Date(paste0(Year, "-01-01")) + days(DoY - 1),
        tz = "Asia/Kuala_Lumpur"
      ) + minutes(Hour * 60)
    ) %>%
    select(DateTime, Year, DoY, Hour, NEE, Rg, Tair, VPD, Ustar)
  
  expected_rows <- ifelse(leap_year(this_year), 366 * 48, 365 * 48)
  
  cat("Rows:", nrow(dat), "\n")
  cat("Expected rows:", expected_rows, "\n")
  cat("Duplicated DateTime:", sum(duplicated(dat$DateTime)), "\n")
  print(range(dat$DateTime, na.rm = TRUE))
  
  EProc <- sEddyProc$new(
    ID = paste0("MukaHead_", this_year),
    Data = dat,
    ColNames = c("NEE", "Rg", "Tair", "VPD", "Ustar"),
    ColPOSIXTime = "DateTime",
    DTS = 48
  )
  
  EProc$sSetLocationInfo(
    LatDeg = 5.47,
    LongDeg = 100.20,
    TimeZoneHour = 8
  )
  
  cat("\nEstimating uStar scenarios...\n")
  
  ustar_result <- tryCatch({
    EProc$sEstimateUstarScenarios()
    EProc$sGetEstimatedUstarThresholdDistribution() %>%
      mutate(Year = this_year)
  }, error = function(e) {
    cat("uStar estimation failed for year:", this_year, "\n")
    cat(e$message, "\n")
    tibble(Year = this_year, uStar_status = "failed")
  })
  
  cat("\nRunning NEE gap filling...\n")
  
  gap_method <- "uStar_scenarios"
  
  gap_result <- try(
    EProc$sMDSGapFillUStarScens("NEE"),
    silent = TRUE
  )
  
  if (inherits(gap_result, "try-error")) {
    cat("\nUStar scenario gap filling failed for year:", this_year, "\n")
    cat("Fallback to normal NEE gap filling.\n")
    cat(as.character(gap_result), "\n")
    
    gap_method <- "normal_MDS"
    
    normal_gap_result <- try(
      EProc$sMDSGapFill("NEE"),
      silent = TRUE
    )
    
    if (inherits(normal_gap_result, "try-error")) {
      cat("\nNormal NEE gap filling also failed for year:", this_year, "\n")
      cat(as.character(normal_gap_result), "\n")
    }
  }
  
  cat("\nGap filling meteorological variables...\n")
  
  try(EProc$sMDSGapFill("Tair"), silent = TRUE)
  try(EProc$sMDSGapFill("VPD"), silent = TRUE)
  try(EProc$sMDSGapFill("Rg"), silent = TRUE)
  
  cat("\nTrying night-time partitioning...\n")
  
  nt_result <- try(
    EProc$sMRFluxPartitionUStarScens(),
    silent = TRUE
  )
  
  if (inherits(nt_result, "try-error")) {
    cat("Night-time partitioning with uStar scenarios failed.\n")
    cat("Trying normal night-time partitioning.\n")
    
    nt_result_2 <- try(
      EProc$sMRFluxPartition(),
      silent = TRUE
    )
    
    if (inherits(nt_result_2, "try-error")) {
      cat("Normal night-time partitioning also failed.\n")
      cat(as.character(nt_result_2), "\n")
    }
  } else {
    cat("Night-time partitioning completed.\n")
  }
  
  cat("\nTrying day-time partitioning...\n")
  
  dt_result <- try(
    EProc$sGLFluxPartitionUStarScens(),
    silent = TRUE
  )
  
  if (inherits(dt_result, "try-error")) {
    cat("Day-time partitioning with uStar scenarios failed.\n")
    cat("Trying normal day-time partitioning.\n")
    
    dt_result_2 <- try(
      EProc$sGLFluxPartition(),
      silent = TRUE
    )
    
    if (inherits(dt_result_2, "try-error")) {
      cat("Normal day-time partitioning also failed.\n")
      cat(as.character(dt_result_2), "\n")
    }
  } else {
    cat("Day-time partitioning completed.\n")
  }
  
  result <- EProc$sExportResults() %>%
    as_tibble() %>%
    mutate(
      DateTime = dat$DateTime,
      Year = dat$Year,
      DoY = dat$DoY,
      Hour = dat$Hour,
      gap_method = gap_method
    ) %>%
    relocate(DateTime, Year, DoY, Hour, gap_method)
  
  gpp_cols <- grep("GPP", names(result), value = TRUE)
  reco_cols <- grep("Reco|RECO", names(result), value = TRUE)
  
  cat("\nGPP columns generated:\n")
  print(gpp_cols)
  
  cat("\nReco columns generated:\n")
  print(reco_cols)
  
  year_output_file <- file.path(
    base_path,
    paste0("reddyproc_partitioned_", this_year, "_WITH_GPP_RECO.csv")
  )
  
  write_csv(result, year_output_file, na = "NA")
  
  cat("\nSaved yearly file:\n")
  cat(year_output_file, "\n")
  
  return(
    list(
      data = result,
      ustar = ustar_result
    )
  )
}

years <- 2016:2025

all_results <- map(years, run_one_year)

partitioned_all <- bind_rows(map(all_results, "data"))
ustar_all <- bind_rows(map(all_results, "ustar"))

write_csv(partitioned_all, output_combined_file, na = "NA")
write_csv(ustar_all, ustar_combined_file, na = "NA")

cat("\n====================================\n")
cat("Final combined output check\n")
cat("====================================\n")

cat("\nRows:\n")
print(nrow(partitioned_all))

cat("\nRecords by year:\n")
print(table(partitioned_all$Year))

cat("\nGap methods:\n")
print(table(partitioned_all$gap_method))

cat("\nGPP columns:\n")
print(grep("GPP", names(partitioned_all), value = TRUE))

cat("\nReco columns:\n")
print(grep("Reco|RECO", names(partitioned_all), value = TRUE))

cat("\nSaved combined partitioned file:\n")
cat(output_combined_file, "\n")

cat("\nSaved combined uStar file:\n")
cat(ustar_combined_file, "\n")

cat("\n05 completed.\n")
