# =========================================
# 00 Build Annual EddyPro Biomet Dataset
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Merge monthly EddyPro biomet export files into one annual biomet file.
# =========================================

library(dplyr)
library(readr)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

base_path <- "~/Documents"
target_year <- "2016"
site_name <- "muka_head01"

output_file <- file.path(
  base_path,
  paste0("eddypro_", site_name, "_biomet_", target_year, "_FINAL.csv")
)

# =========================================
# 1. Locate monthly folders
# =========================================

month_folders <- list.dirs(
  base_path,
  recursive = FALSE,
  full.names = TRUE
)

year_folders <- month_folders[
  grepl(
    paste0("_", target_year, "$"),
    basename(month_folders)
  )
]

year_folders <- sort(year_folders)

cat("\n===== Monthly folders found =====\n")
print(year_folders)
cat("Folder count:", length(year_folders), "\n")

if (length(year_folders) != 12) {
  stop("Expected 12 monthly folders. Please check folder names such as Jan_2016, Feb_2016, etc.")
}

# =========================================
# 2. Locate monthly biomet files
# =========================================

biomet_files <- unlist(
  lapply(year_folders, function(folder) {
    list.files(
      path = folder,
      pattern = "eddypro_muka_head01_biomet_.*_exp\\.csv$",
      full.names = TRUE
    )
  })
)

biomet_files <- sort(biomet_files)

cat("\n===== Biomet files found =====\n")
print(biomet_files)
cat("Biomet file count:", length(biomet_files), "\n")

if (length(biomet_files) != 12) {
  stop("Expected 12 biomet files. Please check monthly folders or file names.")
}

# =========================================
# 3. Function: read one biomet file
# =========================================

read_biomet <- function(file) {
  
  lines <- readLines(file, warn = FALSE)
  
  header_line <- which(
    grepl("date", tolower(lines)) &
      grepl("time", tolower(lines)) &
      grepl(",", lines)
  )[1]
  
  if (is.na(header_line)) {
    cat("\nCannot identify header line in file:\n")
    cat(file, "\n")
    cat("\nFirst 40 lines:\n")
    print(lines[1:min(40, length(lines))])
    stop("Failed to identify biomet header.")
  }
  
  df <- read_csv(
    file,
    skip = header_line - 1,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  
  names(df) <- trimws(names(df))
  
  date_col <- names(df)[tolower(names(df)) == "date"]
  time_col <- names(df)[tolower(names(df)) == "time"]
  
  if (length(date_col) != 1 | length(time_col) != 1) {
    cat("\nCannot find date/time columns after reading file:\n")
    cat(file, "\n")
    cat("Column names:\n")
    print(names(df))
    stop("Failed to generate TIMESTAMP_START.")
  }
  
  dt <- parse_date_time(
    paste(df[[date_col]], df[[time_col]]),
    orders = c(
      "ymd HMS",
      "ymd HM",
      "dmy HMS",
      "dmy HM",
      "mdy HMS",
      "mdy HM"
    ),
    tz = "Asia/Kuala_Lumpur"
  )
  
  df$TIMESTAMP_START <- format(dt, "%Y%m%d%H%M")
  
  df <- df %>%
    filter(
      !is.na(TIMESTAMP_START),
      TIMESTAMP_START != "",
      grepl("^[0-9]{12}$", TIMESTAMP_START)
    )
  
  df$source_file <- basename(file)
  
  return(df)
}

# =========================================
# 4. Read all monthly biomet files
# =========================================

biomet_list <- lapply(biomet_files, read_biomet)

# =========================================
# 5. Align columns
# =========================================

master_cols <- unique(unlist(lapply(biomet_list, names)))

align_cols <- function(df, master_cols) {
  
  missing_cols <- setdiff(master_cols, names(df))
  
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  df <- df[, master_cols]
  
  return(df)
}

biomet_list_aligned <- lapply(
  biomet_list,
  align_cols,
  master_cols = master_cols
)

# =========================================
# 6. Merge annual biomet data
# =========================================

biomet_year <- bind_rows(biomet_list_aligned)

cat("\n===== Before deduplication =====\n")
cat("Rows:", nrow(biomet_year), "\n")
cat("Columns:", ncol(biomet_year), "\n")
cat("Duplicated TIMESTAMP_START:", sum(duplicated(biomet_year$TIMESTAMP_START)), "\n")

# =========================================
# 7. Deduplicate and sort
# =========================================

biomet_year <- biomet_year %>%
  distinct(TIMESTAMP_START, .keep_all = TRUE) %>%
  arrange(TIMESTAMP_START)

# =========================================
# 8. Basic QC
# =========================================

cat("\n===== After deduplication =====\n")
cat("Rows:", nrow(biomet_year), "\n")
cat("Columns:", ncol(biomet_year), "\n")

cat("\n===== Date range =====\n")
print(range(biomet_year$TIMESTAMP_START, na.rm = TRUE))

cat("\n===== Monthly record count =====\n")
print(table(substr(biomet_year$TIMESTAMP_START, 1, 6)))

cat("\n===== Radiation-related columns =====\n")
print(
  names(biomet_year)[
    grepl(
      "sw|par|ppfd|rg|rad|radiation|short|solar|global",
      names(biomet_year),
      ignore.case = TRUE
    )
  ]
)

cat("\n===== Temperature / humidity-related columns =====\n")
print(
  names(biomet_year)[
    grepl(
      "ta|temp|rh|vpd",
      names(biomet_year),
      ignore.case = TRUE
    )
  ]
)

# =========================================
# 9. Save annual biomet file
# =========================================

write_csv(biomet_year, output_file)

cat("\n=========================================\n")
cat("Annual biomet merge completed successfully\n")
cat("=========================================\n")
cat("Saved file:\n")
cat(output_file, "\n")
