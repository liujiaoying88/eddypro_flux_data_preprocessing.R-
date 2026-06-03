# =========================
# EddyPro full_output 年度合并脚本
# AUTO HEADER VERSION
# =========================

library(dplyr)
library(readr)
library(lubridate)

base_path <- "~/Documents"
target_year <- "2016"

# =========================
# 1. 找到年份文件夹
# =========================

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

# =========================
# 2. 找到 full_output 文件
# =========================

full_output_files <- unlist(
  lapply(year_folders, function(folder) {
    list.files(
      path = folder,
      pattern = "eddypro_muka_head01_full_output_.*_exp\\.csv$",
      full.names = TRUE
    )
  })
)

full_output_files <- sort(full_output_files)

cat("找到 full_output 文件数量:", length(full_output_files), "\n")
print(full_output_files)

if (length(full_output_files) != 12) {
  stop("没有找到12个 full_output 文件，请检查月份文件夹或文件名。")
}

# =========================
# 3. 读取单个 full_output 文件函数
# 自动寻找真正表头行
# =========================

read_full_output <- function(file) {
  
  lines <- readLines(file, warn = FALSE)
  
  header_line <- which(
    grepl("date", tolower(lines)) &
      grepl("time", tolower(lines)) &
      grepl(",", lines)
  )[1]
  
  if (is.na(header_line)) {
    cat("\n无法找到真正表头的文件:\n")
    cat(file, "\n")
    cat("\n前40行内容如下:\n")
    print(lines[1:min(40, length(lines))])
    stop("无法识别 full_output 表头。")
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
    cat("\n这个文件读取后找不到 date/time:\n")
    cat(file, "\n")
    cat("列名如下:\n")
    print(names(df))
    stop("无法生成 TIMESTAMP_START。")
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
  
  return(df)
}

# =========================
# 4. 读取12个月 full_output
# =========================

data_list <- lapply(full_output_files, read_full_output)

# =========================
# 5. 统一列
# =========================

master_cols <- unique(unlist(lapply(data_list, names)))

align_cols <- function(df, master_cols) {
  missing_cols <- setdiff(master_cols, names(df))
  
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  df <- df[, master_cols]
  return(df)
}

data_list_aligned <- lapply(
  data_list,
  align_cols,
  master_cols = master_cols
)

# =========================
# 6. 合并
# =========================

year_data <- bind_rows(data_list_aligned)

cat("\n合并前总行数:", nrow(year_data), "\n")

# =========================
# 7. 去重、排序
# =========================

cat("重复 TIMESTAMP_START 数量:", sum(duplicated(year_data$TIMESTAMP_START)), "\n")

year_data <- year_data %>%
  distinct(TIMESTAMP_START, .keep_all = TRUE) %>%
  arrange(TIMESTAMP_START)

# =========================
# 8. 导出
# =========================

output_file <- file.path(
  base_path,
  paste0(
    "eddypro_muka_head01_full_output_",
    target_year,
    "_FINAL.csv"
  )
)

write_csv(year_data, output_file)

# =========================
# 9. 输出检查
# =========================

cat("\n====================\n")
cat(target_year, " full_output 合并完成\n")
cat("====================\n\n")

cat("最终行数:", nrow(year_data), "\n")
cat("最终列数:", ncol(year_data), "\n\n")

cat("文件保存位置:\n")
cat(output_file, "\n\n")

cat("月份数据统计:\n")
print(table(substr(year_data$TIMESTAMP_START, 1, 6)))

cat("\n列名检查:\n")
print(names(year_data))
