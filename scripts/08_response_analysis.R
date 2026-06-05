# =========================================
# 08 Flux Partitioning and Environmental Response Analysis
# Site: MukaHead
# Author: Cai Xiaoliang
# Purpose:
#   Read REddyProc partitioned output and analyze
#   environmental responses of NEE, GPP and Reco.
#
# Core rule:
#   08 only reads reddyproc_partitioned_YEAR.csv.
#   It does not create EProc.
#   It does not run gap filling.
#   It does not run flux partitioning.
# =========================================

library(tidyverse)
library(lubridate)

analysis_year <- 2016
base_path <- "~/Documents"

partitioned_file <- file.path(
  base_path,
  paste0("reddyproc_partitioned_", analysis_year, ".csv")
)

output_with_datetime_file <- file.path(
  base_path,
  paste0("partitioned_with_datetime_", analysis_year, ".csv")
)

# =========================================
# 1. Read partitioned data
# =========================================

partitioned <- read_csv(
  partitioned_file,
  show_col_types = FALSE
)

# =========================================
# 2. Select key columns
# =========================================

nee_col <- "NEE_U50_f"
gpp_col <- "GPP_DT_U50"
reco_col <- "Reco_DT_U50"

required_cols <- c(
  nee_col,
  gpp_col,
  reco_col,
  "Rg_f",
  "Tair_f",
  "VPD_f"
)

missing_cols <- setdiff(
  required_cols,
  names(partitioned)
)

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

cat("\n===== Variables selected =====\n")
cat("NEE:", nee_col, "\n")
cat("GPP:", gpp_col, "\n")
cat("Reco:", reco_col, "\n")

# =========================================
# 3. Reconstruct DateTime
# =========================================

expected_rows <- ifelse(
  leap_year(analysis_year),
  366 * 48,
  365 * 48
)

if (nrow(partitioned) != expected_rows) {
  warning(
    paste(
      "Partitioned row count is",
      nrow(partitioned),
      "but expected",
      expected_rows
    )
  )
}

partitioned <- partitioned %>%
  mutate(
    DateTime = as.POSIXct(
      as.Date(paste0(analysis_year, "-01-01")),
      tz = "Asia/Kuala_Lumpur"
    ) +
      minutes(30) * row_number(),

    Year = year(DateTime),
    DoY = yday(DateTime),
    Hour = hour(DateTime) + minute(DateTime) / 60,

    NEE_selected = as.numeric(.data[[nee_col]]),
    GPP_selected = as.numeric(.data[[gpp_col]]),
    Reco_selected = as.numeric(.data[[reco_col]]),
    Tair_C = as.numeric(Tair_f)
  ) %>%
  relocate(
    DateTime,
    Year,
    DoY,
    Hour
  )

# =========================================
# 4. Basic QC
# =========================================

cat("\n===== DateTime range =====\n")
print(range(partitioned$DateTime, na.rm = TRUE))

cat("\n===== Rows =====\n")
cat("Expected rows:", expected_rows, "\n")
cat("Actual rows:", nrow(partitioned), "\n")

cat("\n===== Duplicate DateTime =====\n")
print(sum(duplicated(partitioned$DateTime)))

cat("\n===== Missing values in key variables =====\n")
print(
  colSums(
    is.na(
      partitioned %>%
        select(
          NEE_selected,
          GPP_selected,
          Reco_selected,
          Rg_f,
          Tair_f,
          VPD_f
        )
    )
  )
)

cat("\n===== Key variable summary =====\n")
print(
  summary(
    partitioned %>%
      select(
        NEE_selected,
        GPP_selected,
        Reco_selected,
        Rg_f,
        Tair_C,
        VPD_f
      )
  )
)

# =========================================
# 5. Helper function for response filtering
# =========================================

clean_response_data <- function(data, x_col, y_col) {
  data %>%
    filter(
      !is.na(.data[[x_col]]),
      !is.na(.data[[y_col]]),
      .data[[x_col]] >= quantile(.data[[x_col]], 0.005, na.rm = TRUE),
      .data[[x_col]] <= quantile(.data[[x_col]], 0.995, na.rm = TRUE),
      .data[[y_col]] >= quantile(.data[[y_col]], 0.005, na.rm = TRUE),
      .data[[y_col]] <= quantile(.data[[y_col]], 0.995, na.rm = TRUE)
    )
}

# =========================================
# 6. Radiation response of GPP
# =========================================

gpp_rg_data <- partitioned %>%
  filter(
    !is.na(GPP_selected),
    !is.na(Rg_f),
    Rg_f > 0
  ) %>%
  clean_response_data(
    x_col = "Rg_f",
    y_col = "GPP_selected"
  )

p1 <- ggplot(
  gpp_rg_data,
  aes(
    x = Rg_f,
    y = GPP_selected
  )
) +
  geom_point(
    alpha = 0.05,
    color = "forestgreen"
  ) +
  geom_smooth(
    method = "loess",
    color = "blue"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Radiation Response of GPP (", analysis_year, ")"),
    x = "Radiation Rg",
    y = gpp_col
  )

print(p1)

ggsave(
  file.path(base_path, paste0("Radiation_Response_GPP_", analysis_year, ".png")),
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 7. Daytime VPD response of GPP
# =========================================

gpp_vpd_data <- partitioned %>%
  filter(
    !is.na(GPP_selected),
    !is.na(VPD_f),
    !is.na(Rg_f),
    Rg_f > 300,
    VPD_f > 0
  ) %>%
  clean_response_data(
    x_col = "VPD_f",
    y_col = "GPP_selected"
  )

p2 <- ggplot(
  gpp_vpd_data,
  aes(
    x = VPD_f,
    y = GPP_selected
  )
) +
  geom_point(
    alpha = 0.05,
    color = "darkred"
  ) +
  geom_smooth(
    method = "loess",
    color = "blue"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Daytime VPD Response of GPP (", analysis_year, ")"),
    x = "VPD",
    y = gpp_col
  )

print(p2)

ggsave(
  file.path(base_path, paste0("Daytime_VPD_Response_GPP_", analysis_year, ".png")),
  p2,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 8. Temperature response of Reco
# =========================================

reco_temp_data <- partitioned %>%
  filter(
    !is.na(Reco_selected),
    !is.na(Tair_C)
  ) %>%
  clean_response_data(
    x_col = "Tair_C",
    y_col = "Reco_selected"
  )

p3 <- ggplot(
  reco_temp_data,
  aes(
    x = Tair_C,
    y = Reco_selected
  )
) +
  geom_point(
    alpha = 0.05,
    color = "blue"
  ) +
  geom_smooth(
    method = "loess",
    color = "red"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Temperature Response of Reco (", analysis_year, ")"),
    x = "Air Temperature (°C)",
    y = reco_col
  )

print(p3)

ggsave(
  file.path(base_path, paste0("Temperature_Response_Reco_", analysis_year, ".png")),
  p3,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 9. Radiation response of NEE
# =========================================

nee_rg_data <- partitioned %>%
  filter(
    !is.na(NEE_selected),
    !is.na(Rg_f),
    Rg_f > 0
  ) %>%
  clean_response_data(
    x_col = "Rg_f",
    y_col = "NEE_selected"
  )

p4 <- ggplot(
  nee_rg_data,
  aes(
    x = Rg_f,
    y = NEE_selected
  )
) +
  geom_point(
    alpha = 0.05,
    color = "forestgreen"
  ) +
  geom_smooth(
    method = "loess",
    color = "blue"
  ) +
  theme_minimal() +
  labs(
    title = paste0("Radiation Response of Gap-filled NEE (", analysis_year, ")"),
    x = "Radiation Rg",
    y = nee_col
  )

print(p4)

ggsave(
  file.path(base_path, paste0("Radiation_Response_NEE_", analysis_year, ".png")),
  p4,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 10. Save partitioned data with DateTime
# =========================================

write_csv(
  partitioned,
  output_with_datetime_file
)

cat("\n=========================================\n")
cat("08 response analysis completed successfully\n")
cat("=========================================\n")
cat("Saved file:\n")
cat(output_with_datetime_file, "\n")
