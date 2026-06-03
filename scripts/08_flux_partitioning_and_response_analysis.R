EProc <- sEddyProc$new(
  'MukaHead',
  flux_reddyproc,
  c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar')
)

EProc$sSetLocationInfo(
  LatDeg = 5.47,
  LongDeg = 100.20,
  TimeZoneHour = 8
)

EProc$sEstimateUstarScenarios()
EProc$sMDSGapFillUStarScens('NEE')
EProc$sMRFluxPartitionUStarScens()
# =========================================
# Flux Partitioning and Environmental Response Analysis (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)
library(REddyProc)

# =========================================
# 1. Read REddyProc gap-filled dataset
# =========================================

filled <- read_csv(
  "/Users/caixiaoliang/Documents/reddyproc_filled_2016.csv"
)

# =========================================
# 2. Reconstruct datetime sequence
# =========================================

filled <- filled %>%
  mutate(
    DateTime = ymd_hm("201601010030") +
      minutes(30) * (row_number() - 1)
  )

# =========================================
# 3. Smoothed VPD response of NEE
# =========================================

ggplot(filled, aes(x = VPD_f, y = NEE_uStar_f)) +
  geom_point(alpha = 0.05, color = "darkred") +
  geom_smooth(method = "loess", color = "blue") +
  theme_minimal() +
  labs(
    title = "Smoothed VPD Response of NEE",
    x = "VPD",
    y = "NEE"
  )

ggsave(
  "/Users/caixiaoliang/Documents/Smoothed_VPD_Response_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 4. Daytime VPD response analysis
# =========================================

day_flux_clean <- filled %>%
  filter(
    Rg_f > 300,
    VPD_f > 0,
    VPD_f < 20,
    NEE_uStar_f > -20,
    NEE_uStar_f < 20,
    !is.na(NEE_uStar_f),
    !is.na(VPD_f),
    !is.na(Rg_f)
  )

ggplot(day_flux_clean, aes(x = VPD_f, y = NEE_uStar_f)) +
  geom_point(alpha = 0.05, color = "darkred") +
  geom_smooth(method = "loess", color = "blue") +
  theme_minimal() +
  labs(
    title = "Clean Daytime VPD Response of NEE",
    x = "VPD",
    y = "NEE"
  )

ggsave(
  "/Users/caixiaoliang/Documents/Clean_Daytime_VPD_Response_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 5. Light response curve analysis
# =========================================

light_flux <- filled %>%
  filter(
    Rg_f > 0,
    NEE_uStar_f > -20,
    NEE_uStar_f < 20
  )

ggplot(light_flux, aes(x = Rg_f, y = NEE_uStar_f)) +
  geom_point(alpha = 0.03, color = "forestgreen") +
  geom_smooth(method = "loess", color = "blue") +
  theme_minimal() +
  labs(
    title = "Light Response Curve of NEE",
    x = "Radiation (Rg)",
    y = "NEE"
  )

ggsave(
  "/Users/caixiaoliang/Documents/Light_Response_NEE_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 6. Run REddyProc flux partitioning
# =========================================

EProc$sMRFluxPartitionUStarScens()

# =========================================
# 7. Export partitioning results
# =========================================

partitioned_data <- EProc$sExportResults()

write_csv(
  partitioned_data,
  "/Users/caixiaoliang/Documents/reddyproc_partitioned_2016.csv"
)

# =========================================
# 8. Check exported variables
# =========================================

names(partitioned_data)

# =========================================
# 9. Light response of partitioned NEE
# =========================================

nee_light <- partitioned_data %>%
  filter(
    Rg_f > 0,
    NEE_U50_f > -50,
    NEE_U50_f < 50
  )

ggplot(nee_light, aes(x = Rg_f, y = NEE_U50_f)) +
  geom_point(alpha = 0.03, color = "forestgreen") +
  geom_smooth(method = "loess", color = "blue") +
  theme_minimal() +
  labs(
    title = "Light Response Curve of Gap-filled NEE",
    x = "Radiation (Rg)",
    y = "NEE_U50_f"
  )

ggsave(
  "/Users/caixiaoliang/Documents/Light_Response_NEE_U50_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 10. Check partition-related functions
# =========================================

grep("Partition", names(EProc), value = TRUE)

# =========================================
# 11. Finished
# =========================================

print("Flux partitioning and response analysis completed successfully.")
```
