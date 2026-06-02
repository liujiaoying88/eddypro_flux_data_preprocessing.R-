# =========================================
# Daily Gap-filled NEE and Environmental Response Analysis (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 1. Read REddyProc gap-filled dataset
# =========================================

filled <- read_csv(
  "output/reddyproc_filled_2016.csv"
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
# 3. Calculate daily mean gap-filled NEE
# =========================================

filled_daily <- filled %>%
  mutate(
    date = as.Date(DateTime)
  ) %>%
  group_by(date) %>%
  summarise(
    NEE_daily = mean(NEE_uStar_f, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================
# 4. Plot daily gap-filled NEE
# =========================================

ggplot(filled_daily, aes(x = date, y = NEE_daily)) +
  geom_line(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Daily Gap-filled NEE (2016)",
    x = "Date",
    y = "Daily NEE"
  )

ggsave(
  "figures/Daily_Gap_filled_NEE_2016.png",
  width = 12,
  height = 6,
  dpi = 300
)

# =========================================
# 5. Plot VPD response of NEE
# =========================================

ggplot(filled, aes(x = VPD_f, y = NEE_uStar_f)) +
  geom_point(alpha = 0.2, color = "darkred") +
  theme_minimal() +
  labs(
    title = "VPD Response of NEE (2016)",
    x = "VPD",
    y = "NEE"
  )

ggsave(
  "figures/NEE_vs_VPD_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 6. Plot temperature response of NEE
# =========================================

ggplot(filled, aes(x = Tair_f, y = NEE_uStar_f)) +
  geom_point(alpha = 0.2, color = "blue") +
  theme_minimal() +
  labs(
    title = "Temperature Response of NEE (2016)",
    x = "Air Temperature (°C)",
    y = "NEE"
  )

ggsave(
  "figures/NEE_vs_Tair_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 7. Save daily dataset
# =========================================

write_csv(
  filled_daily,
  "output/daily_gapfilled_NEE_2016.csv"
)

# =========================================
# 8. Finished
# =========================================

print("Daily NEE and environmental response analysis completed successfully.")
