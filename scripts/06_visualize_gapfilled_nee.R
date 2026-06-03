# =========================================
# Visualize Gap-filled NEE Time Series (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

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
# 3. Plot gap-filled NEE time series
# =========================================

ggplot(filled, aes(x = DateTime, y = NEE_uStar_f)) +
  geom_line(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Gap-filled NEE Time Series (2016)",
    x = "Date",
    y = "NEE"
  )

# =========================================
# 4. Save figure
# =========================================

ggsave(
  "figures/Gap_filled_NEE_2016.png",
  width = 12,
  height = 6,
  dpi = 300
)

# =========================================
# 5. Finished
# =========================================

print("Gap-filled NEE visualization completed successfully.")
