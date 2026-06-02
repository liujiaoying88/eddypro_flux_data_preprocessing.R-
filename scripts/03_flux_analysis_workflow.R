# =========================================
# Flux Analysis Workflow (2016)
# Author: Cai Xiaoliang
# =========================================

# =========================================
# 1. Load packages
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 2. Read CSV file
# =========================================

flux <- read_csv(
"/Users/caixiaoliang/Documents/analysis_2016.csv"
)

# =========================================
# 3. Convert timestamp to datetime
# =========================================

flux <- flux %>%
  mutate(
    datetime = ymd_hm(TIMESTAMP_START)
  )

# =========================================
# 4. Clean invalid values
# Replace invalid flux values (< -9990) with NA
# =========================================

flux <- flux %>%
  mutate(
    FC = ifelse(FC < -9990, NA, FC)
  )

# =========================================
# 5. Check missing values
# =========================================

sum(is.na(flux$FC))

summary(flux$FC)

# =========================================
# 6. Plot full-year CO2 flux time series
# =========================================

ggplot(flux, aes(x = datetime, y = FC)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(
    title = "2016 CO2 Flux Time Series (Cleaned)",
    x = "Date",
    y = "FC"
  )

# =========================================
# 7. Save full-year figure
# =========================================

ggsave(
  "/Users/caixiaoliang/Documents/FC_timeseries_2016.png",
  width = 12,
  height = 6,
  dpi = 300
)

# =========================================
# 8. Create daily mean dataset
# =========================================

flux_daily <- flux %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    FC_daily = mean(FC, na.rm = TRUE)
  )

# =========================================
# 9. Plot daily mean CO2 flux
# =========================================

ggplot(flux_daily, aes(x = date, y = FC_daily)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(
    title = "Daily Mean CO2 Flux",
    x = "Date",
    y = "Daily FC"
  )

# =========================================
# 10. Save daily mean figure
# =========================================

ggsave(
  "/Users/caixiaoliang/Documents/Daily_FC_2016.png",
  width = 12,
  height = 6,
  dpi = 300
)

# =========================================
# 11. Save daily dataset
# =========================================

write_csv(
  flux_daily,
  "/Users/caixiaoliang/Documents/flux_daily_2016.csv"
)

# =========================================
# 12. Finished
# =========================================

print("Flux analysis workflow completed successfully.")
