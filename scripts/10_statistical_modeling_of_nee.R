# =========================================
# Statistical Modeling of NEE (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 1. Monthly regression model
# =========================================

model1 <- lm(
  Monthly_NEE ~ Monthly_VPD,
  data = monthly_flux
)

summary(model1)

# =========================================
# 2. Multiple regression model
# =========================================

model2 <- lm(
  Monthly_NEE ~ Monthly_VPD +
    Monthly_Rg +
    Monthly_Tair,
  data = monthly_flux
)

summary(model2)

# =========================================
# 3. Create daily dataset
# =========================================

daily_flux <- partitioned_data %>%
  mutate(
    Date = as.Date(DateTime)
  ) %>%
  group_by(Date) %>%
  summarise(
    Daily_NEE   = mean(NEE_U50_f, na.rm = TRUE),
    Daily_Rg    = mean(Rg_f, na.rm = TRUE),
    Daily_Tair  = mean(Tair_f, na.rm = TRUE),
    Daily_VPD   = mean(VPD_f, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================
# 4. Daily-scale regression model
# =========================================

daily_model <- lm(
  Daily_NEE ~ Daily_VPD +
    Daily_Rg +
    Daily_Tair,
  data = daily_flux
)

summary(daily_model)

# =========================================
# 5. Save daily dataset
# =========================================

write_csv(
  daily_flux,
  "/Users/caixiaoliang/Documents/daily_flux_for_modeling_2016.csv"
)

# =========================================
# 6. Finished
# =========================================

print("Statistical modeling of NEE completed successfully.")
