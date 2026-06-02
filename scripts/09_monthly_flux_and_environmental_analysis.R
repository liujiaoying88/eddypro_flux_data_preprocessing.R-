# =========================================
# Monthly Flux and Environmental Analysis (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 1. Reconstruct datetime sequence
# =========================================

partitioned_data <- partitioned_data %>%
  mutate(
    DateTime = ymd_hm("201601010030") +
      minutes(30) * (row_number() - 1)
  )

# =========================================
# 2. Calculate monthly mean variables
# =========================================

monthly_flux <- partitioned_data %>%
  mutate(
    Month = month(DateTime, label = TRUE)
  ) %>%
  group_by(Month) %>%
  summarise(
    Monthly_NEE   = mean(NEE_U50_f, na.rm = TRUE),
    Monthly_Rg    = mean(Rg_f, na.rm = TRUE),
    Monthly_Tair  = mean(Tair_f, na.rm = TRUE),
    Monthly_VPD   = mean(VPD_f, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================
# 3. View monthly dataset
# =========================================

monthly_flux

# =========================================
# 4. Plot monthly mean NEE
# =========================================

ggplot(monthly_flux,
       aes(x = Month,
           y = Monthly_NEE,
           group = 1)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(
    title = "Monthly Mean NEE",
    x = "Month",
    y = "Monthly NEE"
  )

ggsave(
  "figures/Monthly_Mean_NEE_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 5. Monthly environmental drivers
# =========================================

monthly_long <- monthly_flux %>%
  pivot_longer(
    cols = c(Monthly_Rg,
             Monthly_Tair,
             Monthly_VPD),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(monthly_long,
       aes(x = Month,
           y = Value,
           group = Variable,
           color = Variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Monthly Environmental Drivers",
    x = "Month",
    y = "Value"
  )

ggsave(
  "figures/Monthly_Environmental_Drivers_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 6. Standardized environmental drivers
# =========================================

monthly_scaled <- monthly_flux %>%
  mutate(
    Rg_scaled    = scale(Monthly_Rg)[,1],
    Tair_scaled  = scale(Monthly_Tair)[,1],
    VPD_scaled   = scale(Monthly_VPD)[,1]
  ) %>%
  select(
    Month,
    Rg_scaled,
    Tair_scaled,
    VPD_scaled
  ) %>%
  pivot_longer(
    cols = -Month,
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(monthly_scaled,
       aes(x = Month,
           y = Value,
           group = Variable,
           color = Variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Standardized Monthly Environmental Drivers",
    x = "Month",
    y = "Scaled Value"
  )

ggsave(
  "figures/Scaled_Environmental_Drivers_2016.png",
  width = 10,
  height = 6,
  dpi = 300
)

# =========================================
# 7. Correlation analysis
# =========================================

correlation_data <- monthly_flux %>%
  select(
    Monthly_NEE,
    Monthly_Rg,
    Monthly_Tair,
    Monthly_VPD
  )

cor_matrix <- cor(
  correlation_data,
  use = "complete.obs"
)

print(cor_matrix)

# =========================================
# 8. Save monthly dataset
# =========================================

write_csv(
  monthly_flux,
  "output/monthly_flux_2016.csv"
)

# =========================================
# 9. Finished
# =========================================

print("Monthly flux and environmental analysis completed successfully.")
