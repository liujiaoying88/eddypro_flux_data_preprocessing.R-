# =========================================
# GAM Modeling of NEE (2016)
# Site: MukaHead
# Author: Cai Xiaoliang
# =========================================

library(tidyverse)
library(lubridate)
library(mgcv)

# =========================================
# 1. Build GAM model
# =========================================

gam_model <- gam(
  Daily_NEE ~
    s(Daily_VPD) +
    s(Daily_Rg) +
    s(Daily_Tair),
  data = daily_flux
)

# =========================================
# 2. View GAM summary
# =========================================

summary(gam_model)

# =========================================
# 3. Plot GAM response curves
# =========================================

plot(
  gam_model,
  pages = 1,
  shade = TRUE
)

# =========================================
# 4. Save GAM response curves
# =========================================

png(
  "/Users/caixiaoliang/Documents/GAM_Response_Curves_2016.png",
  width = 3000,
  height = 2000,
  res = 300
)

plot(
  gam_model,
  pages = 1,
  shade = TRUE
)

dev.off()

# =========================================
# 5. Finished
# =========================================

print("GAM modeling and response curve analysis completed successfully.")
