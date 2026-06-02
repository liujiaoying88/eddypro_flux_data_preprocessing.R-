# =========================
# 1. Load packages
# =========================

library(tidyverse)

# =========================
# 2. Read EddyPro dataset
# =========================

flux <- read_csv(
    "/Users/caixiaoliang/Documents/eddypro_muka_head01_fluxnet_2016_FINAL.csv"
)

# =========================
# 3. Select core variables
# =========================

analysis <- flux %>%
    select(
        TIMESTAMP_START,
        FC,
        LE,
        H,
        USTAR,
        TA_EP,
        RH_EP,
        VPD_EP,
        SW_IN_POT
    )

# =========================
# 4. Check dataset
# =========================

glimpse(analysis)

summary(analysis)

head(analysis)

# =========================
# 5. Save new CSV
# =========================

write_csv(
    analysis,
    "/Users/caixiaoliang/Documents/analysis_2016.csv"
)

# =========================
# 6. Plot first figure
# =========================

ggplot(analysis, aes(x = TA_EP, y = FC)) +
    geom_point(alpha = 0.3) +
    theme_minimal()
