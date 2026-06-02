# =========================
# 1. Load packages
# =========================

library(tidyverse)
library(lubridate)

# =========================
# 2. Read processed dataset
# =========================

flux <- read_csv(
    "output/analysis_2016.csv"
)

# =========================
# 3. Convert timestamp
# =========================

flux <- flux %>%
    mutate(
        datetime = ymd_hm(TIMESTAMP_START)
    )

# =========================
# 4. Dataset checking
# =========================

glimpse(flux)

summary(flux$datetime)

head(flux)

# =========================
# 5. Extract temporal variables
# =========================

flux <- flux %>%
    mutate(
        year  = year(datetime),
        month = month(datetime),
        day   = day(datetime),
        hour  = hour(datetime)
    )

# =========================
# 6. Save updated dataset
# =========================

write_csv(
    flux,
    "output/analysis_2016_datetime.csv"
)
