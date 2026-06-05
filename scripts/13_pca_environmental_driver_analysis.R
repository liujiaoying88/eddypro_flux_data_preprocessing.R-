# =========================================
# 13 PCA Analysis of Carbon Fluxes and Environmental Drivers
# Site: MukaHead / CEMACS, Penang, Malaysia
# Author: Cai Xiaoliang
# Purpose:
#   Principal Component Analysis (PCA) to identify dominant
#   environmental gradients controlling carbon fluxes.
#
# Core rule:
#   13 reads saved CSV files only.
# =========================================

library(tidyverse)
library(lubridate)

# =========================================
# 0. Parameters
# =========================================

analysis_year <- 2016
base_path <- "~/Documents"

daily_file <- file.path(
  base_path,
  paste0("daily_flux_for_modeling_", analysis_year, ".csv")
)

pca_scores_file <- file.path(
  base_path,
  paste0("pca_scores_", analysis_year, ".csv")
)

pca_loadings_file <- file.path(
  base_path,
  paste0("pca_loadings_", analysis_year, ".csv")
)

pca_variance_file <- file.path(
  base_path,
  paste0("pca_variance_explained_", analysis_year, ".csv")
)

# =========================================
# 1. Read daily data
# =========================================

daily_flux <- read_csv(
  daily_file,
  show_col_types = FALSE
)

required_cols <- c(
  "Date",
  "Daily_NEE_mean",
  "Daily_GPP_mean",
  "Daily_Reco_mean",
  "Daily_Rg_mean",
  "Daily_Tair_mean",
  "Daily_VPD_mean"
)

missing_cols <- setdiff(
  required_cols,
  names(daily_flux)
)

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# =========================================
# 2. Prepare PCA dataset
# =========================================

pca_data <- daily_flux %>%
  mutate(
    Date = as.Date(Date),
    Month_num = month(Date),
    Season = case_when(
      Month_num %in% c(12, 1, 2) ~ "Dry",
      Month_num %in% c(5, 6, 7, 8, 9, 10) ~ "Wet",
      Month_num %in% c(3, 4, 11) ~ "Transition",
      TRUE ~ NA_character_
    ),
    Season = factor(
      Season,
      levels = c("Dry", "Transition", "Wet")
    )
  ) %>%
  select(
    Date,
    Season,
    Daily_NEE_mean,
    Daily_GPP_mean,
    Daily_Reco_mean,
    Daily_Rg_mean,
    Daily_Tair_mean,
    Daily_VPD_mean
  ) %>%
  drop_na()

pca_numeric <- pca_data %>%
  select(
    Daily_NEE_mean,
    Daily_GPP_mean,
    Daily_Reco_mean,
    Daily_Rg_mean,
    Daily_Tair_mean,
    Daily_VPD_mean
  )

# =========================================
# 3. Run PCA
# =========================================

pca_result <- prcomp(
  pca_numeric,
  center = TRUE,
  scale. = TRUE
)

# =========================================
# 4. Variance explained
# =========================================

pca_variance <- tibble(
  PC = paste0(
    "PC",
    seq_along(pca_result$sdev)
  ),
  Eigenvalue = pca_result$sdev^2,
  Variance_Explained = (pca_result$sdev^2) / sum(pca_result$sdev^2),
  Cumulative_Variance = cumsum(
    (pca_result$sdev^2) / sum(pca_result$sdev^2)
  )
)

cat("\n===== PCA variance explained =====\n")
print(pca_variance)

write_csv(
  pca_variance,
  pca_variance_file
)

# =========================================
# 5. PCA scores
# =========================================

pca_scores <- as.data.frame(
  pca_result$x
) %>%
  as_tibble() %>%
  bind_cols(
    pca_data %>%
      select(
        Date,
        Season
      ),
    .
  )

write_csv(
  pca_scores,
  pca_scores_file
)

# =========================================
# 6. PCA loadings
# =========================================

pca_loadings <- as.data.frame(
  pca_result$rotation
) %>%
  rownames_to_column(
    var = "Variable"
  ) %>%
  as_tibble()

cat("\n===== PCA loadings =====\n")
print(pca_loadings)

write_csv(
  pca_loadings,
  pca_loadings_file
)

# =========================================
# 7. Scree plot
# =========================================

p1 <- ggplot(
  pca_variance,
  aes(
    x = PC,
    y = Variance_Explained
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  geom_line(
    aes(
      y = Cumulative_Variance,
      group = 1
    ),
    color = "red",
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      y = Cumulative_Variance
    ),
    color = "red",
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = paste0("PCA Variance Explained (", analysis_year, ")"),
    x = "Principal Component",
    y = "Variance Explained"
  )

print(p1)

ggsave(
  file.path(
    base_path,
    paste0("PCA_Variance_Explained_", analysis_year, ".png")
  ),
  p1,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 8. PCA biplot data preparation
# =========================================

scores_plot <- pca_scores %>%
  select(
    Date,
    Season,
    PC1,
    PC2
  )

loadings_plot <- pca_loadings %>%
  select(
    Variable,
    PC1,
    PC2
  ) %>%
  mutate(
    PC1 = PC1 * 3,
    PC2 = PC2 * 3
  )

# =========================================
# 9. PCA biplot
# =========================================

p2 <- ggplot(
  scores_plot,
  aes(
    x = PC1,
    y = PC2,
    color = Season
  )
) +
  geom_point(
    alpha = 0.6,
    size = 2
  ) +
  geom_segment(
    data = loadings_plot,
    aes(
      x = 0,
      y = 0,
      xend = PC1,
      yend = PC2
    ),
    inherit.aes = FALSE,
    arrow = arrow(
      length = unit(0.25, "cm")
    ),
    color = "black",
    linewidth = 0.8
  ) +
  geom_text(
    data = loadings_plot,
    aes(
      x = PC1,
      y = PC2,
      label = Variable
    ),
    inherit.aes = FALSE,
    color = "black",
    size = 4,
    vjust = -0.5
  ) +
  theme_minimal() +
  labs(
    title = paste0("PCA Biplot of Carbon Fluxes and Environmental Drivers (", analysis_year, ")"),
    x = paste0(
      "PC1 (",
      round(pca_variance$Variance_Explained[1] * 100, 1),
      "%)"
    ),
    y = paste0(
      "PC2 (",
      round(pca_variance$Variance_Explained[2] * 100, 1),
      "%)"
    ),
    color = "Season"
  )

print(p2)

ggsave(
  file.path(
    base_path,
    paste0("PCA_Biplot_Flux_Environment_", analysis_year, ".png")
  ),
  p2,
  width = 10,
  height = 8,
  dpi = 600
)

# =========================================
# 10. PC1 and PC2 seasonal boxplots
# =========================================

pca_scores_long <- pca_scores %>%
  select(
    Season,
    PC1,
    PC2
  ) %>%
  pivot_longer(
    cols = c(PC1, PC2),
    names_to = "PC",
    values_to = "Score"
  )

p3 <- ggplot(
  pca_scores_long,
  aes(
    x = Season,
    y = Score,
    fill = Season
  )
) +
  geom_boxplot(
    alpha = 0.7,
    outlier.alpha = 0.4
  ) +
  facet_wrap(
    ~ PC,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = paste0("Seasonal Distribution of PCA Scores (", analysis_year, ")"),
    x = "Season",
    y = "PCA Score"
  )

print(p3)

ggsave(
  file.path(
    base_path,
    paste0("Seasonal_PCA_Scores_", analysis_year, ".png")
  ),
  p3,
  width = 10,
  height = 6,
  dpi = 600
)

# =========================================
# 11. Identify dominant variables for PC1 and PC2
# =========================================

dominant_pc1 <- pca_loadings %>%
  select(
    Variable,
    PC1
  ) %>%
  mutate(
    Abs_Loading = abs(PC1)
  ) %>%
  arrange(
    desc(Abs_Loading)
  )

dominant_pc2 <- pca_loadings %>%
  select(
    Variable,
    PC2
  ) %>%
  mutate(
    Abs_Loading = abs(PC2)
  ) %>%
  arrange(
    desc(Abs_Loading)
  )

cat("\n===== Dominant variables for PC1 =====\n")
print(dominant_pc1)

cat("\n===== Dominant variables for PC2 =====\n")
print(dominant_pc2)

write_csv(
  dominant_pc1,
  file.path(
    base_path,
    paste0("pca_dominant_variables_PC1_", analysis_year, ".csv")
  )
)

write_csv(
  dominant_pc2,
  file.path(
    base_path,
    paste0("pca_dominant_variables_PC2_", analysis_year, ".csv")
  )
)

# =========================================
# 12. Finished
# =========================================

saved_files <- c(
  pca_scores_file,
  pca_loadings_file,
  pca_variance_file,
  file.path(base_path, paste0("pca_dominant_variables_PC1_", analysis_year, ".csv")),
  file.path(base_path, paste0("pca_dominant_variables_PC2_", analysis_year, ".csv")),
  file.path(base_path, paste0("PCA_Variance_Explained_", analysis_year, ".png")),
  file.path(base_path, paste0("PCA_Biplot_Flux_Environment_", analysis_year, ".png")),
  file.path(base_path, paste0("Seasonal_PCA_Scores_", analysis_year, ".png"))
)

cat("\n=========================================\n")
cat("13 PCA analysis completed successfully\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
