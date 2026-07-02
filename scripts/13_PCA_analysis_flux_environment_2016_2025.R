# ============================================================
# 13_PCA_analysis_flux_environment_2016_2025.R
# PCA Analysis of Carbon Fluxes and Environmental Drivers
# Site: MukaHead / CEMACS, Penang, Malaysia
# Author: Cai Xiaoliang
# ============================================================

library(tidyverse)
library(lubridate)
library(readr)

# =========================
# 0. File paths
# =========================

base_path <- "/Users/caixiaoliang/Documents"

daily_file <- file.path(
  base_path,
  "08_daily_partitioned_flux_2016_2025.csv"
)

pca_scores_file <- file.path(
  base_path,
  "13_pca_scores_2016_2025.csv"
)

pca_loadings_file <- file.path(
  base_path,
  "13_pca_loadings_2016_2025.csv"
)

pca_variance_file <- file.path(
  base_path,
  "13_pca_variance_explained_2016_2025.csv"
)

pca_dominant_pc1_file <- file.path(
  base_path,
  "13_pca_dominant_variables_PC1_2016_2025.csv"
)

pca_dominant_pc2_file <- file.path(
  base_path,
  "13_pca_dominant_variables_PC2_2016_2025.csv"
)

seasonal_pca_summary_file <- file.path(
  base_path,
  "13_seasonal_pca_score_summary_2016_2025.csv"
)

yearly_pca_summary_file <- file.path(
  base_path,
  "13_yearly_pca_score_summary_2016_2025.csv"
)

# Figures
fig_variance_file <- file.path(
  base_path,
  "13_PCA_variance_explained_2016_2025.png"
)

fig_biplot_file <- file.path(
  base_path,
  "13_PCA_biplot_flux_environment_2016_2025.png"
)

fig_seasonal_scores_file <- file.path(
  base_path,
  "13_seasonal_PCA_scores_2016_2025.png"
)

fig_yearly_scores_file <- file.path(
  base_path,
  "13_yearly_PCA_scores_2016_2025.png"
)

fig_loading_file <- file.path(
  base_path,
  "13_PCA_loadings_PC1_PC2_2016_2025.png"
)

# =========================
# 1. Read daily data
# =========================

daily_flux <- read_csv(
  daily_file,
  show_col_types = FALSE,
  na = c("NA", "-9999", "-9999.0")
)

# =========================
# 2. Check required columns
# =========================

required_cols <- c(
  "Year",
  "Date",
  "NEE_daily",
  "GPP_daily",
  "Reco_daily",
  "Rg_daily",
  "Tair_daily",
  "VPD_daily"
)

missing_cols <- setdiff(required_cols, names(daily_flux))

if (length(missing_cols) > 0) {
  cat("\nMissing columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

# =========================
# 3. Prepare PCA dataset
# =========================

pca_data <- daily_flux %>%
  mutate(
    Year = as.integer(Year),
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
    ),
    
    NEE_daily = as.numeric(NEE_daily),
    GPP_daily = as.numeric(GPP_daily),
    Reco_daily = as.numeric(Reco_daily),
    Rg_daily = as.numeric(Rg_daily),
    Tair_daily = as.numeric(Tair_daily),
    VPD_daily = as.numeric(VPD_daily)
  ) %>%
  select(
    Year,
    Date,
    Season,
    NEE_daily,
    GPP_daily,
    Reco_daily,
    Rg_daily,
    Tair_daily,
    VPD_daily
  ) %>%
  drop_na()

cat("\n===== PCA input check =====\n")
cat("Rows used for PCA:", nrow(pca_data), "\n")
cat("Years:\n")
print(table(pca_data$Year))
cat("Seasons:\n")
print(table(pca_data$Season))

if (nrow(pca_data) < 30) {
  stop("Too few complete records for PCA.")
}

pca_numeric <- pca_data %>%
  select(
    NEE_daily,
    GPP_daily,
    Reco_daily,
    Rg_daily,
    Tair_daily,
    VPD_daily
  )

# =========================
# 4. Run PCA
# =========================

pca_result <- prcomp(
  pca_numeric,
  center = TRUE,
  scale. = TRUE
)

# =========================
# 5. Variance explained
# =========================

pca_variance <- tibble(
  PC = paste0("PC", seq_along(pca_result$sdev)),
  Eigenvalue = pca_result$sdev^2,
  Variance_Explained = (pca_result$sdev^2) / sum(pca_result$sdev^2),
  Cumulative_Variance = cumsum((pca_result$sdev^2) / sum(pca_result$sdev^2))
)

cat("\n===== PCA variance explained =====\n")
print(pca_variance)

write_csv(
  pca_variance,
  pca_variance_file,
  na = "NA"
)

# =========================
# 6. PCA scores
# =========================

pca_scores <- as.data.frame(pca_result$x) %>%
  as_tibble() %>%
  bind_cols(
    pca_data %>%
      select(
        Year,
        Date,
        Season
      ),
    .
  )

write_csv(
  pca_scores,
  pca_scores_file,
  na = "NA"
)

# =========================
# 7. PCA loadings
# =========================

pca_loadings <- as.data.frame(pca_result$rotation) %>%
  rownames_to_column(var = "Variable") %>%
  as_tibble()

cat("\n===== PCA loadings =====\n")
print(pca_loadings)

write_csv(
  pca_loadings,
  pca_loadings_file,
  na = "NA"
)

# =========================
# 8. Dominant variables for PC1 and PC2
# =========================

dominant_pc1 <- pca_loadings %>%
  select(Variable, PC1) %>%
  mutate(
    Abs_Loading = abs(PC1)
  ) %>%
  arrange(desc(Abs_Loading))

dominant_pc2 <- pca_loadings %>%
  select(Variable, PC2) %>%
  mutate(
    Abs_Loading = abs(PC2)
  ) %>%
  arrange(desc(Abs_Loading))

cat("\n===== Dominant variables for PC1 =====\n")
print(dominant_pc1)

cat("\n===== Dominant variables for PC2 =====\n")
print(dominant_pc2)

write_csv(
  dominant_pc1,
  pca_dominant_pc1_file,
  na = "NA"
)

write_csv(
  dominant_pc2,
  pca_dominant_pc2_file,
  na = "NA"
)

# =========================
# 9. Seasonal PCA score summary
# =========================

seasonal_pca_summary <- pca_scores %>%
  group_by(Season) %>%
  summarise(
    n = n(),
    PC1_mean = mean(PC1, na.rm = TRUE),
    PC1_sd = sd(PC1, na.rm = TRUE),
    PC2_mean = mean(PC2, na.rm = TRUE),
    PC2_sd = sd(PC2, na.rm = TRUE),
    PC3_mean = mean(PC3, na.rm = TRUE),
    PC3_sd = sd(PC3, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n===== Seasonal PCA score summary =====\n")
print(seasonal_pca_summary)

write_csv(
  seasonal_pca_summary,
  seasonal_pca_summary_file,
  na = "NA"
)

# =========================
# 10. Yearly PCA score summary
# =========================

yearly_pca_summary <- pca_scores %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    PC1_mean = mean(PC1, na.rm = TRUE),
    PC1_sd = sd(PC1, na.rm = TRUE),
    PC2_mean = mean(PC2, na.rm = TRUE),
    PC2_sd = sd(PC2, na.rm = TRUE),
    PC3_mean = mean(PC3, na.rm = TRUE),
    PC3_sd = sd(PC3, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n===== Yearly PCA score summary =====\n")
print(yearly_pca_summary, n = Inf)

write_csv(
  yearly_pca_summary,
  yearly_pca_summary_file,
  na = "NA"
)

# =========================
# 11. Scree plot
# =========================

p_variance <- ggplot(
  pca_variance,
  aes(x = PC, y = Variance_Explained)
) +
  geom_col(fill = "steelblue") +
  geom_line(
    aes(y = Cumulative_Variance, group = 1),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    aes(y = Cumulative_Variance),
    color = "red",
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "PCA Variance Explained (2016–2025)",
    x = "Principal Component",
    y = "Variance explained / cumulative variance"
  )

print(p_variance)

ggsave(
  fig_variance_file,
  plot = p_variance,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 12. PCA biplot
# =========================

scores_plot <- pca_scores %>%
  select(
    Year,
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

p_biplot <- ggplot(
  scores_plot,
  aes(
    x = PC1,
    y = PC2,
    color = Season
  )
) +
  geom_point(
    alpha = 0.45,
    size = 1.4
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
    arrow = arrow(length = unit(0.25, "cm")),
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
    title = "PCA Biplot of Carbon Fluxes and Environmental Drivers (2016–2025)",
    x = paste0("PC1 (", round(pca_variance$Variance_Explained[1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(pca_variance$Variance_Explained[2] * 100, 1), "%)"),
    color = "Season"
  )

print(p_biplot)

ggsave(
  fig_biplot_file,
  plot = p_biplot,
  width = 10,
  height = 8,
  dpi = 300
)

# =========================
# 13. Seasonal PCA score boxplots
# =========================

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

p_seasonal_scores <- ggplot(
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
    title = "Seasonal Distribution of PCA Scores (2016–2025)",
    x = "Season",
    y = "PCA score"
  )

print(p_seasonal_scores)

ggsave(
  fig_seasonal_scores_file,
  plot = p_seasonal_scores,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 14. Yearly PCA score boxplots
# =========================

pca_year_long <- pca_scores %>%
  select(
    Year,
    PC1,
    PC2
  ) %>%
  pivot_longer(
    cols = c(PC1, PC2),
    names_to = "PC",
    values_to = "Score"
  )

p_yearly_scores <- ggplot(
  pca_year_long,
  aes(
    x = factor(Year),
    y = Score,
    fill = factor(Year)
  )
) +
  geom_boxplot(
    alpha = 0.7,
    outlier.alpha = 0.3
  ) +
  facet_wrap(
    ~ PC,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Yearly Distribution of PCA Scores (2016–2025)",
    x = "Year",
    y = "PCA score"
  )

print(p_yearly_scores)

ggsave(
  fig_yearly_scores_file,
  plot = p_yearly_scores,
  width = 12,
  height = 6,
  dpi = 300
)

# =========================
# 15. PCA loading plot
# =========================

loading_long <- pca_loadings %>%
  select(
    Variable,
    PC1,
    PC2
  ) %>%
  pivot_longer(
    cols = c(PC1, PC2),
    names_to = "PC",
    values_to = "Loading"
  )

p_loading <- ggplot(
  loading_long,
  aes(
    x = reorder(Variable, abs(Loading)),
    y = Loading,
    fill = PC
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "PCA Loadings for PC1 and PC2 (2016–2025)",
    x = "Variable",
    y = "Loading"
  )

print(p_loading)

ggsave(
  fig_loading_file,
  plot = p_loading,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 16. Finished
# =========================

saved_files <- c(
  pca_scores_file,
  pca_loadings_file,
  pca_variance_file,
  pca_dominant_pc1_file,
  pca_dominant_pc2_file,
  seasonal_pca_summary_file,
  yearly_pca_summary_file,
  fig_variance_file,
  fig_biplot_file,
  fig_seasonal_scores_file,
  fig_yearly_scores_file,
  fig_loading_file
)

cat("\n=========================================\n")
cat("13 PCA analysis completed successfully.\n")
cat("=========================================\n")
cat("Saved files:\n")
print(saved_files)
