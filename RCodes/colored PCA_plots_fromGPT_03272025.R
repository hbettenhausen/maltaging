# Load libraries ----------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)

# Load the data -----------------------------------------
data_raw <- read_excel("NewPCA for R_03272025.xlsx")

# Drop rows with missing numeric data -------------------
data_clean <- data_raw |>
  drop_na()  # Apply drop_na() once to ensure sync

# Create custom labels ----------------------------------
labels <- data_clean |>
  mutate(label = paste(Variety, Month, sep = "_")) |>
  pull(label)

# Extract Variety as a grouping variable ----------------
variety_group <- data_clean |>
  pull(Variety)

# Keep only numeric data for PCA ------------------------
data_numeric <- data_clean |>
  select(where(is.numeric))

# Run PCA -----------------------------------------------
pca_res <- prcomp(data_numeric, center = TRUE, scale. = TRUE)

# Individuals Plot: Color by Variety --------------------
fviz_pca_ind(
  pca_res,
  label = "none",            # No default labels
  geom = "point",            # Use points
  habillage = variety_group, # Color by Variety
  addEllipses = TRUE,        # Optional: draw confidence ellipses
  ellipse.type = "confidence",
  repel = TRUE
) +
  ggtitle("PCA: Individuals Colored by Variety") +
  theme_minimal()

# OPTIONAL: Add text labels manually (Variety_Month)
fviz_pca_ind(
  pca_res,
  geom.ind = "text",
  label = labels,
  col.ind = variety_group,
  palette = "jco",          # or choose other ggplot2 palettes
  repel = TRUE
) +
  ggtitle("PCA: Individuals Labeled and Colored by Variety") +
  theme_minimal()

