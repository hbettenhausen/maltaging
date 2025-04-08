# Load libraries
library(tidyverse)
library(ggrepel)

# Load the cleaned dataset with Maltster A/B/C labels
allmets <- read_csv("AllMetsv.1_03282025.csv")

# Split metadata and metabolite data
meta <- allmets %>%
  select(ID, Variety, Origin, Month)

mets <- allmets %>%
  select(-ID, -Variety, -Origin, -Month)

# Run PCA
pca_res <- prcomp(mets, scale. = TRUE)

# Combine PCA scores with metadata
pca_df <- as.data.frame(pca_res$x) %>%
  bind_cols(meta)

# Define shape and color mappings
maltster_shapes <- c("Maltster A" = 16, "Maltster B" = 17, "Maltster C" = 15)  # circle, triangle, square
month_colors <- c("1" = "#1b9e77", "6" = "#d95f02", "12" = "#7570b3")  # green, orange, purple (colorblind friendly)

# PCA Plot
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Origin, color = factor(Month)), size = 4, stroke = 1.2) +
  geom_text_repel(aes(label = Month), size = 4, color = "black", max.overlaps = 100) +
  geom_hline(yintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  geom_vline(xintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  labs(
    title = "Figure 1c: PCA Colored by Age (Month)",
    x = paste0("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)"),
    color = "Month"
  ) +
  scale_shape_manual(values = maltster_shapes) +
  scale_color_manual(values = month_colors) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 12),
    legend.position = "right"
  )
