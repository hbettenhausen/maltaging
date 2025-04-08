# Load libraries
library(tidyverse)
library(ggrepel)

# Load dataset
allmets <- read_csv("AllMetsv.1_03282025.csv")

# Filter to only Maltster B
malt_b <- allmets %>%
  filter(Origin == "Maltster B")

# Split metadata and metabolite data
meta_b <- malt_b %>%
  select(ID, Variety, Origin, Month)

mets_b <- malt_b %>%
  select(-ID, -Variety, -Origin, -Month)

# Run PCA
pca_res_b <- prcomp(mets_b, scale. = TRUE)

# Combine PCA scores with metadata
pca_df_b <- as.data.frame(pca_res_b$x) %>%
  bind_cols(meta_b)

# PCA Plot
ggplot(pca_df_b, aes(x = PC1, y = PC2)) +
  geom_point(shape = 17, color = "black", size = 4, stroke = 1.2) +  # All black triangles
  geom_text_repel(aes(label = Month), size = 4, color = "black", max.overlaps = 100) +
  geom_hline(yintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  geom_vline(xintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  labs(
    title = "Figure 1c: PCA of Maltster B by Age",
    x = paste0("PC1 (", round(summary(pca_res_b)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res_b)$importance[2, 2] * 100, 1), "%)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

