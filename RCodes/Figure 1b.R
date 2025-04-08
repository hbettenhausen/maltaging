# Load libraries (again, if you're in a new session)
library(tidyverse)
library(ggrepel)

# Load the updated dataset
allmets <- read_csv("AllMetsv.1_03282025.csv")

# Filter to only Maltster A & B
allmets_ab <- allmets %>%
  filter(Origin %in% c("Maltster A", "Maltster B"))

# Split metadata and metabolite data
meta_ab <- allmets_ab %>%
  select(ID, Variety, Origin, Month)

mets_ab <- allmets_ab %>%
  select(-ID, -Variety, -Origin, -Month)

# Run PCA
pca_res_ab <- prcomp(mets_ab, scale. = TRUE)

# Combine PCA scores with metadata
pca_df_ab <- as.data.frame(pca_res_ab$x) %>%
  bind_cols(meta_ab)

# Define shapes and colors
maltster_shapes_ab <- c("Maltster A" = 16, "Maltster B" = 17)
maltster_colors_ab <- c("Maltster A" = "black", "Maltster B" = "darkred")

# PCA Plot for 1b
ggplot(pca_df_ab, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Origin, color = Origin), size = 4, stroke = 1.2) +
  geom_text_repel(aes(label = Month), size = 4, color = "black", max.overlaps = 100) +
  geom_hline(yintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  geom_vline(xintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  labs(
    title = "Figure 1b: PCA of Maltsters A and B",
    x = paste0("PC1 (", round(summary(pca_res_ab)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res_ab)$importance[2, 2] * 100, 1), "%)")
  ) +
  scale_shape_manual(values = maltster_shapes_ab) +
  scale_color_manual(values = maltster_colors_ab) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "right"
  )
