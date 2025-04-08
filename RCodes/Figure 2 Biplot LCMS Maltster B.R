# Load libraries
library(tidyverse)
library(ggrepel)

# Load LC-MS data
lcms_data <- read_csv("LCMS_ONLY_03282925.csv")

# Filter to Maltster B
malt_b <- lcms_data %>%
  filter(Origin == "Maltster B")

# Split metadata and metabolites
meta_b <- malt_b %>%
  select(ID, Variety, Origin, Month)

mets_b <- malt_b %>%
  select(-ID, -Variety, -Origin, -Month)

# PCA
pca_res <- prcomp(mets_b, scale. = TRUE)

# Scores
scores <- as.data.frame(pca_res$x) %>%
  bind_cols(meta_b)

# Loadings
loadings <- as.data.frame(pca_res$rotation)
loadings$metabolite <- rownames(loadings)

# 🧼 Clean metabolite names by removing ".1", ".2", etc.
loadings <- loadings %>%
  mutate(class = str_replace(metabolite, "\\.\\d+$", ""))

# 🎯 Define classes to highlight
highlight_classes <- c(
  "Dipeptides", "Phytoceramides", "Fatty acid esters",
  "Tocotrienols", "Indoles", "Lineolic acids and derivatives"
)

# Group everything else as "Other"
loadings <- loadings %>%
  mutate(class_group = if_else(class %in% highlight_classes, class, "Other"))

# Scale loadings using eigenvalues
eigvals <- pca_res$sdev^2
loadings <- loadings %>%
  mutate(
    PC1_scaled = PC1 * eigvals[1],
    PC2_scaled = PC2 * eigvals[2]
  )

# 🎨 Color palette
class_colors <- c(
  "Dipeptides" = "steelblue",
  "Phytoceramides" = "darkgreen",
  "Fatty acid esters" = "firebrick",
  "Tocotrienols" = "goldenrod",
  "Indoles" = "purple",
  "Lineolic acids and derivatives" = "darkorange",
  "Other" = "gray70"
)

# Plot
ggplot() +
  geom_point(data = scores, aes(x = PC1, y = PC2), shape = 17, color = "black", size = 4, stroke = 1.2) +
  geom_text_repel(data = scores, aes(x = PC1, y = PC2, label = Month), size = 4, color = "black", max.overlaps = Inf) +
  
  geom_point(data = loadings, aes(x = PC1_scaled, y = PC2_scaled, fill = class_group),
             shape = 21, color = "black", size = 3, stroke = 0.3) +
  
  geom_hline(yintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  geom_vline(xintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  
  labs(
    title = "PCA Biplot – LC-MS (Maltster B, Highlighted Chemical Classes)",
    x = paste0("PC1 (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)"),
    fill = "Chemical Class"
  ) +
  scale_fill_manual(values = class_colors) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )

