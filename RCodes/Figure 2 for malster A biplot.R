# Load libraries
library(tidyverse)
library(ggrepel)

# Load GC-MS data
gcms_data <- read_csv("GCMS_ONLY_03282025.csv")

# Filter to Maltster A only
malt_a <- gcms_data %>%
  filter(Origin == "Maltster A")

# Split metadata and metabolite matrix
meta_a <- malt_a %>%
  select(ID, Variety, Origin, Month)

mets_a <- malt_a %>%
  select(-ID, -Variety, -Origin, -Month)

# Run PCA
pca_res_a <- prcomp(mets_a, scale. = TRUE)

# Scores (samples)
scores_a <- as.data.frame(pca_res_a$x) %>%
  bind_cols(meta_a)

# Loadings (metabolites)
loadings_a <- as.data.frame(pca_res_a$rotation)
loadings_a$metabolite <- rownames(loadings_a)

# Top 15 loading vectors (by PC1 + PC2 magnitude)
top_loadings_a <- loadings_a %>%
  mutate(magnitude = sqrt(PC1^2 + PC2^2)) %>%
  arrange(desc(magnitude)) %>%
  slice(1:15)

# Scale arrows
scaling_factor <- 10
top_loadings_a <- top_loadings_a %>%
  mutate(PC1_scaled = PC1 * scaling_factor,
         PC2_scaled = PC2 * scaling_factor)

# Plot
ggplot() +
  # Sample points
  geom_point(data = scores_a, aes(x = PC1, y = PC2), shape = 17, color = "black", size = 4, stroke = 1.2) +
  geom_text_repel(data = scores_a, aes(x = PC1, y = PC2, label = Month), size = 4, color = "black", max.overlaps = Inf) +
  
  # Arrows for loadings
  geom_segment(data = top_loadings_a,
               aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray30") +
  
  # Green dots at arrow tips
  geom_point(data = top_loadings_a,
             aes(x = PC1_scaled, y = PC2_scaled), shape = 21, fill = "forestgreen", color = "black", size = 3.5, stroke = 0.4) +
  
  # Metabolite labels
  geom_text_repel(data = top_loadings_a,
                  aes(x = PC1_scaled, y = PC2_scaled, label = metabolite),
                  size = 3.5, color = "gray10", max.overlaps = Inf) +
  
  # Crosshairs
  geom_hline(yintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  geom_vline(xintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  
  # Axis labels
  labs(
    title = "Extra Figure: PCA Biplot – GC-MS Features (Maltster A)",
    x = paste0("PC1 (", round(summary(pca_res_a)$importance[2,1]*100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res_a)$importance[2,2]*100, 1), "%)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )
