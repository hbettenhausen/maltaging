# Load libraries
library(tidyverse)
library(ggfortify)

# Load your cleaned data
allmets <- read_csv("AllMetsv.1_03282025.csv")

# View column names to double check
# glimpse(allmets)

# Split metadata and metabolites
meta <- allmets %>%
  select(ID, Variety, Origin, Month)

mets <- allmets %>%
  select(-ID, -Variety, -Origin, -Month)

# Run PCA
pca_res <- prcomp(mets, scale. = TRUE)

# Combine PCA scores and metadata
pca_df <- as.data.frame(pca_res$x) %>%
  bind_cols(meta)

# Plot PCA: Color by Origin (i.e., maltster), shape by Month
ggplot(pca_df, aes(x = PC1, y = PC2, color = Origin, shape = factor(Month))) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Figure 1a: PCA of All Maltsters",
    x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)")
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))


#____________________
#V.2 from GPT with different metrics
# Load libraries
library(tidyverse)
library(ggrepel)

# Load your updated dataset
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

# Define shapes and colors for each maltster
maltster_shapes <- c("Maltster A" = 16, "Maltster B" = 17, "Maltster C" = 15)  # circle, triangle, square
maltster_colors <- c("Maltster A" = "black", "Maltster B" = "darkred", "Maltster C" = "steelblue")

# PCA Plot
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Origin, color = Origin), size = 4, stroke = 1.2) +
  geom_text_repel(aes(label = Month), size = 4, color = "black") +
  labs(
    title = "Figure 1a: PCA of All Maltsters",
    x = paste0("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)")
  ) +
  scale_shape_manual(values = maltster_shapes) +
  scale_color_manual(values = maltster_colors) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "right"
  )
###v.3 from GPT with crosshairs and fixing errors
# Load libraries
library(tidyverse)
library(ggrepel)

# Load your updated dataset
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

# Define shapes and colors for each maltster
maltster_shapes <- c("Maltster A" = 16, "Maltster B" = 17, "Maltster C" = 15)  # circle, triangle, square
maltster_colors <- c("Maltster A" = "black", "Maltster B" = "darkred", "Maltster C" = "steelblue")

# PCA Plot with fixes and crosshairs
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Origin, color = Origin), size = 4, stroke = 1.2) +
  geom_text_repel(aes(label = Month), size = 4, color = "black", max.overlaps = 100) +
  geom_hline(yintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  geom_vline(xintercept = 0, linewidth = 1, color = "gray30", linetype = "dashed") +
  labs(
    title = "Figure 1a: PCA of All Maltsters",
    x = paste0("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)")
  ) +
  scale_shape_manual(values = maltster_shapes) +
  scale_color_manual(values = maltster_colors) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "right"
  )


