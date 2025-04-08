# Standardize numeric data (same as PCA input)
scaled_data <- scale(data_numeric)

# Add row names (Variety_Month labels)
rownames(scaled_data) <- labels
# Base heatmap (with row/column clustering)
heatmap(scaled_data,
        scale = "none",   # Already scaled
        Rowv = TRUE,      # Cluster rows
        Colv = TRUE,      # Cluster columns
        col = colorRampPalette(c("navy", "white", "firebrick3"))(100),
        margins = c(8, 8),
        main = "Heatmap: Scaled Variables by Variety_Month")
# Install if needed
# install.packages("pheatmap")
library(pheatmap)

# Optional: create annotation by variety
annotation_row <- data.frame(Variety = variety_group)
rownames(annotation_row) <- labels

# Generate heatmap
pheatmap(
  mat = scaled_data,
  annotation_row = annotation_row,
  color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_rownames = TRUE,
  fontsize_row = 7,
  main = "Heatmap of Scaled Variables by Sample"
)
