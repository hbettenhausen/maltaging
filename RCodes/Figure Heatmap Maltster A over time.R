##LCMS FOR MALTSTER A####


# Load required library
library(pheatmap)

# Load your z-scored heatmap data
heat_data <- read.csv("MaltsterA_GCMS_Heatmap_Zscore.csv", row.names = 1)

# Generate the heatmap
pheatmap(heat_data,
         scale = "none",                # already z-scored
         cluster_rows = TRUE,
         cluster_cols = FALSE,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         fontsize_row = 8,
         fontsize_col = 12,
         main = "GC-MS Metabolite Changes Over Time (Maltster A)",
         angle_col = 45)
##LCMS FOR MALTSTER A####
# Load required library
library(pheatmap)

# Load the z-scored LC-MS data for Maltster A
heat_data_lcms_a <- read.csv("MaltsterA_LCMS_Heatmap_Zscore.csv", row.names = 1)

# Select the top 30 metabolites
heat_data_subset_a <- heat_data_lcms_a[1:50,]  # Top 30 metabolites

# Reorder columns to keep months in order (1, 6, 12) manually
heat_data_subset_a <- heat_data_subset_a[, c("1", "6", "12")]

# Plot the heatmap with hierarchical clustering (no reordering for months)
pheatmap(heat_data_subset_a,
         scale = "none",                # Already z-scored
         cluster_rows = TRUE,           # Cluster metabolites by similarity
         cluster_cols = FALSE,          # Do not cluster columns (months stay in order)
         color = colorRampPalette(c("blue", "lightblue", "red"))(100), # Custom color gradient
         fontsize_row = 8,              # Font size for row labels (metabolites)
         fontsize_col = 12,             # Font size for column labels (months)
         main = "LC-MS Heatmap with Clustering (Maltster A)",
         angle_col = 45)                # Rotate column labels for better readability
