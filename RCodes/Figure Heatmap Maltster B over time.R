##GCMS

# Load required library
library(pheatmap)

# Load your z-scored heatmap data GCMS
heat_data_b <- read.csv("MaltsterB_LCMS_Heatmap_Zscore.csv", row.names = 1)

# Generate the heatmap
pheatmap(heat_data_b,
         scale = "none",                # already z-scored
         cluster_rows = TRUE,
         cluster_cols = FALSE,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         fontsize_row = 8,
         fontsize_col = 12,
         main = "GC-MS Metabolite Changes Over Time (Maltster B)",
         angle_col = 45)

##LCMS FOR MALTSTER B##
# Load required library
library(pheatmap)

# Load the z-scored LC-MS data for Maltster B
heat_data_lcms_b <- read.csv("MaltsterB_LCMS_Heatmap_Zscore.csv", row.names = 1)

# Plot the heatmap
pheatmap(heat_data_lcms_b,
         scale = "none",                # already z-scored
         cluster_rows = TRUE,           # Cluster metabolites by similarity
         cluster_cols = FALSE,          # Do not cluster time points
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         fontsize_row = 8,              # Font size for row labels (metabolites)
         fontsize_col = 12,             # Font size for column labels (time points)
         main = "LC-MS Class-Level Changes Over Time (Maltster B)",
         angle_col = 45)                # Rotate column labels for better readability

######simplified
# Load required library
library(pheatmap)

# Load the z-scored LC-MS data (already simplified)
heat_data_lcms_b <- read.csv("MaltsterB_LCMS_Heatmap_Zscore.csv", row.names = 1)

# Select a subset of metabolites (top 10 for simplicity)
heat_data_subset <- heat_data_lcms_b[1:30,]  # Adjust this as needed for top metabolites

# Plot the simplified heatmap
pheatmap(heat_data_subset,
         scale = "none",                # Already z-scored
         cluster_rows = FALSE,          # No clustering for rows (metabolites)
         cluster_cols = FALSE,          # No clustering for columns (months)
         color = colorRampPalette(c("white", "lightblue", "red"))(100), # Simple color gradient
         fontsize_row = 8,              # Font size for row labels (metabolites)
         fontsize_col = 12,             # Font size for column labels (months)
         main = "Simplified LC-MS Heatmap (Maltster B)",
         angle_col = 45)                # Rotate column labels for better readability
####
# Load required library
library(pheatmap)

# Load the z-scored LC-MS data (already simplified)
heat_data_lcms_b <- read.csv("MaltsterB_LCMS_Heatmap_Zscore.csv", row.names = 1)

# Select the top 30 metabolites
heat_data_subset <- heat_data_lcms_b[1:30,]  # Top 30 metabolites

# Reorder columns to keep months in order (1, 6, 12)
heat_data_subset <- heat_data_subset[, c("1", "6", "12")]

# Plot the heatmap with hierarchical clustering
pheatmap(heat_data_subset,
         scale = "none",                # Already z-scored
         cluster_rows = TRUE,           # Cluster metabolites by similarity
         cluster_cols = TRUE,           # Cluster months by similarity (but months stay in order)
         color = colorRampPalette(c("blue", "lightblue", "red"))(100), # Color gradient for values
         fontsize_row = 8,              # Font size for row labels (metabolites)
         fontsize_col = 12,             # Font size for column labels (months)
         main = "LC-MS Heatmap with Clustering (Maltster B)",
         angle_col = 45)                # Rotate column labels for better readability
