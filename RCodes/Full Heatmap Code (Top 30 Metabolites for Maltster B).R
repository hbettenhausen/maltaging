# Load libraries
library(readr)
library(dplyr)
library(pheatmap)

# Load CSV (includes Metabolite + Class + Timepoints)
df <- read_csv("MaltsterB_LCMS_Heatmap_Zscore.csv")

# Convert to data.frame
df <- as.data.frame(df)

# Separate class column if present
metabolite_names <- df[[1]]
class_labels <- df[[2]]
df_numeric <- df[, -c(1, 2)]  # keep only timepoint columns

# Set rownames to metabolite names
rownames(df_numeric) <- metabolite_names

# Order time columns numerically
df_numeric <- df_numeric[, order(as.numeric(colnames(df_numeric)))]

# Select top 30 most variable metabolites
top_30_names <- names(sort(apply(df_numeric, 1, var), decreasing = TRUE))[1:30]
df_top <- df_numeric[top_30_names, ]

# Match class labels for selected metabolites
annotation_row <- data.frame(Class = class_labels[match(rownames(df_top), metabolite_names)])
rownames(annotation_row) <- rownames(df_top)

# Draw heatmap with clustering and class labels
pheatmap(
  mat = df_top,
  scale = "none",  # already z-scored
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  annotation_row = annotation_row,
  color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
  show_rownames = TRUE,
  fontsize_row = 7,
  fontsize_col = 9,
  main = "LC–MS Heatmap with Clustering (Maltster B)"
)
