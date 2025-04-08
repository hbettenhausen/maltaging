# Load libraries ----------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)

# Load the data -----------------------------------------
data_raw <- read_excel("NewPCA for R_03272025.xlsx")

# Inspect the structure (optional)
glimpse(data_raw)

# Create individual labels: Variety_Month ---------------
labels <- data_raw |>
  drop_na() |>
  mutate(label = paste(Variety, Month, sep = "_")) |>
  pull(label)

# Select numeric columns and remove missing -------------
data_numeric <- data_raw |>
  select(where(is.numeric)) |>
  drop_na()

# Run PCA (centered + scaled) ---------------------------
pca_res <- prcomp(data_numeric, center = TRUE, scale. = TRUE)

# Scree Plot: Variance explained ------------------------
fviz_eig(pca_res, addlabels = TRUE, barfill = "#00AFBB", barcolor = "#2E9FDF") +
  ggtitle("Scree Plot: Variance Explained by PCs")

# Individuals Plot: With custom labels ------------------
fviz_pca_ind(
  pca_res,
  label = "none",  # hide auto labels
  geom.ind = "text",
  repel = TRUE,
  col.ind = "cos2",  # quality of representation
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  geom_text(aes(label = labels), size = 3, vjust = 1.5) +
  ggtitle("PCA: Individuals Labeled by Variety and Month")

# Variables Plot: Contributions to PCs ------------------
fviz_pca_var(
  pca_res,
  col.var = "contrib", 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) +
  ggtitle("PCA: Variable Contributions")

# Biplot: Individuals + Variables -----------------------
fviz_pca_biplot(
  pca_res,
  repel = TRUE,
  label = "var",
  col.var = "#2E9FDF",
  col.ind = "#696969"
)

# Extract variable contributions ------------------------
var_contrib <- pca_res |>
  get_pca_var() |>
  purrr::pluck("contrib") |>
  as.data.frame()

# Show top 10 contributors to PC1 -----------------------
top_pc1 <- var_contrib |>
  arrange(desc(Dim.1)) |>
  head(10)

print(top_pc1)
