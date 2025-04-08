# Load libraries ---------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)

# Load and inspect data --------------------------------
data_raw <- read_excel("NewPCA for R_03272025.xlsx")

# OPTIONAL: View column types to decide what to keep
glimpse(data_raw)

# Select numeric columns only for PCA ------------------
data_numeric <- data_raw |>
  select(where(is.numeric)) |>
  drop_na()  # drop rows with missing values

# Run PCA ----------------------------------------------
pca_res <- prcomp(data_numeric, center = TRUE, scale. = TRUE)

# Scree Plot (variance explained by PCs) ---------------
fviz_eig(pca_res, addlabels = TRUE, barfill = "#00AFBB", barcolor = "#2E9FDF")

# Individuals Plot -------------------------------------
fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  col.ind = "cos2",   # Quality of representation
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

# Variables Plot (Loadings) ----------------------------
fviz_pca_var(
  pca_res,
  col.var = "contrib",  # Contribution to PCs
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

# Biplot (Individuals + Variables) ---------------------
fviz_pca_biplot(
  pca_res,
  repel = TRUE,
  col.var = "#2E9FDF", 
  col.ind = "#696969"
)

# Extract variable contributions -----------------------
var_contrib <- pca_res |>
  factoextra::get_pca_var() |>
  purrr::pluck("contrib") |>
  as.data.frame()

# View top contributors to PC1
top_pc1 <- var_contrib |>
  arrange(desc(Dim.1)) |>
  head(10)

# Print top contributors
print(top_pc1)
