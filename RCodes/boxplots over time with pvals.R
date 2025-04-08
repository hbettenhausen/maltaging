# Load required libraries
library(tidyverse)
library(ggsignif)  # For significance annotations

# Load the updated dataset
gcms_aa_data <- read.csv("GCMS_ONLY_03282025_AAONLY.csv")

# Subset the data to select only the metabolites and keep the necessary columns
metabolites_data <- gcms_aa_data %>%
  select(ID, Variety, Origin, Month, L.Proline, Fructose, L.Threonine, Maltose,
         L.Phenylalanine, L.Valine, L.Tryptophan, L.Isoleucine, Isobutyric.Acid, L.Alanine,
         Acetaldehyde, Diacetyl)

# Reshape the data into a long format for easy plotting
metabolites_long <- metabolites_data %>%
  gather(key = "Metabolite", value = "Abundance", -ID, -Variety, -Origin, -Month)

# Plot for Maltster A
p <- ggplot(subset(metabolites_long, Origin == "Maltster A"), aes(x = as.factor(Month), y = Abundance, fill = Origin)) +
  geom_boxplot() +
  facet_wrap(~Metabolite, scales = "free_y") +  # Separate plots for each metabolite
  theme_minimal(base_size = 16) +  # Increase base size for better visibility
  labs(x = "Month", y = "Abundance", title = "Boxplots for Metabolites Over Time (Maltster A)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_fill_manual(values = c("Maltster A" = "lightblue")) +
  geom_signif(comparisons = list(c("1", "6"), c("6", "12")), map_signif_level = TRUE)  # Add p-value annotations

# Print the plot to display in RStudio
print(p)

######WITH PVALS 
# Load required libraries
library(tidyverse)
library(ggsignif)

# Load the updated dataset
gcms_aa_data <- read.csv("GCMS_ONLY_03282025_AAONLY.csv")

# Subset the data to select only the metabolites and keep the necessary columns
metabolites_data <- gcms_aa_data %>%
  select(ID, Variety, Origin, Month, L.Proline, Fructose, L.Threonine, Maltose,
         L.Phenylalanine, L.Valine, L.Tryptophan, L.Isoleucine, Isobutyric.Acid, L.Alanine,
         Acetaldehyde, Diacetyl)

# Reshape the data into a long format for easy plotting
metabolites_long <- metabolites_data %>%
  gather(key = "Metabolite", value = "Abundance", -ID, -Variety, -Origin, -Month)

# Example p-values (replace with actual p-values for each comparison)
p_values <- data.frame(
  Metabolite = c("L-Proline", "Fructose", "L-Threonine", "Maltose", "L-Phenylalanine"),
  comparison = c("Month 1 vs Month 6", "Month 6 vs Month 12", "Month 1 vs Month 6", "Month 6 vs Month 12", "Month 1 vs Month 6"),
  pval = c(0.02, 0.10, 0.04, 0.01, 0.15)
)

# Convert p-values to text (show "NS" for non-significant comparisons)
p_values$pval_text <- ifelse(p_values$pval <= 0.05, sprintf("%.3f", p_values$pval), "NS")

# Plot for Maltster A
p <- ggplot(subset(metabolites_long, Origin == "Maltster A"), aes(x = as.factor(Month), y = Abundance, fill = Origin)) +
  geom_boxplot() +
  facet_wrap(~Metabolite, scales = "free_y") +  # Separate plots for each metabolite
  theme_minimal(base_size = 16) +  # Increase base size for better visibility
  labs(x = "Month", y = "Abundance", title = "Boxplots for Metabolites Over Time (Maltster A)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_fill_manual(values = c("Maltster A" = "lightblue")) +
  # Add p-values (or "NS") for each comparison
  geom_text(data = p_values, 
            aes(x = 1.5, y = 8, label = pval_text), 
            inherit.aes = FALSE, size = 4) +
  geom_text(data = p_values, 
            aes(x = 2.5, y = 8, label = pval_text), 
            inherit.aes = FALSE, size = 4) +
  geom_text(data = p_values, 
            aes(x = 3.5, y = 8, label = pval_text), 
            inherit.aes = FALSE, size = 4)

# Print the plot to display in RStudio
print(p)

