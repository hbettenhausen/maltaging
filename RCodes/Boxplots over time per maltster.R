# Load required libraries
library(tidyverse)

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

# Plot for Maltster B
p <- ggplot(subset(metabolites_long, Origin == "Maltster B"), aes(x = as.factor(Month), y = Abundance, fill = Origin)) +
  geom_boxplot() +
  facet_wrap(~Metabolite, scales = "free_y") +  # Separate plots for each metabolite
  theme_minimal(base_size = 16) +  # Increase base size for better visibility
  labs(x = "Month", y = "Abundance", title = "Boxplots for Metabolites Over Time (Maltster B)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_fill_manual(values = c("Maltster B" = "green"))

# Print the plot to display
print(p)

# Optionally, save the plot as a PNG manually from RStudio (or use the following command to save it)
# png("MaltsterA_Boxplots.png", width = 12, height = 10, units = "in", res = 300)
# print(p)
# dev.off()  # Close the device after saving the plot
