# Load required packages
library(dplyr)
library(readr)
library(corrr)     # for correlation matrices
library(officer)   # for Word export
library(flextable) # for table formatting

# Select relevant variables
cor_data <- master_data %>%
  select(Disbelief, PowerDistance, GNI_per_capita) %>%
  na.omit()

# Compute correlation matrix
cor_matrix <- correlate(cor_data, method = "pearson") %>%
  shave() %>%
  fashion()

# Print to console
print(cor_matrix)

# Save as CSV for backup
write_csv(cor_matrix, "Data/correlation_matrix.csv")

# Export to Word (formatted)
doc <- read_docx() %>%
  body_add_par("Correlation Matrix: Pearson’s r", style = "heading 1") %>%
  body_add_flextable(flextable(cor_matrix))

print(doc, target = "Figures/correlation_matrix.docx")

cat("Correlation matrix saved to Data/correlation_matrix.csv and Figures/correlation_matrix.docx\n")
