# Load required packages
library(dplyr)
library(readr)
library(flextable)
library(officer)

# Load summary table
wave_summary <- read_csv("Data/wave_summary.csv")

# Create styled flextable
ft <- flextable(wave_summary) %>%
  set_caption("Wave-Level Summary Statistics (Mean ± SD)") %>%
  autofit() %>%
  theme_booktabs() %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "center", part = "all") %>%
  bold(part = "header")

# Export to Word document
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par(" ", style = "Normal")  # spacing

print(doc, target = "Data/wave_summary.docx")

# Optional: confirmation message
cat("Wordocument saved to Data/wave_summary.docx\n")
