# ─────────────────────────────────────────────
# Load libraries (if needed)
# ─────────────────────────────────────────────
library(dplyr)
library(ggplot2)
library(flextable)
library(officer)

# ─────────────────────────────────────────────
# Load wave-level data
# ─────────────────────────────────────────────
wvs_national <- read.csv("Data/wave_summary.csv")

# Clean column types
wvs_national$PowerDistance <- as.numeric(wvs_national$PowerDistance)

# Target years to compare with Hofstede
target_years <- c(1990, 1995, 2020)

# Create table dynamically for available years
comparison_tbl <- wvs_national %>%
  filter(Year %in% target_years) %>%
  mutate(
    Hofstede_PDI = 77,
    WVS_PC1_PDI = round(PowerDistance, 2)
  ) %>%
  select(Year, Hofstede_PDI, WVS_PC1_PDI)

# Print
print(comparison_tbl)

# Save as CSV
write.csv(comparison_tbl, "Figures/pdi_comparison.csv", row.names = FALSE)

# Export as DOCX table
doc <- read_docx() |>
  body_add_par("Table: Comparison of Hofstede PDI and WVS PC1 Scores", style = "heading 1") |>
  body_add_flextable(flextable(comparison_tbl) |> autofit())

print(doc, target = "Figures/pdi_comparison.docx")
message("📊 Table exported to Figures/pdi_comparison.csv and .docx")
