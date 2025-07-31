library(ggplot2)
library(ggrepel)
library(dplyr)

cor_val <- cor(master_data$Disbelief, master_data$PowerDistance, use = "complete.obs")
cor_label <- paste0("Pearson's r = ", round(cor_val, 2))

cor_plot <- ggplot(master_data, aes(x = Disbelief, y = PowerDistance)) +
  geom_point(color = "#2C3E50", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "#E74C3C", linetype = "dashed") +
  geom_text_repel(size = 4, box.padding = 0.5, max.overlaps = Inf) +
  geom_text(
    x = min(master_data$Disbelief, na.rm = TRUE),
    y = max(master_data$PowerDistance, na.rm = TRUE),
    label = cor_label,
    hjust = 0,
    size = 5,
    color = "steelblue"
  ) +
  labs(
    title = "Correlation Between Disbelief and Power Distance",
    x = "Disbelief (Composite Index)",
    y = "Power Distance (PC1 Score)"
  ) +
  theme_minimal(base_size = 14)

# Save the plot
ggsave("Figures/disbelief_vs_powerdistance.png", cor_plot, width = 8, height = 6, dpi = 300)


# Optional message
cat("Plot saved to Figures/disbelief_vs_powerdistance.png\n")
# Add Word export at end of correlation_plot.R

# Load Word tools
library(officer)
library(magrittr)

# Add plot to Word
doc_plot <- read_docx() %>%
  body_add_par("Scatterplot: Disbelief vs. Power Distance", style = "heading 1") %>%
  body_add_gg(cor_plot, width = 6.5, height = 5.5)

# Save
print(doc_plot, target = "Figures/disbelief_vs_powerdistance.docx")

cat("Word export saved to Figures/disbelief_vs_powerdistance.docx\n")

