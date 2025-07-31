# ───────────────────────────────────────────────
# Load required libraries
# ───────────────────────────────────────────────
library(ggplot2)
library(ggrepel)
library(dplyr)
library(patchwork)

# ───────────────────────────────────────────────
# Load your processed wave-level data
# ───────────────────────────────────────────────
if (!file.exists("Data/wave_summary.csv")) {
  stop("❌ File 'Data/wave_summary.csv' not found. Run the main analysis first.")
}
wvs_national <- read.csv("Data/wave_summary.csv")

# Convert columns safely
wvs_national$Disbelief     <- as.numeric(wvs_national$Disbelief)
wvs_national$PowerDistance <- as.numeric(wvs_national$PowerDistance)

# ───────────────────────────────────────────────
# Define historical events (year + label)
# ───────────────────────────────────────────────
events <- data.frame(
  Year = c(1991, 1996, 1998, 2004, 2014, 2020),
  Label = c(
    "Liberalization",
    "Election: United Front",
    "BJP Govt",
    "Congress UPA Begins",
    "Modi Govt",
    "COVID-19"
  ),
  Abbrev = c("Lib", "UF", "BJP", "UPA", "Modi", "COVID")
)

# Compute vertical positions
dis_range <- range(wvs_national$Disbelief, na.rm = TRUE)
pd_range  <- range(wvs_national$PowerDistance, na.rm = TRUE)
events$y_dis <- dis_range[1] + 0.8 * diff(dis_range)
events$y_pd  <- pd_range[1]  + 0.85 * diff(pd_range)

# ───────────────────────────────────────────────
# Plot: Disbelief Index over Time (with short labels)
# ───────────────────────────────────────────────
plot_disbelief <- ggplot(wvs_national, aes(x = Year, y = Disbelief)) +
  geom_line(color = "#1B4F72", linewidth = 1.3) +
  geom_point(color = "#1B4F72", size = 3) +
  geom_vline(data = events, aes(xintercept = Year),
             linetype = "dotted", color = "gray50", linewidth = 0.5) +
  geom_text_repel(data = events,
                  aes(x = Year, y = y_dis, label = Abbrev),
                  size = 4, color = "gray30", fontface = "italic",
                  nudge_y = 0.02, segment.alpha = 0.5,
                  inherit.aes = FALSE) +
  labs(
    title = "Disbelief Index Over Time (India)",
    x = "Year", y = "Disbelief (Composite Index)"
  ) +
  theme_classic(base_size = 14)

# ───────────────────────────────────────────────
# Plot: Power Distance Index (PC1, full labels)
# ───────────────────────────────────────────────
plot_pd <- ggplot(wvs_national, aes(x = Year, y = PowerDistance)) +
  geom_line(color = "#B03A2E", linewidth = 1.3) +
  geom_point(color = "#B03A2E", size = 3) +
  geom_vline(data = events, aes(xintercept = Year),
             linetype = "dotted", color = "gray50", linewidth = 0.5) +
  geom_text(data = events, aes(x = Year, y = y_pd, label = Label),
            color = "gray30", angle = 90, vjust = -0.3, hjust = 0,
            size = 3.2, fontface = "italic", inherit.aes = FALSE) +
  labs(
    title = "Power Distance (PC1) Over Time (India)",
    x = "Year", y = "Power Distance (PC1 Score)"
  ) +
  theme_classic(base_size = 14)

# ───────────────────────────────────────────────
# Combine and Export
# ───────────────────────────────────────────────
combined_plot <- plot_disbelief / plot_pd +
  plot_annotation(title = "WVS India: Cultural Indicators Over Time")

# Ensure Figures/ exists
if (!dir.exists("Figures")) dir.create("Figures")

ggsave("Figures/disbelief_timeseries.png", plot_disbelief, width = 8, height = 5, dpi = 300)
ggsave("Figures/powerdistance_timeseries.png", plot_pd, width = 8, height = 5, dpi = 300)
ggsave("Figures/combined_timeseries.png", combined_plot, width = 8, height = 10, dpi = 300)

message("✅ Time-series plots successfully saved to 'Figures/'")
