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
wvs_national <- read.csv("Data/wave_summary.csv")

wvs_national$Disbelief     <- as.numeric(wvs_national$Disbelief)
wvs_national$PowerDistance <- as.numeric(wvs_national$PowerDistance)

# ───────────────────────────────────────────────
# Define historical events (year + label)
# ───────────────────────────────────────────────
events <- data.frame(
  Year = c(1991, 1996, 1998, 2004, 2014, 2020),
  Label = c("Lib", "UF", "BJP", "UPA", "Modi", "COVID")
)

# Dynamic vertical position
dis_range <- range(wvs_national$Disbelief, na.rm = TRUE)
pd_range  <- range(wvs_national$PowerDistance, na.rm = TRUE)
events$y_dis <- dis_range[1] + 0.9 * diff(dis_range)
events$y_pd  <- pd_range[1]  + 0.85 * diff(pd_range)

# ───────────────────────────────────────────────
# Optional: Arrow trend line for Power Distance
# ───────────────────────────────────────────────
add_arrow <- TRUE  # 🔁 switch to FALSE if you don’t want the arrow

if (add_arrow) {
  arrow_df <- tibble::tibble(
    x = mean(c(1990, 2001)),
    xend = mean(c(2012, 2022)),
    y = mean(wvs_national$PowerDistance[wvs_national$Year %in% c(1990, 1995, 2001)], na.rm = TRUE),
    yend = mean(wvs_national$PowerDistance[wvs_national$Year %in% c(2012, 2022)], na.rm = TRUE)
  )
}

# ───────────────────────────────────────────────
# Plot: Disbelief Index over Time
# ───────────────────────────────────────────────
plot_disbelief <- ggplot(wvs_national, aes(x = Year, y = Disbelief)) +
  geom_line(color = "#1B4F72", linewidth = 1.3) +
  geom_point(color = "#1B4F72", size = 3) +
  geom_vline(data = events, aes(xintercept = Year), 
             linetype = "dotted", color = "gray50", linewidth = 0.5) +
  geom_text(data = events, aes(x = Year, y = y_dis, label = Label),
            color = "gray30", angle = 0, vjust = -0.7, hjust = 0.5,
            size = 3.4, fontface = "italic", inherit.aes = FALSE) +
  labs(
    title = "Disbelief Index Over Time (India)",
    x = "Year", y = "Disbelief (Composite Index)"
  ) +
  theme_classic(base_size = 14)

# ───────────────────────────────────────────────
# Plot: Power Distance Index (PC1)
# ───────────────────────────────────────────────
plot_pd <- ggplot(wvs_national, aes(x = Year, y = PowerDistance)) +
  geom_line(color = "#B03A2E", linewidth = 1.3) +
  geom_point(color = "#B03A2E", size = 3) +
  geom_vline(data = events, aes(xintercept = Year),
             linetype = "dotted", color = "gray50", linewidth = 0.5) +
  geom_text(data = events, aes(x = Year, y = y_pd, label = Label),
            color = "gray30", angle = 0, vjust = -0.7, hjust = 0.5,
            size = 3.4, fontface = "italic", inherit.aes = FALSE)

if (add_arrow) {
  plot_pd <- plot_pd + 
    geom_segment(data = arrow_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.25, "cm")),
                 color = "steelblue", linewidth = 1.2)
}

plot_pd <- plot_pd +
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

if (!dir.exists("Figures")) dir.create("Figures")

ggsave("Figures/disbelief_timeseries.png", plot_disbelief, width = 8, height = 5, dpi = 300)
ggsave("Figures/powerdistance_timeseries.png", plot_pd, width = 8, height = 5, dpi = 300)
ggsave("Figures/combined_timeseries.png", combined_plot, width = 8, height = 10, dpi = 300)

message("✅ Time-series plots with event labels saved in 'Figures/'")
