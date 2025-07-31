# ───────────────────────────────────────────────
# Load libraries
# ───────────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(ggrepel)

# ───────────────────────────────────────────────
# Load data
# ───────────────────────────────────────────────
wvs_national <- read.csv("Data/wave_summary.csv")

# ───────────────────────────────────────────────
# Normalize Disbelief & PowerDistance (0–100 scale)
# ───────────────────────────────────────────────
normalize_100 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / diff(rng) * 100
}
wvs_national <- wvs_national %>%
  mutate(
    Disbelief_norm     = normalize_100(Disbelief),
    PowerDistance_norm = normalize_100(PowerDistance)
  )

# ───────────────────────────────────────────────
# Event abbreviations & y positions
# ───────────────────────────────────────────────
events <- data.frame(
  Year = c(1991, 1996, 1998, 2004, 2014, 2020),
  Label = c("Lib", "UF", "BJP", "UPA", "Modi", "COVID")
)
events$y_dis <- 95
events$y_pd  <- 90

# ───────────────────────────────────────────────
# Plot: Disbelief normalized
# ───────────────────────────────────────────────
plot_dis <- ggplot(wvs_national, aes(Year, Disbelief_norm)) +
  geom_line(color = "#1B4F72", linewidth = 1.2) +
  geom_point(size = 3, color = "#1B4F72") +
  geom_vline(data = events, aes(xintercept = Year), linetype = "dotted", color = "gray50") +
  geom_text(data = events, aes(x = Year, y = y_dis, label = Label),
            angle = 0, size = 4, color = "gray30", fontface = "bold", hjust = 0.5) +
  labs(
    title = "Disbelief (Normalized 0–100)",
    x = "Year",
    y = "Disbelief Score"
  ) +
  theme_minimal(base_size = 14)

# ───────────────────────────────────────────────
# Plot: Power Distance normalized + arrow
# ───────────────────────────────────────────────
# Freccia solo se entrambi gli anni sono presenti
y_1998 <- wvs_national$PowerDistance_norm[match(1998, wvs_national$Year)]
y_2020 <- wvs_national$PowerDistance_norm[match(2020, wvs_national$Year)]

if (!is.na(y_1998) && !is.na(y_2020)) {
  pd_arrow <- data.frame(
    x = 1998,
    xend = 2020,
    y = y_1998,
    yend = y_2020
  )
}


plot_pd <- ggplot(wvs_national, aes(Year, PowerDistance_norm)) +
  geom_line(color = "#B03A2E", linewidth = 1.2) +
  geom_point(size = 3, color = "#B03A2E") +
  geom_vline(data = events, aes(xintercept = Year), linetype = "dotted", color = "gray50") +
  geom_text(data = events, aes(x = Year, y = y_pd, label = Label),
            angle = 0, size = 4, color = "gray30", fontface = "bold", hjust = 0.5)

# ➕ Aggiungi la freccia solo se pd_arrow è stato creato
if (exists("pd_arrow")) {
  plot_pd <- plot_pd +
    geom_segment(data = pd_arrow, aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.25, "cm")), linewidth = 1, color = "darkred") +
    annotate("text", x = 2010, y = mean(c(pd_arrow$y, pd_arrow$yend), na.rm = TRUE) + 4,
             label = "Downward trend", color = "darkred", fontface = "italic", size = 4)
}

# Aggiungi etichette finali
plot_pd <- plot_pd +
  labs(
    title = "Power Distance (Normalized 0–100)",
    x = "Year",
    y = "Power Distance Score"
  ) +
  theme_minimal(base_size = 14)

# ───────────────────────────────────────────────
# Save plots
# ───────────────────────────────────────────────
if (!dir.exists("Figures")) dir.create("Figures")
ggsave("Figures/disbelief_timeseries_norm.png", plot_dis, width = 8, height = 5, dpi = 300)
ggsave("Figures/powerdistance_timeseries_norm.png", plot_pd, width = 8, height = 5, dpi = 300)

# Combine
combined <- plot_dis / plot_pd +
  plot_annotation(title = "WVS India: Normalized Cultural Indicators Over Time")

ggsave("Figures/combined_timeseries_norm.png", combined, width = 8, height = 10, dpi = 300)

message("✅ Normalized plots saved to Figures/")
