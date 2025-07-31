# ─────────────────────────────────────────────
# plot_pdi_comparison_with_events.R
# ─────────────────────────────────────────────

# 1) Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# 2) Read wave-level summary
wvs <- read_csv("Data/wave_summary.csv", show_col_types = FALSE) %>%
  rename(PC1 = PowerDistance) %>%
  select(Year, PC1)

# 3) Hofstede benchmark
hofstede_pdi <- 77

# 4) Rescale PC1 to [30–100]
anchor_min <- 30; anchor_max <- 100
pc1_range  <- range(wvs$PC1, na.rm = TRUE)
wvs <- wvs %>%
  mutate(PC1_scaled = anchor_min +
           (PC1 - pc1_range[1]) / diff(pc1_range) * (anchor_max - anchor_min)
  )

# 5) Define events & abbreviazioni (come per gli altri grafici)
events <- data.frame(
  Year = c(1991, 1996, 1998, 2004, 2014, 2020),
  Abbrev = c("Lib", "UF", "BJP", "UPA", "Modi", "COVID")
) %>%
  # calcola y posizione dinamica appena sopra il range di punti
  mutate(
    y = anchor_max + 2
  )

# 6) Costruisci il plot con eventi
p <- ggplot(wvs, aes(x = Year)) +
  geom_line(aes(y = PC1_scaled), color = "#B03A2E", linewidth = 1.2) +
  geom_point(aes(y = PC1_scaled), color = "#B03A2E", size = 3) +
  
  # benchmark Hofstede
  geom_hline(yintercept = hofstede_pdi, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  annotate("text",
           x     = max(wvs$Year),
           y     = hofstede_pdi + 3,
           label = "Hofstede PDI = 77",
           hjust = 1,
           color = "gray40",
           size = 4,
           fontface = "italic"
  ) +
  
  # linee verticali eventi + abbreviazioni
  geom_vline(data = events, aes(xintercept = Year),
             linetype = "dotted", color = "gray50", linewidth = 0.6) +
  geom_text(data = events,
            aes(x = Year, y = y, label = Abbrev),
            color = "gray20", size = 4, fontface = "bold",
            vjust = 0, angle = 0, inherit.aes = FALSE
  ) +
  
  labs(
    title    = "Power Distance: WVS PC1 vs Hofstede Benchmark",
    subtitle = "WVS PC1 rescaled a [30–100] con eventi annotati",
    x        = "Anno",
    y        = "Power Distance (Scala comparabile 0–100)"
  ) +
  theme_minimal(base_size = 14)

# 7) Export
if (!dir.exists("Figures")) dir.create("Figures")
ggsave("Figures/pdi_comparison_rescaled_events.png", p, width = 8, height = 5, dpi = 300)

message("Plot with eventi salvato in: Figures/pdi_comparison_rescaled_events.png")
