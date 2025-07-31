# ───────────────────────────────────────
# Load libraries
# ───────────────────────────────────────
library(ggplot2); library(dplyr); library(readr)

# ───────────────────────────────────────
# Load microdata con religione
# ───────────────────────────────────────
# Assicurati di avere wvs_all dal tuo script principale
wvs_all <- read_rds("Data/wvs_all_processed.rds")  # o usa read_csv

# ───────────────────────────────────────
# Controlla variabile religione (ad es. I_RELIGBEL)
# ───────────────────────────────────────
wvs_all <- wvs_all %>%
  filter(!is.na(I_RELIGBEL)) %>%
  mutate(ReligionBel = factor(I_RELIGBEL,
                              levels = c(1,2),
                              labels = c("Believer", "Non-believer")))

# ───────────────────────────────────────
# Frequenze per anno
# ───────────────────────────────────────
religion_dist <- wvs_all %>%
  group_by(Year, ReligionBel) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(p = n / sum(n) * 100)

# ───────────────────────────────────────
# Plot stacked bar per onda
# ───────────────────────────────────────
p <- ggplot(religion_dist, aes(x = factor(Year), y = p, fill = ReligionBel)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set2", name = "Religious Belief") +
  labs(
    title = "Religious Belief Distribution per WVS Wave (India)",
    x = "Year",
    y = "% of respondents"
  ) +
  theme_minimal(base_size = 14)

# ───────────────────────────────────────
# Export
# ───────────────────────────────────────
if(!dir.exists("Figures")) dir.create("Figures")
ggsave("Figures/religion_distribution.png", p, width = 8, height = 5, dpi = 300)
message("Religion distribution plot saved to Figures/")
