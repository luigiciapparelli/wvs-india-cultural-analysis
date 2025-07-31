# ───────────────────────────────────────────────────────────────
# 1) Compute wave‐level means & SDs
# ───────────────────────────────────────────────────────────────
library(dplyr)
library(kableExtra)

summary_table <- wvs_all %>%
  filter(Year %in% c(1990,1995,2001,2006,2012,2022)) %>%
  group_by(Year) %>%
  summarise(
    Disbelief_Mean          = mean(Disbelief,           na.rm = TRUE),
    Disbelief_SD            = sd(Disbelief,             na.rm = TRUE),
    PowerDistance_Mean      = mean(PowerDistance_PC1,   na.rm = TRUE),
    PowerDistance_SD        = sd(PowerDistance_PC1,     na.rm = TRUE),
    n                       = n()
  ) %>%
  ungroup()

# ───────────────────────────────────────────────────────────────
# 2) Render as a publication‑quality table
# ───────────────────────────────────────────────────────────────
summary_table %>%
  mutate(
    Disbelief = sprintf("%.3f (±%.3f)", Disbelief_Mean, Disbelief_SD),
    PowerDistance = sprintf("%.3f (±%.3f)", PowerDistance_Mean, PowerDistance_SD)
  ) %>%
  select(Year, Disbelief, PowerDistance, n) %>%
  kbl(
    caption = "Wave‑level summary of Disbelief and Power Distance in India (WVS 1990–2022)",
    col.names = c("Year","Disbelief\n(Mean ± SD)","Power Distance\n(Mean ± SD)","N"),
    booktabs = TRUE,
    align = c("c","r","r","c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position","striped"),
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")
