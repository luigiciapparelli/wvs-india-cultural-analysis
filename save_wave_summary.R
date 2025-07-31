# ───────────────────────────────────────────────
# RIGENERA IL FILE wave_summary.csv
# ───────────────────────────────────────────────

library(dplyr)
library(readr)

# Assicurati che wvs_all sia già caricato nella tua sessione.
# Se non è presente, apri e riesegui lo script dove carichi i dati WVS e calcoli PowerDistance_PC1 e Disbelief.

# Crea tabella per ogni anno (Year), con media delle due variabili
wave_summary <- wvs_all %>%
  group_by(Year) %>%
  summarise(
    Disbelief = mean(Disbelief, na.rm = TRUE),
    PowerDistance = mean(PowerDistance_PC1, na.rm = TRUE)
  )

# Salva il risultato in CSV
write_csv(wave_summary, "Data/wave_summary.csv")
message("File salvato in: Data/wave_summary.csv")
