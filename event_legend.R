# ─────────────────────────────────────────────
# Librerie necessarie
# ─────────────────────────────────────────────
library(officer)
library(flextable)

# ─────────────────────────────────────────────
# Tabella leggenda abbreviata
# ─────────────────────────────────────────────
legend_tbl <- data.frame(
  Abbreviation = c("Lib", "UF", "BJP", "UPA", "Modi", "COVID"),
  Description  = c(
    "1991 Economic Liberalization",
    "1996 Election: United Front",
    "1998 BJP Government Begins",
    "2004 Congress UPA Begins",
    "2014 Modi Government Begins",
    "2020 COVID-19 Pandemic"
  )
)

# ─────────────────────────────────────────────
# Crea tabella orizzontale e salva in Word
# ─────────────────────────────────────────────
ft <- flextable(legend_tbl) |>
  set_header_labels(
    Abbreviation = "Code",
    Description = "Event Description"
  ) |>
  autofit() |>
  theme_booktabs()

# Aggiungi titolo e salva in .docx
doc <- read_docx() |>
  body_add_par("Legend: Annotated Events in Time-Series Plots", style = "heading 1") |>
  body_add_flextable(ft)

print(doc, target = "Figures/event_legend.docx")
message("✅ Legend table saved to Figures/event_legend.docx")

