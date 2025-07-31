# build_PD_index.R
# Description: Construct and validate PD Index from WVS data

# ─────────────────────────────────────────────────────────────────────────────
# 0) Setup
# ─────────────────────────────────────────────────────────────────────────────
setwd("~/Documents/Thesis_Analysis")  # adjust if needed

library(dplyr)
library(psych)
library(ggplot2)
library(knitr)
library(kableExtra)
library(readr)

# ─────────────────────────────────────────────────────────────────────────────
# 1) Load data
# ─────────────────────────────────────────────────────────────────────────────
wvs_all <- read_csv("Data/wvs_all_data.csv")

# ─────────────────────────────────────────────────────────────────────────────
# 2) Create PD Index: Best PCA combo
# ─────────────────────────────────────────────────────────────────────────────
wvs_all <- wvs_all %>%
  mutate(PD_Index = rowMeans(select(., conf_police_rev, conf_gov_rev, conf_polpart_rev), na.rm = TRUE))

# ─────────────────────────────────────────────────────────────────────────────
# 3) Scree plot of selected PCA
# ─────────────────────────────────────────────────────────────────────────────
pca_data <- na.omit(select(wvs_all, conf_police_rev, conf_gov_rev, conf_polpart_rev))
pca_model <- prcomp(pca_data, scale. = TRUE)

scree_df <- data.frame(
  PC = paste0("PC", 1:length(pca_model$sdev)),
  Variance = (pca_model$sdev)^2 / sum(pca_model$sdev^2)
)

ggplot(scree_df, aes(x = PC, y = Variance)) +
  geom_col(fill = "#2c7fb8") +
  geom_line(aes(group = 1), color = "black", linetype = "dashed") +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Scree Plot: PCA on Confidence in Institutions",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal(base_size = 13)

ggsave("PD_scree_plot.png", width = 6, height = 4)

# ─────────────────────────────────────────────────────────────────────────────
# 4) Summary statistics
# ─────────────────────────────────────────────────────────────────────────────
summary(wvs_all$PD_Index)
hist(wvs_all$PD_Index, main = "Distribution of PD_Index", col = "#3182bd", xlab = "PD Index (avg of 3 items)")

# ─────────────────────────────────────────────────────────────────────────────
# 5) Save styled table
# ─────────────────────────────────────────────────────────────────────────────
full_results <- read_csv("Data/pca_variance_alpha.csv")

final_pca <- full_results %>%
  filter(vars == "conf_police_rev,conf_gov_rev,conf_polpart_rev")

kbl(final_pca,
    caption = "Best Principal Component: PD Index (3-item)",
    col.names = c("Variables", "Variance Explained (PVE)", "Cronbach’s Alpha"),
    digits = 4,
    format = "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE, width = "20em") %>%
  column_spec(2:3, width = "10em") %>%
  save_kable("PD_Index_Table.html")
# ─────────────────────────────────────────────────────────────────────────────
# 6) Add table export to Word
# ─────────────────────────────────────────────────────────────────────────────
library(officer)
library(flextable)

top_table <- readr::read_csv("Data/pca_variance_alpha.csv") %>%
  arrange(desc(pve), desc(alpha)) %>%
  head(10) %>%
  flextable() %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Top PCA Combinations", style = "heading 1") %>%
  body_add_flextable(top_table)

print(doc, target = "Top_PCA_Table.docx")
