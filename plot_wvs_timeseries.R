# 1) Load needed packages
library(readr)
library(dplyr)
library(ggplot2)

# 2) Read in your full WVS data
#    adjust the path if needed
wvs <- read_csv("Data/wvs_all_data.csv", show_col_types = FALSE)

# 3) Inspect column names to find the exact PC1 names
colnames(wvs)[grepl("PowerDistance", colnames(wvs)) | grepl("Disbelief", colnames(wvs))]

# 4) Compute wave‐level averages
wvs_summary <- wvs %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(
    Disbelief       = mean(Disbelief, na.rm = TRUE),
    PowerDistancePC = mean(PowerDistance_PC1, na.rm = TRUE)  # replace with the exact name from step 3 if different
  ) %>%
  arrange(Year)

# 5) Plot
ggplot(wvs_summary, aes(x = Year)) +
  geom_line(aes(y = Disbelief,       color = "Disbelief"),   size = 1) +
  geom_point(aes(y = Disbelief,       color = "Disbelief"),   size = 2) +
  geom_line(aes(y = PowerDistancePC,  color = "Power Distance"), size = 1) +
  geom_point(aes(y = PowerDistancePC, color = "Power Distance"), size = 2) +
  scale_color_manual(
    name = "",
    values = c("Disbelief" = "steelblue", "Power Distance" = "firebrick")
  ) +
  labs(
    title    = "WVS Indices over Time",
    x        = "Survey Year",
    y        = "Index (PC1 score)",
    caption  = "Source: WVS India waves"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
