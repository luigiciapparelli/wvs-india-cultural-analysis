# Load necessary libraries
library(dplyr)
library(readr)

# Ensure `wvs_all` and `master_data` are loaded in environment
# Or source your data loading script

# Prepare summary table
wave_summary <- master_data %>%
  group_by(Year) %>%
  summarise(
    Disbelief_Mean = round(mean(Disbelief, na.rm = TRUE), 4),
    Disbelief_SD   = round(sd(Disbelief, na.rm = TRUE), 4),
    PowerDistance_Mean = round(mean(PowerDistance, na.rm = TRUE), 4),
    PowerDistance_SD   = round(sd(PowerDistance, na.rm = TRUE), 4),
    GNI_Mean       = round(mean(GNI_per_capita, na.rm = TRUE), 2),
    GNI_SD         = round(sd(GNI_per_capita, na.rm = TRUE), 2)
  ) %>%
  mutate(
    Disbelief = paste0(Disbelief_Mean, " ± ", Disbelief_SD),
    PowerDistance = paste0(PowerDistance_Mean, " ± ", PowerDistance_SD),
    GNI_per_capita = paste0(GNI_Mean, " ± ", GNI_SD)
  ) %>%
  select(Year, Disbelief, PowerDistance, GNI_per_capita)

# Save to CSV
write_csv(wave_summary, "Data/wave_summary.csv")

# Optional: print to viewer
print(wave_summary)

