# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Load your cleaned dataset (adjust path if needed)
wvs_all <- read.csv("/home/username/Documents/Thesis_Analysis/Data/wvs_all_with_PC1.csv")

# Step 1: Select and summarize data
summary_df <- wvs_all %>%
  select(Year, Disbelief, PowerDistance_PC1) %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(
    Disbelief = mean(Disbelief, na.rm = TRUE),
    PowerDistance_PC1 = mean(PowerDistance_PC1, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Disbelief, PowerDistance_PC1),
               names_to = "Index", values_to = "Value")

# Step 2: Plot both indices in one figure
plot <- ggplot(summary_df, aes(x = Year, y = Value, color = Index)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Disbelief" = "#1f77b4", "PowerDistance_PC1" = "#ff7f0e")) +
  labs(
    title = "Trends in Disbelief and Power Distance (India, WVS)",
    x = "Survey Year", y = "Index Value", color = "Construct"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top")

# Step 3: Save as high-resolution image in /Data
ggsave("/home/username/Documents/Thesis_Analysis/Data/combined_wvs_trends.png", plot, dpi = 300, width = 10, height = 6)

# Also display the plot inside RStudio
print(plot)
