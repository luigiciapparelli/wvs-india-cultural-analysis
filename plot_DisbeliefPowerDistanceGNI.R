library(ggplot2)
library(dplyr)
library(tidyr)
setwd("/home/username/Documents/Thesis_Analysis")

# Load merged dataset with GNI and indices
master_data <- read.csv("/home/username/Documents/Thesis_Analysis/Data/master_data.csv")  # This is your panel with Year, Disbelief, PowerDistance, GNI

# Reshape for plotting
plot_data <- master_data %>%
  select(Year, Disbelief, PowerDistance, GNI_per_capita) %>%
  pivot_longer(cols = c("Disbelief", "PowerDistance"), names_to = "Index", values_to = "Value")

# Scale GNI per capita to overlay (dividing by 10,000 for visual comparison — adjust as needed)
gni_scaled <- master_data %>%
  mutate(Value = GNI_per_capita / 10000, Index = "GNI_per_capita_scaled") %>%
  select(Year, Index, Value)

# Combine all for plotting
plot_combined <- bind_rows(plot_data, gni_scaled)

# Plot all three
plot <- ggplot(plot_combined, aes(x = Year, y = Value, color = Index)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Disbelief" = "#1f77b4",
    "PowerDistance" = "#ff7f0e",
    "GNI_per_capita_scaled" = "#2ca02c"
  )) +
  labs(
    title = "Disbelief, Power Distance, and GNI per Capita in India (1995–2022)",
    x = "Year", y = "Scaled Index Value",
    color = "Index"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top")

# Save plot
ggsave("Data/combined_with_gni.png", plot, dpi = 300, width = 10, height = 6)

# Also display
print(plot)
