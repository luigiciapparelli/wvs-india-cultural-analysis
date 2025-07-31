wdi_data <- read.csv("Data/WDI_India_2023.csv", header = TRUE, stringsAsFactors = FALSE)
head(wdi_data)
# Remove rows where Country.Code is empty or contains non-data text
wdi_data_clean <- subset(wdi_data, Country.Code != "" & !grepl("Data from", Country.Name))
head(wdi_data_clean)
# Identify year columns by pattern (assuming they all start with "X" and end with the year)
year_cols <- grep("^X[0-9]{4}", names(wdi_data_clean), value = TRUE)
# Convert these columns to numeric
wdi_data_clean[year_cols] <- lapply(wdi_data_clean[year_cols], as.numeric)
str(wdi_data_clean)
write.csv(wdi_data_clean, "Data/WDI_India_Clean.csv", row.names = FALSE)
