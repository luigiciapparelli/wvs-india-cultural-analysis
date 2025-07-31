install.packages(c("tidyr", "dplyr", "readxl"))
library(tidyr)
library(dplyr)
library(readxl)
wdi_clean <- read.csv("WDI_India_Clean.csv", header = TRUE, stringsAsFactors = FALSE)
head(wdi_clean)
wdi_long <- wdi_clean %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year_raw",
    values_to = "GNI_per_capita"
  ) %>%
  mutate(
    # Remove everything after the year number; assume the column names start with "X" followed by the year.
    Year = as.numeric(sub("\\..*", "", Year_raw))
  ) %>%
  select(-Year_raw)  # Drop the temporary variable
head(wdi_long)
summary(wdi_long$Year)
library(tidyr)
library(dplyr)

wdi_long <- wdi_clean %>%
  pivot_longer(
    cols = matches("^X[0-9]{4}"),
    names_to = "Year_raw",
    values_to = "GNI_per_capita"
  ) %>%
  mutate(
    # Extract the four-digit year that immediately follows "X"
    Year = as.numeric(sub("^X([0-9]{4}).*", "\\1", Year_raw))
  ) %>%
  select(-Year_raw)
head(wdi_long)
summary(wdi_long$Year)
# Check WDI data (already in long format)
str(wdi_long$Year)
summary(wdi_long$Year)

library(readxl)

mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 2)

mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 2)

names(mospi_data)
library(dplyr)
mospi_data <- mospi_data %>% rename(Year = `...1`)
mospi_data <- mospi_data %>% rename(Per_Capita_Income = `...4`)
mospi_data <- mospi_data %>% select(-`...3`)
names(mospi_data)
mospi_data <- mospi_data %>% select(-`प्रचलित भावों  पर तथा स्थिर  भावों पर   (At Current Prices and at constant Prices)`)
names(mospi_data)
summary(mospi_data$Year)
mospi_data$Year <- as.numeric(mospi_data$Year)
unique(mospi_data$Year)
View(mospi_data)
library(readxl)
library(dplyr)
library(tidyr)

# Read the file and skip the first two rows so that row 3 becomes the header
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 2)
names(mospi_data)
library(readxl)
library(dplyr)

# Determine the correct header row by inspecting the Excel file
# For example, if the actual headers are on the 3rd row, skip the first 2 rows
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 2)

# View the first few rows to confirm correct import
head(mospi_data)

# Rename columns for easier reference if necessary
# For example, if the third column is 'Per Capita Income'
colnames(mospi_data)[3] <- "Per_Capita_Income"

# Convert 'Year' column to numeric, handling non-numeric entries
mospi_data <- mospi_data %>%
  mutate(Year = as.numeric(Year))

# Check for NA values introduced during conversion
summary(mospi_data$Year)

# If there are NA values, inspect the original 'Year' column to identify non-numeric entries
unique(mospi_data$Year)
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 2)
# Read the header row separately
header <- read_excel("mospi_per_capita.xlsx", skip = 2, n_max = 1, col_names = FALSE)

# Read the data starting from the row after the header
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 3, col_names = header)
library(readxl)

# Read the header row separately
header <- read_excel("mospi_per_capita.xlsx", skip = 2, n_max = 1, col_names = FALSE)

# Extract the column names as a character vector
col_names <- as.character(unlist(header[1, ]))

# Read the data starting from the row after the header
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 3, col_names = col_names)

# View the first few rows to confirm correct import
head(mospi_data)
library(readxl)

# Step 1: Read the header row (3rd row) to get column names
header <- read_excel("mospi_per_capita.xlsx", skip = 2, n_max = 1, col_names = FALSE)

# Extract the column names as a character vector
col_names <- as.character(unlist(header))

# Step 2: Read the data starting from the row after the header (4th row)
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 3, col_names = col_names)

# View the first few rows to confirm correct import
head(mospi_data)
library(readxl)
library(dplyr)
library(tidyr)  # For data reshaping
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 2, col_names = TRUE)
# Example: Rename columns based on your Excel structure
colnames(mospi_data) <- c(
  "S.No", 
  "Category", 
  "2011-12", "2012-13", "2013-14", "2014-15", 
  "2015-16", "2016-17", "2017-18", "2018-19", 
  "2019-20", "2020-21", "2021-22", "2022-23"
)
mospi_data <- mospi_data %>%
  filter(!is.na(S.No) & S.No != "NA")  # Example condition
mospi_data_long <- mospi_data %>%
  pivot_longer(
    cols = `2011-12`:`2022-23`,  # Replace with your year columns
    names_to = "Year",
    values_to = "Per_Capita_Income"
  )
mospi_data_long <- mospi_data_long %>%
  mutate(
    Year = as.numeric(sub("-.*", "", Year))  # Extract the start year
  )
library(readxl)
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 5)
names(mospi_data)
pcgdp_current <- mospi_data[4, ]
population <- mospi_data[2, ]
pcgdp_constant <- mospi_data[10, ]
library(tidyr)
mospi_long <- mospi_data %>%
  pivot_longer(
    cols = 3:14,
    names_to = "Year_Range",
    values_to = "Per_Capita_GDP_Current"
  )

library(readxl)
mospi_data <- read_excel("mospi_per_capita.xlsx", skip = 5)
names(mospi_data)
library(dplyr)
library(tidyr)

mospi_long <- mospi_data %>%
  pivot_longer(
    cols = 3:14,                  # Select columns 3 to 14
    names_to = "Year_Range",      # New column to store the original column names (year ranges)
    values_to = "Per_Capita_GDP_Current"  # New column for the corresponding values
    head(mospi_long)
  )
mospi_long <- mospi_data %>%
  pivot_longer(cols = 3:14, names_to = "Year_Range", values_to = "Per_Capita_GDP_Current")
head(mospi_long)
mospi_long <- mospi_long %>%
  mutate(Year = as.numeric(sub("-.*", "", Year_Range)))
head(mospi_long)
summary(mospi_long$Year)
mospi_long <- mospi_long %>% select(-Year_Range)
names(mospi_long)
write.csv(mospi_long, "mospi_data_long.csv", row.names = FALSE)
mospi_long <- read.csv("mospi_data_long.csv", stringsAsFactors = FALSE)
head(mospi_long)
names(mospi_long)
str(mospi_long)
unique(mospi_long$Year)
mospi_long$Year <- as.numeric(sub("-.*", "", mospi_long$Year_Range))
str(mospi_long$Year)
summary(mospi_long$Year)
names(mospi_long)
str(mospi_long)
library(readxl)
library(dplyr)
library(tidyr)
mospi_all <- read_excel("mospi_per_capita.xlsx", col_names = FALSE)
View(mospi_all)
years_row <- mospi_all[6, ]  # Row 6
pop_row <- mospi_all[8, ]    # Row 8
gdp_row <- mospi_all[10, ]   # Row 10
years_vec <- as.character(unlist(years_row[1, 3:14]))
pop_vec   <- as.numeric(unlist(pop_row[1, 3:14]))
gdp_vec   <- as.numeric(unlist(gdp_row[1, 3:14]))
years_vec
pop_vec
gdp_vec
# Create a vector of year labels from row 6, columns 3 to 14
years_vec <- as.character(unlist(years_row[1, 3:14]))

# Create a vector of population values from row 8, columns 3 to 14
pop_vec <- as.numeric(unlist(pop_row[1, 3:14]))

# Create a vector of GDP current per capita values from row 10, columns 3 to 14
gdp_vec <- as.numeric(unlist(gdp_row[1, 3:14]))
# Check the year labels
print(years_vec)

# Check the population values
print(pop_vec)

# Check the GDP values
print(gdp_vec)
Year <- as.numeric(sub("-.*", "", years_vec))
print(Year)
mospi_final <- data.frame(
  Year = Year,
  Population = pop_vec,
  GDP_Per_Capita_Current = gdp_vec,
  stringsAsFactors = FALSE
)
head(mospi_final)
summary(mospi_final)
unique(mospi_final$Year)
write.csv(mospi_final, "mospi_data_final.csv", row.names = FALSE)

