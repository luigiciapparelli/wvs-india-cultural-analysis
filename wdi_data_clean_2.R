wdi_clean <- read.csv("Data/WDI_India_Clean.csv", stringsAsFactors = FALSE)
names(wdi_clean)
new_names <- sapply(names(wdi_clean), function(x) {
  if (grepl("^X[0-9]{4}", x)) {             # Check if the name starts with "X" followed by 4 digits
    sub("^X([0-9]{4}).*", "\\1", x)           # Extract the 4-digit year and discard the rest
  } else {
    x                                        # Otherwise, leave the name unchanged
  }
})
names(wdi_clean) <- new_names
names(wdi_clean)
head(wdi_clean)
summary(wdi_clean$`2000`)
write.csv(wdi_clean, "Data/WDI_India_Clean.csv", row.names = FALSE)
