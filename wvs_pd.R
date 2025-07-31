wvs_full <- read.csv("WVS_India.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
str(wvs_full)
names(wvs_full)
head(wvs_full)
grep("^Q27$", names(wvs_full), value = TRUE)
grep("^Q28$", names(wvs_full), value = TRUE)
grep("^Q29$", names(wvs_full), value = TRUE)
grep("^Q30$", names(wvs_full), value = TRUE)
grep("^Q31$", names(wvs_full), value = TRUE)
grep("^Q32$", names(wvs_full), value = TRUE)

power_vars <- grep("^Q(27|28|29|30|31|32)$", names(wvs_full), value = TRUE)

print(power_vars)
library(dplyr)
power_items <- wvs_full %>% select(all_of(power_vars))
head(power_items)
str(power_items)
power_items <- power_items %>% mutate(across(everything(), as.numeric))
summary(power_items)
power_items_complete <- na.omit(power_items)
power_pca <- prcomp(power_items_complete, center = TRUE, scale. = TRUE)
summary(power_pca)
wvs_full$PowerDistance_PC1 <- NA
complete_indices <- as.numeric(rownames(power_items_complete))
wvs_full$PowerDistance_PC1[complete_indices] <- power_pca$x[, 1]
write.csv(wvs_full, "WVS_India_powerdistance.csv", row.names = FALSE)
summary(wvs_full$PowerDistance_PC1)
complete_indices <- as.numeric(rownames(power_items_complete))
length(complete_indices) == length(power_pca$x[, 1])  # Should return TRUE
wvs_full$PowerDistance_PC1 <- NA
wvs_full$PowerDistance_PC1[complete_indices] <- power_pca$x[, 1]
summary(wvs_full$PowerDistance_PC1)
write.csv(wvs_full, "WVS_India_powerdistance.csv", row.names = FALSE)
wvs_check <- read.csv("WVS_India_powerdistance.csv", stringsAsFactors = FALSE)
summary(wvs_check$PowerDistance_PC1)
wvs_full$PowerDistance_PC1 <- power_pca$x[, 1]
write.csv(wvs_full[, c("Respondent_ID", "PowerDistance_PC1")], "WVS_India_powerdistance.csv", row.names = FALSE)
wvs_full$Respondent_ID <- 1:nrow(wvs_full)
write.csv(wvs_full[, c("Respondent_ID", "PowerDistance_PC1")], "WVS_India_powerdistance.csv", row.names = FALSE)
names(wvs_full)
write.csv(wvs_full["PowerDistance_PC1"], "WVS_India_powerdistance.csv", row.names = FALSE)
