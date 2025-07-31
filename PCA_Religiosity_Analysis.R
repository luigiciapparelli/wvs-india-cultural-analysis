wvs_data <- read.csv("WVS_Wave_.csv", header = TRUE, stringsAsFactors = FALSE)
head(wvs_data)
names(wvs_data)
religion_cols <- grep("RELIG", names(wvs_data), ignore.case = TRUE, value = TRUE)
print(religion_cols)
religion_cols <- grep("^I_RELIG", names(wvs_data), ignore.case = TRUE, value = TRUE)
print(religion_cols)
length(names(wvs_data))
wvs_data <- read.csv("WVS_India.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
length(names(wvs_data))
head(wvs_data)
# Search for any column names containing "RELIG" (case-insensitive)
religion_cols <- grep("RELIG", names(wvs_data), ignore.case = TRUE, value = TRUE)
print(religion_cols)
wvs_data_filtered <- wvs_data[, religion_cols]
head(wvs_data_filtered)
write.csv(wvs_data_filtered, "WVS_India_Filtered.csv", row.names = FALSE)
summary(wvs_data_filtered)
hist(wvs_data_filtered$I_RELIGIMP, main = "Distribution of I_RELIGIMP", xlab = "I_RELIGIMP")
hist(wvs_data_filtered$I_RELIGBEL, main = "Distribution of I_RELIGBEL", xlab = "I_RELIGBEL")
hist(wvs_data_filtered$I_RELIGPRAC, main = "Distribution of I_RELIGPRAC", xlab = "I_RELIGPRAC")
wvs_data_filtered$Religiosity_Index <- rowMeans(wvs_data_filtered[, c("I_RELIGIMP", "I_RELIGBEL", "I_RELIGPRAC")], na.rm = TRUE)
head(wvs_data_filtered$Religiosity_Index)
summary(wvs_data_filtered$Religiosity_Index)
hist(wvs_data_filtered$Religiosity_Index, main = "Distribution of Religiosity Index", xlab = "Composite Religiosity", col = "lightblue")
hist(wvs_data_filtered$Religiosity_Index,
     main = "Distribution of Religiosity Index",
     xlab = "Composite Religiosity (Mean of I_RELIGIMP, I_RELIGBEL, I_RELIGPRAC)",
     ylab = "Frequency",
     col = "grey",         # Changed from lightblue to navy
     border = "black",
     cex.main = 1.5,       # Increase title size
     cex.lab = 1.3)        # Increase axis label size
wvs_relig <- wvs_data_filtered[, c("I_RELIGIMP", "I_RELIGBEL", "I_RELIGPRAC")]
wvs_relig_complete <- na.omit(wvs_relig)
wvs_relig_complete <- wvs_relig_complete[!apply(wvs_relig_complete, 1, function(x) any(is.infinite(x))), ]
religion_pca <- prcomp(wvs_relig_complete, center = TRUE, scale. = TRUE)
summary(religion_pca)
wvs_data_filtered$Religiosity_PC1 <- NA
wvs_data_filtered$Religiosity_PC1[as.numeric(rownames(wvs_relig_complete))] <- religion_pca$x[, 1]
summary(wvs_data_filtered$Religiosity_PC1)
cor_I_RELIGIMP <- cor(wvs_data_filtered$Religiosity_PC1, wvs_data_filtered$I_RELIGIMP, use = "complete.obs")
cor_I_RELIGBEL <- cor(wvs_data_filtered$Religiosity_PC1, wvs_data_filtered$I_RELIGBEL, use = "complete.obs")
cor_I_RELIGPRAC <- cor(wvs_data_filtered$Religiosity_PC1, wvs_data_filtered$I_RELIGPRAC, use = "complete.obs")
cor_I_RELIGIMP
cor_I_RELIGBEL
cor_I_RELIGPRAC
save.image("workspace.RData")
wvs_data_filtered$Religiosity_PC1 <- -wvs_data_filtered$Religiosity_PC1
cor(wvs_data_filtered$Religiosity_PC1, wvs_data_filtered$I_RELIGIMP, use = "complete.obs")
cor(wvs_data_filtered$Religiosity_PC1, wvs_data_filtered$I_RELIGBEL, use = "complete.obs")
cor(wvs_data_filtered$Religiosity_PC1, wvs_data_filtered$I_RELIGPRAC, use = "complete.obs")
cor_I_RELIGIMP
cor_I_RELIGBEL
cor_I_RELIGPRAC
save.image("workspace.RData")
wvs_data_filtered$Religiosity_PC1 <- -wvs_data_filtered$Religiosity_PC1
summary(wvs_data_filtered$Religiosity_PC1)
hist(wvs_data_filtered$Religiosity_PC1,
     main = "Distribution of Religiosity PC1 Scores",
     xlab = "Composite Religiosity (PC1 Score)",
     ylab = "Frequency",
     col = "grey",         # A formal, thesis-friendly color
     border = "black",
     cex.main = 1.5,       # Larger title text for clarity
     cex.lab = 1.3)        # Larger axis labels
