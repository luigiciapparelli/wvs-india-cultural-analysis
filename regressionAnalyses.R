## Completely clear out your workspace
rm(list = ls(all.names = TRUE))        # remove all user objects, including hidden ones
if (length(dev.list()) > 0) dev.off()  # close any open graphics devices
gc(verbose = FALSE)                    # run garbage collection

## Detach any non‐base packages that might be loaded
pkgs <- setdiff(    # find all non-base packages
  names(sessionInfo()$otherPkgs),
  "stats"          # leave base stats on for functions like `filter()`
)
for (p in pkgs) {
  try(detach(paste0("package:", p), character.only = TRUE, unload = TRUE), silent = TRUE)
}

## Optionally, restart your R session (in RStudio: Ctrl+Shift+F10)
## Set working directory "home/username/Documents/" where you placed Thesis_Analysis folder
setwd("/home/username/Documents/Thesis_Analysis")

# ─────────────────────────────────────────────────────────────────
# 0) Load packages
# ─────────────────────────────────────────────────────────────────
library(psych)    # for cronbach’s α
library(readxl)
library(dplyr)
library(tidyr)
library(plm)
library(lmtest)
library(sandwich)

# ─────────────────────────────────────────────────────────────────────────────
# 1) Load & standardize Year and Religion Items across WVS waves
# ─────────────────────────────────────────────────────────────────────────────
# 1a) Extend your read_wvs() to take a named vector of regex → new column names
read_wvs <- function(path, year_col_name, pd_patterns) {
  df <- read_excel(path, sheet = 1) %>%
    mutate(Year = as.numeric(.data[[year_col_name]])) %>%
    rename(
      I_RELIGIMP = `I_RELIGIMP: RELIGIMP - Welzel disbelief- 1: Inverse importance of religion`,
      I_RELIGBEL = `I_RELIGBEL: RELIGBEL - Welzel disbelief- 2: Inverse religious person`,
      I_RELIGPRAC = `I_RELIGPRAC: RELIGPRAC - Welzel disbelief- 3: Inverse religious practice`
    )
  # 1b) For each pattern → desired_name, find exactly one matching column and rename it
  for (pattern in names(pd_patterns)) {
    new_name <- pd_patterns[[pattern]]
    matches   <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    
    if (length(matches) == 1) {
      names(df)[names(df) == matches] <- new_name
    } else if (length(matches) == 0) {
      message(sprintf("!!!  No match in %s for pattern /%s/. Variable '%s' will be NA.", basename(path), pattern, new_name))
      df[[new_name]] <- NA  # Add NA column
    } else {
      message(sprintf("!!!  Multiple matches in %s for pattern /%s/:\n- %s\nPlease refine regex.", 
                      basename(path), pattern, paste(matches, collapse = "\n- ")))
      df[[new_name]] <- NA  # Fallback to NA to continue processing
    }
  }
  
  return(df)
}

# 1c) Define your eight PD regex patterns up front
pd_patterns <- c(
  "Future.*respect.*authority"     = "future_respect",
  "Confidence.*police"             = "conf_police",
  "Confidence.*government"         = "conf_gov",
  #(V88: Following instructions at work-->Deprecated since 2006)
  "Follow.*instruct|Obey.*instruct" = "follow_instr",
  "Confidence.*Political Parties"  = "conf_polpart",
  "Armed.*forces"                  = "conf_arm",
  "Major.*companies"               = "conf_majorcomp",
  "Income equality"                = "unequal_power"  
)

# 1d) Load each WVS wave through read_wvs()
wvs_1990 <- read_wvs("Data/WVS_India_1990.xlsx", "V377: Year survey", pd_patterns)
wvs_1995 <- read_wvs("Data/WVS_India_1995.xlsx", "V238: Year survey", pd_patterns)
wvs_2001 <- read_wvs("Data/WVS_India_2001.xlsx", "V246: Year survey", pd_patterns)
wvs_2006 <- read_wvs("Data/WVS_India_2006.xlsx", "V260: Year survey", pd_patterns)
wvs_2012 <- read_wvs("Data/WVS_India_2012.xlsx", "V262: Year survey", pd_patterns)
wvs_2022 <- read_wvs("Data/WVS_India_2022.xlsx", "A_YEAR: Year of survey", pd_patterns)

# ─────────────────────────────────────────────
# Fix 2012: Set 'unequal_power' manually
# ─────────────────────────────────────────────
# Inspect manually verified column
col_2012 <- "V96: Income equality"
if (col_2012 %in% names(wvs_2012)) {
  wvs_2012$unequal_power <- wvs_2012[[col_2012]]
} else {
  message("!!! V96 missing from 2012. unequal_power set to NA.")
  wvs_2012$unequal_power <- NA
}

# ─────────────────────────────────────────────
# Fix 2022: Set 'unequal_power' manually
# ─────────────────────────────────────────────
col_2022 <- "Q106: Incomes should be made more equal vs There should be greater incentives for individual effort"
if (col_2022 %in% names(wvs_2022)) {
  wvs_2022$unequal_power <- wvs_2022[[col_2022]]
} else {
  message("!!! Q106 missing from 2022. unequal_power set to NA.")
  wvs_2022$unequal_power <- NA
}

# ───────────────
# Manual hard-fix for follow_instr (Deprecated since 2006)
# eg. 2006
#col_follow_2006 <- "V150: Important child qualities: Obedience"
#if (col_follow_2006 %in% names(wvs_2006)) {
#  wvs_2006$follow_instr <- wvs_2006[[col_follow_2006]]
#} else {
#  message("!!! V150 missing from 2006. follow_instr set to NA.")
#  wvs_2006$follow_instr <- NA
#}
# ───────────────

# ─────────────────────────────────────────────────────────────────────────────
# 2) Bind all waves, recode, reverse-scale
# ─────────────────────────────────────────────────────────────────────────────
wvs_all <- bind_rows(wvs_1990, wvs_1995, wvs_2001, wvs_2006, wvs_2012, wvs_2022) %>%
  mutate(Year = if_else(Year == 2023, 2022, Year)) %>%
  mutate(across(
    c(future_respect, conf_police, conf_gov, follow_instr,
      conf_polpart, conf_arm, conf_majorcomp),
    ~ ifelse(. %in% -4:-1, NA_real_, as.numeric(.))
  )) %>%
  mutate(
    unequal_power = as.numeric(unequal_power),
    future_respect_rev  = max(future_respect, na.rm = TRUE) + 1 - future_respect,
    conf_police_rev     = max(conf_police,    na.rm = TRUE) + 1 - conf_police,
    conf_gov_rev        = max(conf_gov,       na.rm = TRUE) + 1 - conf_gov,
    #(V88: Following instructions at work-->Deprecated since 2006)
    follow_instr_rev    = max(follow_instr,   na.rm = TRUE) + 1 - follow_instr,
    conf_polpart_rev    = max(conf_polpart,   na.rm = TRUE) + 1 - conf_polpart,
    conf_arm_rev        = max(conf_arm,       na.rm = TRUE) + 1 - conf_arm,
    conf_majorcomp_rev  = max(conf_majorcomp, na.rm = TRUE) + 1 - conf_majorcomp,
    unequal_power_rev   = unequal_power
  )

 # 2b) NOW INSERT THE WVS “Disbelief” INDEX 

wvs_all <- wvs_all %>%
  # First compute the three WVS “inverse” scores exactly as in their manual
  mutate(
    i_religimp = case_when(
      I_RELIGIMP == 1 ~ 0,
      I_RELIGIMP == 2 ~ 0.33,
      I_RELIGIMP == 3 ~ 0.66,
      I_RELIGIMP == 4 ~ 1,
      TRUE            ~ NA_real_
    ),
    i_religbel = case_when(
      I_RELIGBEL == 1 ~ 0,
      I_RELIGBEL %in% c(2,3) ~ 1,
      TRUE             ~ NA_real_
    ),
    i_religprac = case_when(
      I_RELIGPRAC >= 1 & I_RELIGPRAC <= 4 ~ (I_RELIGPRAC - 1) / 6,
      I_RELIGPRAC == 5                    ~ 0.5,
      I_RELIGPRAC >= 6                    ~ (I_RELIGPRAC - 2) / 6,
      TRUE                                ~ NA_real_
    )
  ) %>%
  # 2c)Then build the composite “Disbelief” sub‐index
  rowwise() %>%
  mutate(
    Disbelief = case_when(
      !any(is.na(c(i_religimp, i_religbel, i_religprac))) ~
        mean(c(i_religimp, i_religbel, i_religprac)),
      is.na(i_religimp) & all(!is.na(c(i_religbel, i_religprac))) ~
        0.088 + 0.423 * i_religbel + 0.468 * i_religprac,
      is.na(i_religbel) & all(!is.na(c(i_religimp, i_religprac))) ~
        0.078 + 0.501 * i_religimp + 0.435 * i_religprac,
      is.na(i_religprac) & all(!is.na(c(i_religimp, i_religbel))) ~
        0.023 + 0.481 * i_religimp + 0.393 * i_religbel,
      TRUE ~ NA_real_
    ),
    Weight2a = if_else(
      !any(is.na(c(i_religimp, i_religbel, i_religprac))),
      1,
      0.66
    )
  ) %>%
  ungroup()
# Sanity check: Presence of Disbelief & Weight2a variables
stopifnot(all(c("Disbelief","Weight2a") %in% names(wvs_all)))
# Sanity check: Distribution:
table(is.na(wvs_all$Disbelief))
summary(wvs_all$Disbelief)
table(wvs_all$Weight2a)

# 1a) Do all eight "_rev" columns exist?
rev_cols <- c(
  "future_respect_rev","conf_police_rev","conf_gov_rev","follow_instr_rev",
  "conf_polpart_rev","conf_arm_rev","conf_majorcomp_rev","unequal_power_rev"
)
stopifnot(all(rev_cols %in% names(wvs_all)))

# 1b) How many non‐NA values in each?
sapply(rev_cols, function(col) sum(!is.na(wvs_all[[col]])))

  # 2d) Sanity check
  # Do these columns now exist?
  colnames(wvs_all)[grepl("_rev$", colnames(wvs_all))]
  # Do they have non‐NA values?
  sapply(paste0(c("future_respect","conf_police","conf_gov",
                  "follow_instr","conf_polpart","conf_arm",
                  "conf_majorcomp","unequal_power"), "_rev"),
         function(v) table(is.na(wvs_all[[v]])))
# ────────────────────────────────────────────────────────────────────────────
# 3) Compute final PC1s
# ────────────────────────────────────────────────────────────────────────────            
make_pc1 <- function(df, items, out_name) {
  mat <- df %>% select(all_of(items)) %>% mutate(across(everything(), as.numeric))
  idx <- complete.cases(mat)
  scores <- rep(NA, nrow(df))
  if (sum(idx) > 2) {
    scores[idx] <- prcomp(scale(mat[idx,]), center=FALSE, scale.=FALSE)$x[,1]
  }
  df[[out_name]] <- scores
  df
}
# pick your best PD subset from pca_exploration.R
best_pd_items <- c("conf_police_rev",
                   "conf_gov_rev",
                   "conf_polpart_rev")

wvs_all <- wvs_all %>%
  make_pc1(best_pd_items,                            "PowerDistance_PC1")
# ────────────────────────────────────────────────────────────────────────────
# 4) Summarize wave-level
# ────────────────────────────────────────────────────────────────────────────
wvs_national <- wvs_all %>%
  group_by(Year) %>%
  summarise(
    Disbelief = mean(Disbelief,   na.rm=TRUE),
    PowerDistance = mean(PowerDistance_PC1, na.rm=TRUE)
  )
# ────────────────────────────────────────────────────────────────────────────
# 5) Load & pivot WDI data
# ────────────────────────────────────────────────────────────────────────────
source("Scripts/wdi_data_clean.R")     # writes wdi_clean
source("Scripts/wdi_data_clean_2.R")   # reads it

econ_data <- wdi_clean %>%
  pivot_longer(starts_with("20"), names_to="Year", values_to="GNI_per_capita") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year %in% c(1995,2001,2006,2012,2022)) %>%
  bind_rows(data.frame(Country.Name="India", Country.Code="IND",
                       Year=1995, GNI_per_capita=354.32))
# ────────────────────────────────────────────────────────────────────────────
# 6) Build panel & run fixed-effects
# ────────────────────────────────────────────────────────────────────────────
master_data <- econ_data %>%
  left_join(wvs_national, by="Year")
# 1a) Show the first few rows
head(master_data)

# Sanity check: for each Year you have non‐missing PC scores
master_data %>%
  group_by(Year) %>%
  summarise(
    n_obs   = n(),
    n_PD     = sum(!is.na(PowerDistance)),
    n_Relig  = sum(!is.na(Disbelief)),
    mean_GNI = mean(GNI_per_capita, na.rm=TRUE)
  ) %>%
  print()
# ────────────────────────────────────────────────────────────────────────────
# 7) Preliminary diagnostics: stationarity tests (ADB, PP, KPSS)
# ────────────────────────────────────────────────────────────────────────────
library(urca)   # for ADF, PP, KPSS via ur.df(), ur.pp(), ur.kpss()

# a small helper that runs and prints all three on a vector
run_stationarity_tests <- function(x, name) {
  cat("\n==========", name, "==========\n")
  
  # 1) ADF (with trend) -> tau3
  adf <- ur.df(x, type = "trend", selectlags = "AIC")
  tau3  <- round(adf@teststat["tau3"], 3)
  c_adf <- round(adf@cval["tau3", ], 3)
  cat("ADF τ3   =", tau3, "\n")
  cat("   1pct/5pct/10pct CVs =", paste(c_adf, collapse = " / "), "\n\n")
  
  # 2) Phillips–Perron (trend) -> Z_tau
  pp <- ur.pp(x, type = "Z-tau", model = "trend", lags = "short")
  zt      <- round(pp@teststat, 3)
  c_pp    <- round(pp@cval, 3)
  cat("PP Zτ   =", zt, "\n")
  cat("   1pct/5pct/10pct CVs =", paste(c_pp, collapse = " / "), "\n\n")
  
  # 3) KPSS (trend) -> tau
  kpss <- ur.kpss(x, type = "tau", lags = "short")
  stat    <- round(kpss@teststat, 3)
  c_kpss  <- round(kpss@cval, 3)
  cat("KPSS τ  =", stat, "\n")
  cat("   10pct/5pct/2.5pct/1pct CVs =", paste(c_kpss, collapse = " / "), "\n\n")
}

# Build annual series as ts (frequency = 1)
years        <- sort(unique(master_data$Year))
gni_ts       <- ts(master_data$GNI_per_capita, start = min(years), frequency = 1)
pd_ts        <- ts(master_data$PowerDistance,      start = min(years), frequency = 1)
disbelief_ts <- ts(master_data$Disbelief,          start = min(years), frequency = 1)

# Run all tests
run_stationarity_tests(gni_ts,       "GNI per capita")
run_stationarity_tests(pd_ts,        "PowerDistance")
run_stationarity_tests(disbelief_ts, "Disbelief")

# ─────────────────────────────────────────────────────────────────────────────
# 8) First-Difference Regression
# ─────────────────────────────────────────────────────────────────────────────
diff_data <- master_data %>%
  arrange(Year) %>%
  mutate(
    d_GNI = c(NA, diff(GNI_per_capita)),
    d_PD  = c(NA, diff(PowerDistance)),
    d_Disbelief = c(NA, diff(Disbelief))
  ) %>%
  drop_na()

model_diff <- lm(d_PD ~ d_GNI + d_Disbelief, data = diff_data)
summary(model_diff)

# ─────────────────────────────────────────────────────────────────────────────
# 9) Panel FE model (robustness check)
# ─────────────────────────────────────────────────────────────────────────────
pdata <- pdata.frame(master_data, index = c("Country.Code", "Year"))

model_fe <- plm(PowerDistance ~ GNI_per_capita + Disbelief,
                data = pdata, model = "within")

summary(model_fe)
coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC1"))  # robust SEs

# ─────────────────────────────────────────────────────────────────────────────
# 10) Export to word table
# ─────────────────────────────────────────────────────────────────────────────
# install.packages("flextable") # if not done already

# First-Difference Regression Results
library(flextable)

# Create table for First-Diff
table_diff <- data.frame(
  Variable = c("Intercept", "Δ GNI per capita", "Δ Disbelief"),
  Estimate = coef(model_diff),
  `Std. Error` = summary(model_diff)$coefficients[, "Std. Error"],
  `p-value` = summary(model_diff)$coefficients[, "Pr(>|t|)"]
)

flextable(table_diff) %>%
  autofit() %>%
  set_caption("Table: First-Difference Regression Results")

# Fixed effects Results
library(lmtest)
library(sandwich)

robust_results <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC1"))

table_fe <- data.frame(
  Variable = rownames(robust_results),
  Estimate = robust_results[, "Estimate"],
  `Robust SE` = robust_results[, "Std. Error"],
  `p-value` = robust_results[, "Pr(>|t|)"]
)

flextable(table_fe) %>%
  autofit() %>%
  set_caption("Table: Panel Fixed-Effects Regression with Robust SEs")

write.csv(wvs_all, "Data/wvs_all_with_PC1.csv", row.names = FALSE)
# Save the merged dataset
write.csv(master_data, "Data/master_data.csv", row.names = FALSE)
