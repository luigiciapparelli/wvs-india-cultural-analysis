# pca_exploration.R

library(psych)
library(dplyr)
library(readr)

# 1) Candidate PD vars (pooled across waves)
cand <- c(
  "future_respect_rev","conf_police_rev","conf_gov_rev",
  "conf_polpart_rev","conf_arm_rev","follow_instr_rev",
  "conf_majorcomp_rev","unequal_power_rev"
)

# 2) Set up results container with all three columns
results <- tibble(
  vars  = character(),
  pve   = numeric(),
  alpha = numeric()
)

# 3) Loop through combinations 3–length(cand)
for (k in 3:length(cand)) {
  for (v in combn(cand, k, simplify = FALSE)) {
    mat <- na.omit(select(wvs_all, all_of(v)))
    if (nrow(mat) > 5) {
      # PC1 variance explained
      pve_val   <- summary(prcomp(mat, scale.=TRUE))$importance[2,1]
      # Cronbach's alpha (auto‐flip keys)
      alpha_val <- psych::alpha(mat, check.keys=TRUE)$total$raw_alpha
      
      results <- add_row(
        results,
        vars  = paste(v, collapse = ","), 
        pve   = pve_val,                       
        alpha = alpha_val
      )
      
      
    }
  }
}

# 4) Sort and write out variance and Cronbach’s α
full_results <- arrange(results, desc(pve), desc(alpha))
readr::write_csv(full_results, "Data/pca_variance_alpha.csv")

# 5) Quick preview 
head(full_results, 10)