library(psych)
library(dplyr)

# Candidate religion items (including services & prayer)
relig_cand <- c(
  "I_RELIGIMP",   # inverse importance of religion
  "I_RELIGBEL",   # inverse religious person
  "I_RELIGPRAC",  # inverse religious practice
  "V139",         # attendance at services
  "V140"          # frequency of prayer
)

# Prepare results container
relig_results <- tibble(
  vars  = character(),
  pve   = numeric(),
  alpha = numeric()
)

# Loop over all subsets of size ≥ 3
for (k in 3:length(relig_cand)) {
  for (subset in combn(relig_cand, k, simplify = FALSE)) {
    mat <- wvs_all %>% 
      select(all_of(subset)) %>% 
      mutate(across(everything(), as.numeric)) %>% 
      na.omit()
    
    if (nrow(mat) > 5) {
      pve_val   <- summary(prcomp(mat, scale.=TRUE))$importance[2,1]
      alpha_val <- psych::alpha(mat, check.keys=TRUE)$total$raw_alpha
      
      relig_results <- relig_results %>% 
        add_row(
          vars  = paste(subset, collapse = ","),
          pve   = pve_val,
          alpha = alpha_val
        )
    }
  }
}

# Sort by variance explained then α
relig_results <- relig_results %>%
  arrange(desc(pve), desc(alpha))

# Write out
readr::write_csv(relig_results, "Data/relig_pca_variance_alpha.csv")

# Print top 5 to console
print(head(relig_results, 5))
