################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Principal Components Analysis

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(psych)

##### Drinking Water Elements ##################################################
# Extract and Join Scores
pca_scores_water <- pca_fit_water$scores %>% 
  as_tibble() %>% 
  select(PC1:PC4)

pca_scores_water %>% head()
pca_scores_water %>% dim()

df_covar_scores_water <- cbind(df_covar, pca_scores_water) %>%
  as_tibble()

df_covar_scores_water <- df_covar_scores_water %>%
  select(-c(UID,AGE,LSI,SEBMI,medSEMUAC,wAs,uAs,wAs10,uAsB))

df_covar_scores_water %>% head()
df_covar_scores_water %>% dim()

# Fit Models
df_tbl3 <- rbind(
  df_covar_scores_water %>% lm_pca_scores(y = "PC1"),
  df_covar_scores_water %>% lm_pca_scores(y = "PC2"),
  df_covar_scores_water %>% lm_pca_scores(y = "PC3"),
  df_covar_scores_water %>% lm_pca_scores(y = "PC4")
)

# Generate Table
tbl3 <- df_tbl3 %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ round(.x, 2))) %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ format(.x, nsmall = 2))) %>%
  mutate(p.value = ifelse(p.value >= 0.01, round(p.value, 2), ifelse(p.value < 0.01 & p.value >= 0.001, "<0.01", ifelse(p.value < 0.001, "<0.001", p.value)))) %>%
  mutate(est = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  pivot_wider(id_cols = c(x,term), names_from = y, values_from = c(est,p.value)) %>%
  mutate(across(est_PC1:p.value_PC4, ~ ifelse(term == "(Intercept)", "Reference", .x))) %>%
  select(x, term, ends_with("PC1"), ends_with("PC2"), ends_with("PC3"), ends_with("PC4"))

tbl3 %>% head()

##### Urinary Elements #########################################################
# Extract and Join Scores
pca_scores_urine <- pca_fit_urine$scores %>% 
  as_tibble() %>% 
  select(PC1:PC7)

pca_scores_urine %>% head()
pca_scores_urine %>% dim()

df_covar_scores_urine <- cbind(df_covar, pca_scores_urine) %>%
  as_tibble()

df_covar_scores_urine <- df_covar_scores_urine %>%
  select(-c(UID,AGE,LSI,SEBMI,medSEMUAC,wAs,uAs,wAs10,uAsB))

df_covar_scores_urine %>% head()
df_covar_scores_urine %>% dim()

# Fit Models
df_tbl4 <- rbind(
  df_covar_scores_urine %>% lm_pca_scores(y = "PC1", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC2", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC3", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC4", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC5", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC6", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC7", ymax = PC7)
)

# Generate Table
tbl4 <- df_tbl4 %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ round(.x, 2))) %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ format(.x, nsmall = 2))) %>%
  mutate(p.value = ifelse(p.value >= 0.01, round(p.value, 2), ifelse(p.value < 0.01 & p.value >= 0.001, "<0.01", ifelse(p.value < 0.001, "<0.001", p.value)))) %>%
  mutate(est = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  pivot_wider(id_cols = c(x,term), names_from = y, values_from = c(est,p.value)) %>%
  mutate(across(est_PC1:p.value_PC7, ~ ifelse(term == "(Intercept)", "Reference", .x))) %>%
  select(x, term, ends_with("PC1"), ends_with("PC2"), ends_with("PC3"), ends_with("PC4"), ends_with("PC5"), ends_with("PC6"), ends_with("PC7"))

tbl4 %>% head()
