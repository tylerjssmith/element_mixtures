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
  select(all_of(1:n_pc_water))

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

##### Urinary Elements #########################################################
# Extract and Join Scores
pca_scores_urine <- pca_fit_urine$scores %>% 
  as_tibble() %>% 
  select(all_of(1:n_pc_urine))

pca_scores_urine %>% head()
pca_scores_urine %>% dim()

df_covar_scores_urine <- cbind(df_covar, pca_scores_urine) %>%
  as_tibble()

df_covar_scores_urine <- df_covar_scores_urine %>%
  select(-c(UID,AGE,LSI,SEBMI,medSEMUAC,wAs,uAs,wAs10,uAsB))

# (Reverse Sign on PC5)
# (Notes: In PCA, the solution is unique up to a sign flip. Here, the signs
#  on PC5 loadings and scores were reversed so that As, Mo, and W load
#  positively, which improves interpretability.)
df_covar_scores_urine <- df_covar_scores_urine %>%
  mutate(PC5 = PC5 * -1)

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

