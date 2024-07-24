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
# Log-transform and Scale Variables
df_water_impt %>% head()
df_water_impt %>% dim()

df_water_impt_ln_z <- df_water_impt %>%
  select(-c(.imp,.id,UID)) %>%
  mutate(across(Al:W, ~ log(.x))) %>%
  mutate(across(Al:W, ~ scale(.x)))

df_water_impt_ln_z %>% head()

# Scree Plot and Parallel Analysis
fa.parallel(df_water_impt_ln_z, fa = "pc", nfactors = ncol(df_water_impt_ln_z), 
  n.iter = 200)

# Principal Components Analysis
(pca_fit_water <- principal(df_water_impt_ln_z, 
  nfactors = ncol(df_water_impt_ln_z), rotate = "none"))

# Extract Loadings
# (Get Loadings for Principal Components with Eigenvalues >1)
n_pc_water <- sum(pca_fit_water$values > 1)
pca_loadings_water <- unclass(pca_fit_water$loadings)[,1:n_pc_water]

# (Convert Loadings to Tibble)
pca_loadings_water <- pca_loadings_water %>%
  as_tibble(rownames = "Element")

##### Urinary Elements #########################################################
# Log-transform and Scale Variables
df_urine_impt %>% head()
df_urine_impt %>% dim()

df_urine_impt_ln_z <- df_urine_impt %>%
  tibble() %>%
  select(-c(.imp,.id,UID,SPECIFICGRAVITY)) %>%
  mutate(across(Al:Zn, ~ log(.x))) %>%
  mutate(across(Al:Zn, ~ scale(.x)))

df_urine_impt_ln_z %>% head()

# Scree Plot and Parallel Analysis
fa.parallel(df_urine_impt_ln_z, fa = "pc", nfactors = ncol(df_urine_impt_ln_z), 
  n.iter = 200)

# Principal Components Analysis
(pca_fit_urine <- principal(df_urine_impt_ln_z, 
  nfactors = ncol(df_urine_impt_ln_z), rotate = "none"))

# Extract Loadings
# (Get Loadings for Principal Components with Eigenvalues >1)
n_pc_urine <- sum(pca_fit_urine$values > 1)
pca_loadings_urine <- unclass(pca_fit_urine$loadings)[,1:n_pc_urine]

# (Convert Loadings to Tibble)
pca_loadings_urine <- pca_loadings_urine %>%
  as_tibble(rownames = "Element")
pca_loadings_urine %>% head()

# (Reverse Sign on PC5)
# (Notes: In PCA, the solution is unique up to a sign flip. Here, the signs
#  on PC5 loadings and scores were reversed so that As, Mo, and W load
#  positively, which improves interpretability.)
pca_loadings_urine <- pca_loadings_urine %>%
  mutate(PC5 = PC5 * -1)



