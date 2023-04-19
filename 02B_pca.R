################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Principal Components Analysis

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(psych)

##### Drinking Water Elements ##################################################
# Log-transform and Scale Variables
df_water_impt %>% head()

df_water_impt_ln_z <- df_water_impt %>%
  group_by(.imp) %>%
  mutate(across(Al:W, ~ log(.x))) %>%
  mutate(across(Al:W, ~ scale(.x))) %>%
  ungroup()

df_water_impt_ln_z %>% head()

# Calculate Mean Correlation Matrix
corr_mat_water <- list()

for(i in 1:m) {
  data <- df_water_impt_ln_z %>% 
    filter(.imp == i) %>% 
    select(Al:W)
  corr_mat_water[[i]] <- cor(data, method = "pearson")
}

corr_mat_water_mean <- Reduce("+", corr_mat_water) / m

corr_mat_water_mean %>% head()

# Generate Scree Plot
scree(corr_mat_water_mean)

# Run PCA
(pca_fit_water <- principal(corr_mat_water_mean, 
  nfactors = ncol(corr_mat_water_mean), rotate = "none"))

##### Urinary Elements #########################################################
# Log-transform and Scale Variables
df_urine_impt %>% head()

df_urine_impt_ln_z <- df_urine_impt %>%
  tibble() %>%
  group_by(.imp) %>%
  mutate(across(Al:Zn, ~ log(.x))) %>%
  mutate(across(Al:Zn, ~ scale(.x))) %>%
  ungroup()

df_urine_impt_ln_z %>% head()

# Calculate Mean Correlation Matrix
corr_mat_urine <- list()

for(i in 1:m) {
  data <- df_urine_impt_ln_z %>% 
    filter(.imp == i) %>% 
    select(Al:Zn)
  corr_mat_urine[[i]] <- cor(data, method = "pearson")
}

corr_mat_urine_mean <- Reduce("+", corr_mat_urine) / m

corr_mat_urine_mean %>% head()

# Generate Scree Plot
scree(corr_mat_urine_mean)

# Run PCA
(pca_fit_urine <- principal(corr_mat_urine_mean, 
  nfactors = ncol(corr_mat_urine_mean), rotate = "none"))

