################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Principal Components Analysis

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(psych)

##### Calculate Correlation Matrices ###########################################
# Log-transform and Scale Variables
df_water_impt_ln_z <- impt_combined %>%
  tibble() %>%
  group_by(.imp) %>%
  mutate(across(Al:W, ~ log(.x))) %>%
  mutate(across(Al:W, ~ scale(.x))) %>%
  ungroup()

df_water_impt_ln_z %>% head()

# Calculate Mean Correlation Matrix
corr_mat <- list()

for(i in 1:m) {
  data <- df_water_impt_ln_z %>% select(Al:W)
  corr_mat[[i]] <- cor(data, method = "pearson")
}

corr_mat_mean <- Reduce("+", corr_mat) / m

corr_mat_mean %>% head()

# Generate Scree Plot
scree(corr_mat_mean)

# Run PCA
(pca_fit <- principal(corr_mat_mean, nfactors = ncol(corr_mat_mean), rotate = "none"))

##### Sensitivity Analysis #####################################################
# Log-transform and Scale Variables
df_water_impt_ln_z_no_outliers <- impt_combined_no_outliers %>%
  tibble() %>%
  group_by(.imp) %>%
  mutate(across(Al:W, ~ log(.x))) %>%
  mutate(across(Al:W, ~ scale(.x))) %>%
  ungroup()

df_water_impt_ln_z_no_outliers %>% head()

# Calculate Mean Correlation Matrix
corr_mat_no_outliers <- list()

for(i in 1:m) {
  data <- df_water_impt_ln_z_no_outliers %>% select(Al:W)
  corr_mat_no_outliers[[i]] <- cor(data, method = "pearson")
}

corr_mat_mean_no_outliers <- Reduce("+", corr_mat_no_outliers) / m

corr_mat_mean_no_outliers %>% head()

# Generate Scree Plot
scree(corr_mat_mean_no_outliers)

# Run PCA
(pca_fit_no_outliers <- principal(corr_mat_mean_no_outliers, nfactors = ncol(corr_mat_mean_no_outliers), rotate = "none"))

# Compare Results
round(unclass(pca_fit_no_outliers$loadings)[,1] - unclass(pca_fit$loadings)[,1], 2)

cor(df_water_impt_ln_z_no_outliers %>% filter(.imp == 1) %>% pull(As), df_water_impt_ln_z_no_outliers %>% filter(.imp == 1) %>% pull(Mn))
cor(df_water_impt_ln_z %>% filter(.imp == 1) %>% pull(As), df_water_impt_ln_z %>% filter(.imp == 1) %>% pull(Mn))



