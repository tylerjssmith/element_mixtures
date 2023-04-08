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
df_water_impt_ln_z <- complete(impt, action = "long") %>%
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
(pca_fit <- principal(corr_mat_mean, nfactors = 16, rotate = "none"))



