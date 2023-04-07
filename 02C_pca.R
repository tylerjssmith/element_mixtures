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
# Calculate Mean Correlation Matrix
corr_mat <- list()

for(i in 1:5) {
  data <- complete(impt, action = i)
  data <- data %>% select(-UID)
  corr_mat[[i]] <- cor(data, method = "pearson")
}

corr_mat_mean <- Reduce("+", corr_mat) / 5

# Generate Scree Plot
scree(corr_mat_mean)

# Run PCA -
(pca_fit_full <- principal(corr_mat_mean, nfactors = 16, rotate = "none"))
(pca_fit4 <- principal(corr_mat_mean, nfactors = 4, rotate = "none"))



