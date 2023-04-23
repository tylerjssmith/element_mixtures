################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S4

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Loadings
tmp1 <- pca_loadings_water %>%
  mutate(across(PC1:PC4, ~ round(.x, 2)))

# Eigenvalue, Proportion of Variance, Cumulative Proportion
tmp2 <- round(pca_fit_water$values[1:n_pc_water], 2)
tmp3 <- round(pca_fit_water$values[1:n_pc_water] / sum(pca_fit_water$values) * 100, 2)
tmp4 <- round(cumsum(tmp3), 2)

tmp2 <- c("Eigenvalue", tmp2)
tmp3 <- c("Proportion of Variance (%)", tmp3)
tmp4 <- c("Cumulative Proportion (%)", tmp4)

# Combine Rows
tblS4 <- rbind(
  tmp1,
  tmp2,
  tmp3,
  tmp4
)

tblS4 %>% head()
tblS4 %>% tail()

# Remove Intermediate Objects
rm(list = c("tmp1","tmp2","tmp3","tmp4"))

