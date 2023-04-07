################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Load Functions

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Load Functions ###########################################################
# Function: Calculate p-value for Spearman's Correlation
cor_pval <- function(x, y, ...) {
  suppressWarnings(fit <- cor.test(x, y, ...))
  out <- fit$p.value
  return(out)
}