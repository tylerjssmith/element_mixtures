################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Multiple Imputation

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mice)
library(qgcomp)

##### Check Data ###############################################################
# Urine
df_urine_missing %>% head()
df_urine_missing %>% sapply(function(x) sum(is.na(x)))

##### Multiple Imputation: Urine ###############################################