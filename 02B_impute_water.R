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
# Drinking Water
df_water_missing %>% head()
df_water_missing %>% sapply(function(x) sum(is.na(x)))

##### Multiple Imputation: Drinking Water ######################################
# Set Random Seed
set.seed(7023)

# Imputations (m)
m <- 5

# Iterations (maxit)
i <- 1

# Predictor Matrix (predictorMatrix)
# (Note: This ensures UID is not used for imputation.)
mice_predictors_water <- matrix(1, 17, 17)
mice_predictors_water[,1] <- mice_predictors_water[1,] <- 0
mice_predictors_water

# Limits of Detection (lod -- passed to mice.impute.leftcenslognorm())
water_names <- tibble(ELEMENT = colnames(df_water_missing))
water_llod_mice <- left_join(water_names, water_llod, by = "ELEMENT")
water_llod_mice

# (Test Number of Limits of Detection)
if(length(water_llod_mice %>% pull(ELEMENT)) != length(colnames(df_water_missing))) stop("check lod -- number")

# (Test Order of Limits of Detection)
if(sum(water_llod_mice %>% pull(ELEMENT) != colnames(df_water_missing)) == length(water_llod_mice %>% pull(ELEMENT))) stop("check lod -- order")

# (Extract LLOD)
water_llod_mice <- water_llod_mice %>% pull(LLOD)
water_llod_mice

##### Conduct Multiple Imputation ##############################################
impt_water <- mice(df_water_missing, m = m, maxit = i, method = "leftcenslognorm", 
  predictorMatrix = mice_predictors_water, lod = water_llod_mice)

# Check Logged Events
impt_water$loggedEvents

# Extract Imputed Data Sets
df_water_impt <- complete(impt_water, action = "long")

##### Check Imputations ########################################################
df_water_missing %>% sapply(function(x) round(sum(is.na(x)) / 780 * 100, 1))

# Aluminum
check_impt_dens(Al, title = "Aluminum")
check_impt_hist(Al, title = "Aluminum")

# Bromine
check_impt_dens(Br, title = "Bromine")
check_impt_hist(Br, title = "Bromine")

# Sulphur
check_impt_dens(S, title = "Sulphur")
check_impt_hist(S, title = "Sulphur")

# Antimony
check_impt_dens(Sb, title = "Antimony")
check_impt_hist(Sb, title = "Antimony")

# Uranium
check_impt_dens(U, title = "Uranium")
check_impt_hist(U, title = "Uranium")

# Tungsten
check_impt_dens(W, title = "Tungsten")
check_impt_hist(W, title = "Tungsten")










