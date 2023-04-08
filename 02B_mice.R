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
df_water_missing %>% head()
df_water_missing %>% sapply(function(x) sum(is.na(x)))

##### Implement MICE ###########################################################
# Imputations (m)
m <- 5

# Iterations (maxit)
i <- 5

# Predictor Matrix (predictorMatrix)
mice_predictors <- matrix(1, 17, 17)
mice_predictors[,1] <- mice_predictors[1,] <- 0

# Limits of Detection (lod -- passed to mice.impute.leftcenslognorm())
lod <- left_join(tibble(ELEMENT = colnames(df_water_missing)), water_llod, by = "ELEMENT")

# (Test Number of Limits of Detection)
if(length(lod %>% pull(ELEMENT)) != length(colnames(df_water_missing))) stop("check lod -- number")

# (Test Order of Limits of Detection)
if(sum(lod %>% pull(ELEMENT) != colnames(df_water_missing)) == length(lod %>% pull(ELEMENT))) stop("check lod -- order")

lod <- lod %>% pull(LLOD)

# Run MICE
impt <- mice(df_water_missing, m = m, maxit = i, method = "leftcenslognorm", 
  predictorMatrix = mice_predictors, lod = lod)

# Check Logged Events
impt$loggedEvents

##### Check Imputations ########################################################
df_water_missing %>% sapply(function(x) sum(is.na(x)))

check_impt(data_cons = df_water, data_mi = impt, element = Al)
check_impt(data_cons = df_water, data_mi = impt, element = As)
check_impt(data_cons = df_water, data_mi = impt, element = Br)
check_impt(data_cons = df_water, data_mi = impt, element = Fe)
check_impt(data_cons = df_water, data_mi = impt, element = Mo)
check_impt(data_cons = df_water, data_mi = impt, element = P)
check_impt(data_cons = df_water, data_mi = impt, element = S)
check_impt(data_cons = df_water, data_mi = impt, element = Sb)
check_impt(data_cons = df_water, data_mi = impt, element = U)
check_impt(data_cons = df_water, data_mi = impt, element = V)
check_impt(data_cons = df_water, data_mi = impt, element = W)





