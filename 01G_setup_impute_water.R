################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Multiple Imputation -- Water

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mice)
library(qgcomp)

##### Check Data ###############################################################
df_water_miss %>% head()
df_water_miss %>% sapply(function(x) sum(is.na(x)))
df_water_miss %>% dim()

##### Set and Check Parameters #################################################
# Set Random Seed
set.seed(7023)

# Imputations (m)
m <- 25

# Predictor Matrix (predictorMatrix)
# (Note: This ensures UID is not used for imputation.)
tmp <- df_water_miss %>% ncol()
mice_predictors_water <- matrix(1, nrow = tmp, ncol = tmp)
mice_predictors_water[,1] <- mice_predictors_water[1,] <- 0
diag(mice_predictors_water) <- 0
mice_predictors_water
rm(tmp)

# Limits of Detection (lod -- passed to mice.impute.leftcenslognorm())
water_names <- tibble(ELEMENT = colnames(df_water_miss))
water_llod_mice <- left_join(water_names, df_water_llod_val, by = "ELEMENT")
water_llod_mice

# (Test Number of Limits of Detection)
length_water_mice <- length(water_llod_mice %>% pull(ELEMENT))
length_water_miss <- length(colnames(df_water_miss))

if(length_water_mice != length_water_miss) stop("check lod -- number")

rm(length_water_mice)
rm(length_water_miss)

# (Test Order of Limits of Detection)
order_water_mice <- sum(water_llod_mice %>% pull(ELEMENT) != colnames(df_water_miss))
order_water_miss <- length(water_llod_mice %>% pull(ELEMENT))

if(order_water_mice == order_water_miss) stop("check lod -- order")

rm(order_water_mice)
rm(order_water_miss)

# (Extract LLOD)
(water_llod_mice <- water_llod_mice %>% pull(LLOD))

##### Conduct Multiple Imputation ##############################################
# Impute Data
impt_water <- mice(df_water_miss, m = m, method = "leftcenslognorm", 
  predictorMatrix = mice_predictors_water, lod = water_llod_mice)

# Check Logged Events
impt_water$loggedEvents

# Extract Imputed Data Sets
df_water_impt <- complete(impt_water, action = "long") %>%
  tibble()

df_water_impt %>% head()









