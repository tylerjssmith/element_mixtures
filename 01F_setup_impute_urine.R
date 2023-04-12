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
# Match Drinking Water Elements
df_urine_missing_nosg <- df_urine_missing_nosg %>%
  select(colnames(df_water_missing %>% select(-U)), SPECIFICGRAVITY) %>%
  select(UID, SPECIFICGRAVITY, everything())

# Urine
df_urine_missing_nosg %>% head()
df_urine_missing_nosg %>% sapply(function(x) sum(is.na(x)))
df_urine_missing_nosg %>% ncol()

##### Multiple Imputation: Urine ###############################################
# Set Random Seed
set.seed(7023)

# Imputations (m)
m <- 5

# Iterations (maxit)
i <- 1

# Predictor Matrix (predictorMatrix)
# (Note: This ensures UID is not used for imputation.)
mice_predictors_urine <- matrix(1, 17, 17)
mice_predictors_urine[,1:2] <- mice_predictors_urine[1:2,] <- 0
diag(mice_predictors_urine) <- 0
mice_predictors_urine

# Limits of Detection (lod -- passed to mice.impute.leftcenslognorm())
urine_names <- tibble(ELEMENT = colnames(df_urine_missing_nosg))
urine_llod_mice <- left_join(urine_names, urine_llod, by = "ELEMENT")
urine_llod_mice

# (Test Number of Limits of Detection)
if(length(urine_llod_mice %>% pull(ELEMENT)) != length(colnames(df_urine_missing_nosg))) stop("check lod -- number")

# (Test Order of Limits of Detection)
if(sum(urine_llod_mice %>% pull(ELEMENT) != colnames(df_urine_missing_nosg)) == length(urine_llod_mice %>% pull(ELEMENT))) stop("check lod -- order")

# (Extract LLOD)
urine_llod_mice <- urine_llod_mice %>% pull(LLOD)
urine_llod_mice

##### Conduct Multiple Imputation ##############################################
impt_urine <- mice(df_urine_missing_nosg, m = m, maxit = i, method = "leftcenslognorm", 
  predictorMatrix = mice_predictors_urine, lod = urine_llod_mice)

# Check Logged Events
impt_urine$loggedEvents

# Extract Imputed Data Sets
df_urine_impt <- complete(impt_urine, action = "long")

##### Check Imputations ########################################################
df_urine_missing_nosg %>% sapply(function(x) sum(is.na(x)))

check_impt_dens(Fe, df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Iron")
check_impt_dens(Mn, df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Manganese")
check_impt_dens(Sb, df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Antimony")
check_impt_dens(V,  df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Vanadium")
check_impt_dens(W,  df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Tungsten")

check_impt_hist(Fe, df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Iron")
check_impt_hist(Mn, df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Manganese")
check_impt_hist(Sb, df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Antimony")
check_impt_hist(V,  df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Vanadium")
check_impt_hist(W,  df_mi = df_urine_impt, df_s2 = df_urine_nosg, df_llod = urine_llod, title = "Tungsten")

##### Apply Specific Gravity Correction to Imputations #########################
df_urine_impt_sg <- df_urine_impt %>%
  group_by(.imp) %>%
  mutate(across(-c(UID,SPECIFICGRAVITY), ~ .x * (mean(SPECIFICGRAVITY) - 1) / (SPECIFICGRAVITY - 1))) %>%
  ungroup()




