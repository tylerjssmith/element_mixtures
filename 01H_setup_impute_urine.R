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
# Arrange Columns
df_urine_miss_nosg <- df_urine_miss_nosg %>%
  select(UID, SPECIFICGRAVITY, everything())

# Urine
df_urine_miss_nosg %>% head()
df_urine_miss_nosg %>% sapply(function(x) sum(is.na(x)))
df_urine_miss_nosg %>% dim()

##### Multiple Imputation: Urine ###############################################
# Set Random Seed
set.seed(7023)

# Imputations (m)
m <- 1

# Iterations (maxit)
it <- 100

# Predictor Matrix (predictorMatrix)
# (Note: This ensures UID and SPECIFICGRAVITY are not used for imputation.)
tmp <- df_urine_miss_nosg %>% ncol()
mice_predictors_urine <- matrix(1, tmp, tmp)
mice_predictors_urine[,1:2] <- mice_predictors_urine[1:2,] <- 0
diag(mice_predictors_urine) <- 0
mice_predictors_urine
rm(tmp)

# Limits of Detection (lod -- passed to mice.impute.leftcenslognorm())
urine_names <- tibble(ELEMENT = colnames(df_urine_miss_nosg))
urine_llod_mice <- left_join(urine_names, df_urine_llod_val, by = "ELEMENT")
urine_llod_mice

# (Test Number of Limits of Detection)
length_urine_mice <- length(urine_llod_mice %>% pull(ELEMENT))
length_urine_miss <- length(colnames(df_urine_miss_nosg))

if(length_urine_mice != length_urine_miss) stop("check lod -- number")

rm(length_urine_mice)
rm(length_urine_miss)

# (Test Order of Limits of Detection)
order_urine_mice <- sum(urine_llod_mice %>% pull(ELEMENT) != colnames(df_urine_miss_nosg))
order_urine_miss <- length(urine_llod_mice %>% pull(ELEMENT))

if(order_urine_mice == order_urine_miss) stop("check lod -- order")

rm(order_urine_mice)
rm(order_urine_miss)

# (Extract LLOD)
urine_llod_mice <- urine_llod_mice %>% pull(LLOD)
urine_llod_mice

##### Conduct Multiple Imputation ##############################################
impt_urine_nosg <- mice(df_urine_miss_nosg, m = m, maxit = it, method = "leftcenslognorm", 
  predictorMatrix = mice_predictors_urine, lod = urine_llod_mice)

# Check Logged Events
impt_urine_nosg$loggedEvents

# Extract Imputed Data Sets
df_urine_impt_nosg <- complete(impt_urine_nosg, action = "long") %>% 
  tibble()
df_urine_impt_nosg %>% head()
df_urine_impt_nosg %>% dim()

##### Apply Specific Gravity Correction to Urinary Elements ####################
# LLOD/√2: Wide
df_urine_sqt2_nosg %>% head()

df_urine_sqt2 <- df_urine_sqt2_nosg %>%
  mutate(across(-c(UID,SPECIFICGRAVITY,As), ~ .x * (mean(SPECIFICGRAVITY) - 1) / 
      (SPECIFICGRAVITY - 1)))

# Multiple Imputation: Wide
df_urine_impt_nosg %>% head()

df_urine_impt <- df_urine_impt_nosg %>%
  mutate(across(-c(.imp,.id,UID,SPECIFICGRAVITY,As), ~ .x * (mean(SPECIFICGRAVITY) - 1) / 
      (SPECIFICGRAVITY - 1)))
