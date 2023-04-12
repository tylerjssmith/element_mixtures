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
df_urine_miss <- df_urine_miss %>%
  select(colnames(df_water_miss %>% select(-U)), SPECIFICGRAVITY) %>%
  select(UID, SPECIFICGRAVITY, everything())

# Urine
df_urine_miss %>% head()
df_urine_miss %>% sapply(function(x) sum(is.na(x)))
df_urine_miss %>% dim()

##### Multiple Imputation: Urine ###############################################
# Set Random Seed
set.seed(7023)

# Imputations (m)
m <- 25

# Predictor Matrix (predictorMatrix)
# (Note: This ensures UID is not used for imputation.)
tmp <- df_urine_miss %>% ncol()
mice_predictors_urine <- matrix(1, tmp, tmp)
mice_predictors_urine[,1:2] <- mice_predictors_urine[1:2,] <- 0
diag(mice_predictors_urine) <- 0
mice_predictors_urine
rm(tmp)

# Limits of Detection (lod -- passed to mice.impute.leftcenslognorm())
urine_names <- tibble(ELEMENT = colnames(df_urine_miss))
urine_llod_mice <- left_join(urine_names, df_urine_llod_val, by = "ELEMENT")
urine_llod_mice

# (Test Number of Limits of Detection)
length_urine_mice <- length(urine_llod_mice %>% pull(ELEMENT))
length_urine_miss <- length(colnames(df_urine_miss))

if(length_urine_mice != length_urine_miss) stop("check lod -- number")

rm(length_urine_mice)
rm(length_urine_miss)

# (Test Order of Limits of Detection)
order_urine_mice <- sum(urine_llod_mice %>% pull(ELEMENT) != colnames(df_urine_miss))
order_urine_miss <- length(urine_llod_mice %>% pull(ELEMENT))

if(order_urine_mice == order_urine_miss) stop("check lod -- order")

rm(order_urine_mice)
rm(order_urine_miss)

# (Extract LLOD)
urine_llod_mice <- urine_llod_mice %>% pull(LLOD)
urine_llod_mice

##### Conduct Multiple Imputation ##############################################
impt_urine <- mice(df_urine_miss, m = m, method = "leftcenslognorm", 
  predictorMatrix = mice_predictors_urine, lod = urine_llod_mice)

# Check Logged Events
impt_urine$loggedEvents

# Extract Imputed Data Sets
df_urine_impt_nosg <- complete(impt_urine, action = "long") %>% tibble()

##### Check Imputations ########################################################
df_urine_miss %>% sapply(function(x) sum(is.na(x)))

check_impt_dens(Fe, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Iron")
check_impt_dens(Mn, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Manganese")
check_impt_dens(Sb, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Antimony")
check_impt_dens(V,  df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Vanadium")
check_impt_dens(W,  df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Tungsten")

check_impt_hist(Fe, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Iron")
check_impt_hist(Mn, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Manganese")
check_impt_hist(Sb, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Antimony")
check_impt_hist(V,  df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Vanadium")
check_impt_hist(W,  df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2, df_llod = df_urine_llod_val, title = "Tungsten")

##### Apply Specific Gravity Correction to Urinary Elements ####################
# LLOD/√2: Wide
df_urine_sqt2 <- df_urine_sqt2 %>%
  mutate(across(-c(UID,SPECIFICGRAVITY,As), ~ .x * (mean(SPECIFICGRAVITY) - 1) / (SPECIFICGRAVITY - 1)))

# LLOD/√2: Long
df_urine_sqt2_long <- df_urine_sqt2_long %>%
  group_by(Element) %>%
  mutate(Urine = ifelse(Element != "As", Urine * (mean(SPECIFICGRAVITY) - 1) / (SPECIFICGRAVITY - 1), Urine)) %>%
  ungroup()

# Multiple Imputation: Wide
df_urine_impt <- df_urine_impt_nosg %>%
  group_by(.imp) %>%
  mutate(across(-c(UID,SPECIFICGRAVITY,As), ~ .x * (mean(SPECIFICGRAVITY) - 1) / (SPECIFICGRAVITY - 1))) %>%
  ungroup()
