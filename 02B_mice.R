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

##### Set Values <LLOD to Missing ##############################################
tmp1 <- df_water %>%
  rename_with(~ gsub("PE_wMetals_", "", .x)) %>%
  pivot_longer(-UID, names_to = "ELEMENT", values_to = "VALUE")

tmp2 <- ltllod_water %>%
  rename_with(~ gsub("PE_wMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x)) %>%
  pivot_longer(-UID, names_to = "ELEMENT", values_to = "LTLOD")

tmp1 <- left_join(tmp1, tmp2, by = c("UID","ELEMENT"))

tmp1 %>% head()

tmp1 <- tmp1 %>%
  mutate(VALUE = ifelse(LTLOD == 1, NA, VALUE)) %>%
  pivot_wider(id_cols = UID, names_from = ELEMENT, values_from = VALUE)

tmp1 %>% head()

tmp1 %>% sapply(function(x) sum(is.na(x)))

##### Implement MICE ###########################################################
# Predictor Matrix
mice_predictors <- matrix(1, 17, 17)
mice_predictors[,1] <- mice_predictors[1,] <- 0

tmp3 <- read_csv("~/Johns Hopkins/PAIR Data - Documents/Data/Current/assay_water_metals/pair_watermetals_llod_2022_1030.csv")

llod <- left_join(tibble(ELEMENT = colnames(tmp1)), tmp3, by = "ELEMENT")
llod <- llod %>% pull(LLOD)

# Run MICE
impt <- mice(tmp1, m = 5, maxit = 1, method = "leftcenslognorm", predictorMatrix = mice_predictors, lod = llod)

##### Check Imputations ########################################################



