################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Prepare Data -- Water

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

##### Read Data ################################################################
pregtrak <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
water <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
# PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID)

pregtrak %>% head()

# Drinking Water Elements: Values
water_values <- water %>%
  mutate(UID = as.numeric(UID)) %>%
  select(UID, starts_with("PE_wMetals_")) %>%
  select(-ends_with("_LTLOD"))

water_values %>% head()

# Drinking Water Elements: <LLOD
ltllod_water <- water %>%
  mutate(UID = as.numeric(UID)) %>%
  select(UID, ends_with("_LTLOD"))

ltllod_water %>% head()

##### Join Data ################################################################
df_water <- left_join(pregtrak, water_values, by = "UID")

df_water %>% head()

# Remove Source Data
rm(list = c("pregtrak","water","water_values"))

##### Select Elements (1) ######################################################
# Exclude Elements with Mass Drift (K, Mg, Na)
df_water <- df_water %>%
  select(-c(contains("_K"), contains("_Mg"), contains("_Na")))

# List Included Drinking Water Elements
df_water %>%
  select(starts_with("PE_wMetals_")) %>%
  select(-ends_with("_LTLOD")) %>%
  colnames() %>%
  gsub("PE_wMetals_","", .) %>%
  sort()

##### Summarize True Missingness ###############################################
# Visit 1
df_water %>%
  nrow()

df_water %>%
  sapply(function(x) sum(is.na(x)))

##### Keep Complete Cases at Visit 1 ###########################################
df_water <- df_water %>%
  filter(!is.na(PE_wMetals_As))

##### Summarize Values <LLOD ###################################################
tbl_ltllod_water <- ltllod_water %>%
  select(-UID) %>%
  select(-c(contains("_K"), contains("_Mg"), contains("_Na"))) %>%
  rename_with(~ gsub("PE_wMetals_","", .x)) %>%
  rename_with(~ gsub("_LTLOD","",.x)) %>%
  sapply(function(x) sum(x)) %>%
  as_tibble(rownames = "Element") %>%
  rename(n = value) %>%
  mutate(p = n / nrow(df_water) * 100) %>%
  mutate(Category = ifelse(p < 10, 1, ifelse(p >= 10 & p < 30, 2, ifelse(p >= 30 & p < 50, 3, 4)))) %>%
  select(Element, Count = n, Percentage = p, Category) %>%
  arrange(Category, Element, Percentage)

##### Select Elements (2) ######################################################
# Exclude Elements with >50% of Values <LLOD (Cd, Cu, Pb, Zn)
df_water <- df_water %>%
  select(-c(contains("_Cd"), contains("_Cu"), contains("_Pb"), contains("_Zn")))

