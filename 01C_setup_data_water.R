################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Load Functions

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

##### Read Data ################################################################
pregtrak <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
water_pe <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")
water_vx <- read_csv("assay_water_metals/pair_watermetals_vaxf_2022_1030.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
# PREGTRAK
df_water <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID, PEFSST, VAXFSST)

# Drinking Water Elements: PEF
water_pe <- water_pe %>%
  mutate(UID = as.numeric(UID)) %>%
  select(UID, starts_with("PE_wMetals_"))

# Drinking Water Elements: VAXF
water_vx <- water_vx %>%
  mutate(UID = as.numeric(UID)) %>%
  select(UID, starts_with("VX_wMetals_"))

##### Join Data ################################################################
df_water <- left_join(df_water, water_pe, by = "UID")
df_water <- left_join(df_water, water_vx, by = "UID")

df_water %>% head()

# Remove Source Data
rm(list = c("pregtrak","water_pe","water_vx"))

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
  count(PEFSST == 1)

df_water %>%
  select(starts_with("PE_")) %>%
  select(-ends_with("_LTLOD")) %>%
  sapply(function(x) sum(is.na(x)))

# Visit 2
df_water %>%
  count(VAXFSST == 1)

df_water %>%
  select(starts_with("VX_")) %>%
  select(-ends_with("_LTLOD")) %>%
  sapply(function(x) sum(is.na(x)))

##### Keep Complete Cases at Visit 1 ###########################################
df_water <- df_water %>%
  filter(!is.na(PE_wMetals_As))

##### Summarize Values <LLOD ###################################################
llod_water <- df_water %>%
  select(starts_with("PE_wMetals_")) %>%
  select(ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_wMetals_","", .x)) %>%
  rename_with(~ gsub("_LTLOD","",.x)) %>%
  sapply(function(x) sum(x)) %>%
  as_tibble(rownames = "ELEMENT") %>%
  rename(n = value) %>%
  mutate(p = n / nrow(df_water) * 100) %>%
  mutate(CATEGORY = ifelse(p < 10, 1, ifelse(p >= 10 & p < 30, 2, ifelse(p >= 30 & p < 50, 3, 4)))) %>%
  arrange(CATEGORY, ELEMENT)

##### Select Elements (2) ######################################################
# Exclude Elements with >50% of Values <LLOD (Cd, Cu, Pb, Zn)
df_water <- df_water %>%
  select(-c(contains("_Cd"), contains("_Cu"), contains("_Pb"), contains("_Zn")))

