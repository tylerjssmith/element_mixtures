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
pregtrak   <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
water      <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")
water_llod <- read_csv("assay_water_metals/pair_watermetals_llod_2022_1030.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
# PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID)

pregtrak %>% head()

# Drinking Water Elements
water <- water %>%
  select(UID, starts_with("PE_wMetals_"))

water <- water %>%
  mutate(UID = as.numeric(UID))

water %>% head()

##### Join Data ################################################################
df_water <- left_join(pregtrak, water, by = "UID")

##### Address Missingness ######################################################
# True Missingness
df_water %>% nrow()
df_water %>% sapply(function(x) sum(is.na(x)))

# (DROP: Observations with All Values Missing [n=4])
df_water <- df_water %>%
  filter(!is.na(PE_wMetals_As))
df_water %>% nrow()

# (DROP: Elements with Mass Drift [K, Mg, Na])
df_water <- df_water %>%
  select(
    -contains("_K"), 
    -contains("_Mg"), 
    -contains("_Na")
  )

# Values <LLOD
df_water_llod_indicators <- df_water %>%
  select(UID, ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_wMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x)) %>%
  pivot_longer(
    cols = -UID, 
    names_to = "Element", 
    values_to = "Indicator"
  )

df_water_llod_indicators %>% head()

df_water_llod_indicators %>%
  group_by(Element) %>%
  summarise(
    n = sum(Indicator),
    p = n / nrow(df_water) * 100) %>%
  arrange(desc(p))

# (DROP: Elements with >50% of Values <LLOD [Cd, Cu, Pb, Zn])
df_water <- df_water %>%
  select(
    -contains("_Cd"), 
    -contains("_Cu"), 
    -contains("_Pb"),
    -contains("_Zn")
  )

##### Prepare Data Objects #####################################################
# Values: Linear Scale (Wide)
df_water <- df_water %>%
  select(-ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_wMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x))

df_water <- df_water %>%
  select(sort(colnames(df_water))) %>%
  select(UID, everything())

df_water

# Values: Linear Scale (Long)
df_water_long <- df_water %>%
  pivot_longer(
    cols = -UID, 
    names_to = "Element", 
    values_to = "Water"
  )

df_water_long %>% head()

# Values: Linear Scale (Wide) (<LLOD Set to Missing)
df_water_missing <- left_join(df_water_long, df_water_llod_indicators, 
  by = c("UID","Element"))

df_water_missing <- df_water_missing %>%
  mutate(Water = ifelse(Indicator == 1, NA, Water)) %>%
  pivot_wider(id_cols = UID, names_from = Element, values_from = Water)

df_water_missing %>% head()

df_water_missing %>% sapply(function(x) sum(is.na(x)))

##### Remove Source Data #######################################################
rm(list = c("pregtrak","water"))




