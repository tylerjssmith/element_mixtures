################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Prepare Data -- Water

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Set Working Directory
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-JohnsHopkins/PAIR Data - Documents/Data/Current")

##### Read Data ################################################################
pregtrak          <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
water             <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")
df_water_llod_val <- read_csv("assay_water_metals/pair_watermetals_llod_2022_1030.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
# PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID)

pregtrak %>% head()
pregtrak %>% dim()

# Drinking Water Elements (Values/<LLOD)
water <- water %>%
  select(UID, starts_with("PE_wMetals_"))

water <- water %>%
  mutate(UID = as.numeric(UID))

water %>% head()
water %>% dim()
water %>% colnames()

# Drinking Water Elements (LLOD)
df_water_llod_val %>% head()
df_water_llod_val %>% dim()

##### Join Data ################################################################
df_water <- left_join(pregtrak, water, by = "UID")

##### Address Missingness ######################################################
# True Missingness
df_water %>% dim()
df_water %>% sapply(function(x) sum(is.na(x)))

# (DROP: Observations with All Values Missing [n=4])
df_water <- df_water %>%
  filter(!is.na(PE_wMetals_As))

df_water %>% dim()
df_water %>% sapply(function(x) sum(is.na(x)))

# (DROP: Elements with Mass Drift [K, Mg, Na])
df_water <- df_water %>%
  select(
    -contains("_K"), 
    -contains("_Mg"), 
    -contains("_Na")
  )

df_water %>% dim()
df_water %>% sapply(function(x) sum(is.na(x)))

# Values <LLOD
df_water_llod_ind <- df_water %>%
  select(UID, ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_wMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x)) %>%
  pivot_longer(
    cols = -UID, 
    names_to = "Element", 
    values_to = "Indicator"
  )

df_water_llod_ind %>% head()

df_water_llod_ind %>%
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

df_water %>% dim()
df_water %>% sapply(function(x) sum(is.na(x)))

##### Address Measurement Error ################################################
# Drop Aluminum > 50,000 µg/L
# (Note: This value suggests a sand particle was included in the specimen and
#  dissolved via acidification during sample preparation.)
df_water <- df_water %>%
  filter(PE_wMetals_Al <= 50000)

df_water %>% dim()
df_water %>% sapply(function(x) sum(is.na(x)))

##### Prepare Data Objects #####################################################
# Values: LLOD/√2 (Wide)
df_water_sqt2 <- df_water %>%
  select(-ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_wMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x))

df_water_sqt2 <- df_water_sqt2 %>%
  select(sort(colnames(df_water_sqt2))) %>%
  select(UID, everything())

df_water_sqt2 %>% head()

# Values: Missing (Wide)
# (Note: This sets values <LLOD to missing for subsequent imputation.)
df_water_miss <- df_water_sqt2 %>%
  pivot_longer(
    cols = -UID, 
    names_to = "Element", 
    values_to = "Water"
  )

df_water_miss %>% head()

df_water_miss <- left_join(df_water_miss, df_water_llod_ind, 
  by = c("UID","Element"))

df_water_miss %>% head()

df_water_miss <- df_water_miss %>%
  mutate(Water = ifelse(Indicator == 1, NA, Water)) %>%
  pivot_wider(
    id_cols = UID, 
    names_from = Element, 
    values_from = Water
  )

df_water_miss %>% head()

df_water_miss %>% sapply(function(x) sum(is.na(x)))

##### Check Data ###############################################################
df_water_sqt2 %>% head()
df_water_miss %>% head()

dim(df_water_sqt2) == dim(df_water_miss)
colnames(df_water_sqt2) == colnames(df_water_miss)

##### Remove Source Data #######################################################
rm(list = c("pregtrak","water","df_water"))




