################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Prepare Data - Urine

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Set Working Directory
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-JohnsHopkins/PAIR Data - Documents/Data/Current")

##### Read Data ################################################################
pregtrak          <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
urine_element     <- read_csv("assay_urinary_metals/pair_urinarymetals_2022_1029.csv")
urine_arsenic     <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")
df_urine_llod_val <- read_csv("assay_urinary_metals/pair_urinarymetals_llod_2022_1029.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
# PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID)

pregtrak %>% head()
pregtrak %>% dim()

# Urinary Elements
# (Note: This script uses values prior to correction for specific gravity. The
#  correction will be applied following multiple imputation.)
urine_element <- urine_element %>%
  select(UID, SPECIFICGRAVITY, starts_with("PE_uMetals_")) %>%
  select(c(UID, SPECIFICGRAVITY, !ends_with("_SG"))) %>%
  select(-contains("_As"))

urine_element %>% head()
urine_element %>% dim()

# Urinary Arsenic
# (Note: Since all urinary arsenic species were above the LLOD, they will not be
#  affected by multiple imputation and the sum of specific gravity-corrected
#  inorganic, monomethyl, and dimethyl arsenic is included here.)
urine_arsenic <- urine_arsenic %>%
  select(UID, As = PE_uAs_Sum_SG)

urine_arsenic %>% head()
urine_arsenic %>% dim()

##### Join Data ################################################################
# (Specific Gravity-corrected Values and <LLOD Indicators) 
df_urine <- left_join(pregtrak, urine_element, by = "UID")
df_urine <- left_join(df_urine, urine_arsenic, by = "UID")

df_urine %>% head()
df_urine %>% dim()

##### Address Missingness ######################################################
# True Missingness
df_urine %>% dim()


# (DROP: Observations with All Values Missing [n=4])
df_urine <- df_urine %>%
  filter(!is.na(As))

df_urine %>% dim()
df_urine %>% sapply(function(x) sum(is.na(x)))

# Values <LLOD
# (Note: This does not include urinary arsenic.)
df_urine_llod_ind <- df_urine %>%
  select(UID, ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x)) %>%
  pivot_longer(
    cols = -UID, 
    names_to = "Element", 
    values_to = "Indicator"
  )

df_urine_llod_ind %>% head()

df_urine_llod_ind %>%
  group_by(Element) %>%
  summarise(
    n = sum(Indicator),
    p = n / nrow(df_urine) * 100) %>%
  arrange(desc(p))

# (DROP: Elements with >50% of Values <LLOD [U])
df_urine <- df_urine %>%
  select(!contains("PE_uMetals_U"))

df_urine %>% dim()
df_urine %>% sapply(function(x) sum(is.na(x)))

##### Prepare Data Objects #####################################################
# Values: LLOD/√2 (Wide)
df_urine_sqt2_nosg <- df_urine %>%
  select(-ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x))

df_urine_sqt2_nosg <- df_urine_sqt2_nosg %>%
  select(sort(colnames(df_urine_sqt2_nosg))) %>%
  select(UID, SPECIFICGRAVITY, everything())

df_urine_sqt2_nosg %>% head()
df_urine_sqt2_nosg %>% dim()

# Values: Missing (Wide)
# (Note: This sets values <LLOD to missing for subsequent imputation.)
df_urine_miss_nosg <- df_urine_sqt2_nosg %>%
  pivot_longer(
    cols = -c(UID,SPECIFICGRAVITY),
    names_to = "Element",
    values_to = "Urine"
  )

df_urine_miss_nosg %>% head()

df_urine_miss_nosg <- left_join(df_urine_miss_nosg, df_urine_llod_ind, 
  by = c("UID","Element"))

df_urine_miss_nosg %>% head()

df_urine_miss_nosg <- df_urine_miss_nosg %>%
  mutate(Indicator = ifelse(Element == "As", 0, Indicator))

df_urine_miss_nosg <- df_urine_miss_nosg %>%
  mutate(Urine = ifelse(Indicator == 1, NA, Urine)) %>%
  pivot_wider(
    id_cols = c(UID,SPECIFICGRAVITY), 
    names_from = Element, 
    values_from = Urine
  )

df_urine_miss_nosg %>% head()
df_urine_miss_nosg %>% dim()

##### Check Data ###############################################################
df_urine_sqt2_nosg %>% head()
df_urine_miss_nosg %>% head()

dim(df_urine_sqt2_nosg) == dim(df_urine_miss_nosg)
colnames(df_urine_sqt2_nosg) == colnames(df_urine_miss_nosg)

df_urine_miss_nosg %>% sapply(function(x) sum(is.na(x)))

##### Remove Source Data #######################################################
rm(list = c("pregtrak","urine_arsenic","urine_element","df_urine"))




