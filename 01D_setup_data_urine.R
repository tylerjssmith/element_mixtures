################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Urinary Elements

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

##### Read Data ################################################################
pregtrak      <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
urine_element <- read_csv("assay_urinary_metals/pair_urinarymetals_2022_1029.csv")
urine_arsenic <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")
urine_llod    <- read_csv("assay_urinary_metals/pair_urinarymetals_llod_2022_1029.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
# PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID)

pregtrak %>% head()

# Urinary Elements
urine_element <- urine_element %>%
  select(UID, starts_with("PE_uMetals_")) %>%
  select(UID, ends_with("_SG") | ends_with("_LTLOD")) %>%
  select(-contains("_As"))

# Urinary Arsenic
urine_arsenic <- urine_arsenic %>%
  select(UID, As = PE_uAs_Sum_SG)

##### Join Data ################################################################
df_urine <- left_join(pregtrak, urine_element, by = "UID")
df_urine <- left_join(df_urine, urine_arsenic, by = "UID")

df_urine %>% head()

##### Address Missingness ######################################################
# True Missingness
df_urine %>% nrow()
df_urine %>% sapply(function(x) sum(is.na(x)))

# (DROP: Observations with All Values Missing [n=4])
df_urine <- df_urine %>%
  filter(!is.na(As))

# Values <LLOD
# (Note: This does not include urinary arsenic.)
df_urine_llod_indicators <- df_urine %>%
  select(UID, ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x)) %>%
  pivot_longer(
    cols = -UID, 
    names_to = "Element", 
    values_to = "Indicator"
  )

df_urine_llod_indicators %>%
  group_by(Element) %>%
  summarise(
    n = sum(Indicator),
    p = n / nrow(df_urine) * 100) %>%
  arrange(desc(p))

# (DROP: Elements with >50% of Values <LLOD [U])
df_urine <- df_urine %>%
  select(
    -contains("_U_")
  )

##### Prepare Data Objects #####################################################
# Values: Linear Scale (Wide)
df_urine <- df_urine %>%
  select(-ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x)) %>%
  rename_with(~ gsub("_SG", "", .x))
  
df_urine <- df_urine %>%
  select(sort(colnames(df_urine))) %>%
  select(UID, everything())

df_urine %>% head()

# Values: Linear Scale (Long)
df_urine_long <- df_urine %>%
  pivot_longer(
    cols = -UID,
    names_to = "Element",
    values_to = "Urine"
  )

##### Remove Source Data #######################################################
rm(list = c("pregtrak","urine_arsenic","urine_element"))




