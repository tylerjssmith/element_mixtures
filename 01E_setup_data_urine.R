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

# Urinary Elements
# (Note: Starting with Values Uncorrected for Specific Gravity)
urine_element <- urine_element %>%
  select(UID, SPECIFICGRAVITY, starts_with("PE_uMetals_")) %>%
  select(c(UID, SPECIFICGRAVITY, !ends_with("_SG"))) %>%
  select(-contains("_As"))

urine_element %>% head()

# Urinary Arsenic
urine_arsenic <- urine_arsenic %>%
  select(UID, As = PE_uAs_Sum_SG)

urine_arsenic %>% head()

##### Join Data ################################################################
# (Specific Gravity-corrected Values and <LLOD Indicators) 
df_urine <- left_join(pregtrak, urine_element, by = "UID")
df_urine <- left_join(df_urine, urine_arsenic, by = "UID")

df_urine %>% head()

##### Address Missingness ######################################################
# True Missingness
df_urine %>% dim()
df_urine %>% sapply(function(x) sum(is.na(x)))

# (DROP: Observations with All Values Missing [n=4])
df_urine <- df_urine %>%
  filter(!is.na(As))
df_urine %>% dim()

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

##### Prepare Data Objects #####################################################
# Values: Linear Scale (Wide)
df_urine_sqt2 <- df_urine %>%
  select(-ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x))

df_urine_sqt2 <- df_urine_sqt2 %>%
  select(sort(colnames(df_urine_sqt2))) %>%
  select(UID, SPECIFICGRAVITY, everything())

df_urine_sqt2 %>% head()

# Values: Linear Scale (Long)
df_urine_sqt2_long <- df_urine_sqt2 %>%
  pivot_longer(
    cols = -c(UID,SPECIFICGRAVITY),
    names_to = "Element",
    values_to = "Urine"
  )

df_urine_sqt2_long %>% head()

# Values: Linear Scale (Wide) (<LLOD Set to Missing)
df_urine_miss <- left_join(df_urine_sqt2_long, df_urine_llod_ind, 
  by = c("UID","Element"))

df_urine_miss

df_urine_miss <- df_urine_miss %>%
  mutate(Indicator = ifelse(Element == "As", 0, Indicator))

df_urine_miss <- df_urine_miss %>%
  mutate(Urine = ifelse(Indicator == 1, NA, Urine)) %>%
  pivot_wider(id_cols = c(UID,SPECIFICGRAVITY), names_from = Element, 
    values_from = Urine)

df_urine_miss %>% head()

df_urine_miss %>% sapply(function(x) sum(is.na(x)))

##### Remove Source Data #######################################################
rm(list = c("pregtrak","urine_arsenic","urine_element","df_urine"))




