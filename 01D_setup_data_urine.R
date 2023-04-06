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
df_urine_mt <- read_csv("assay_urinary_metals/pair_urinarymetals_2022_1029.csv")
df_urine_as <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Prepare Urinary Elements #################################################
# Subset Urinary Elements
df_urine_mt_val <- df_urine_mt %>%
  select(UID, ends_with("_SG")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x)) %>%
  rename_with(~ gsub("_SG", "", .x)) %>%
  select(-As)

df_urine_mt_val %>% head()

# Subset Urinary Arsenic
df_urine_as_val <- df_urine_as %>%
  select(UID, As = PE_uAs_Sum_SG)

df_urine_as_val %>% head()

# Join Urinary Elements and Arsenic
df_urine_mt_val <- left_join(df_urine_mt_val, df_urine_as_val, by = "UID")

df_urine_mt_val %>% head()

# Prepare to Join
df_urine_mt_val <- df_urine_mt_val %>%
  pivot_longer(
    cols = -UID, 
    names_to = "ELEMENT", 
    values_to = "URINE"
  )

df_urine_mt_val %>% head()

##### Prepare Drinking Water Elements ##########################################
df_water_mt_val <- df_water %>%
  select(UID, starts_with("PE_wMetals_")) %>%
  select(-ends_with("_LTLOD")) %>%
  pivot_longer(-UID, names_to = "ELEMENT", values_to = "WATER") %>%
  mutate(ELEMENT = gsub("PE_wMetals_","",ELEMENT))

df_water_mt_val %>% head()

##### Join Data ################################################################
# Join Data
df_urine <- left_join(df_water_mt_val, df_urine_mt_val, by = c("UID","ELEMENT"))

df_urine %>% head()

# Drop Incomplete Cases
df_urine <- df_urine %>% 
  na.omit()

# Check Counts
df_urine %>% summarise(n = n_distinct(UID))
df_urine %>% count(ELEMENT)

# Summarize Values <LLOD
llod_urine <- df_urine_mt %>%
  select(ends_with("_LTLOD")) %>%
  sapply(function(x) sum(x)) %>%
  as_tibble(rownames = "ELEMENT") %>%
  mutate(ELEMENT = gsub("PE_uMetals_", "", ELEMENT)) %>%
  mutate(ELEMENT = gsub("_LTLOD", "", ELEMENT)) %>%
  filter(ELEMENT %in% df_urine$ELEMENT) %>%
  rename(n = value) %>%
  mutate(p = n / n_distinct(df_urine$UID) * 100) %>%
  arrange(desc(p))

# Remove Source Data
rm(list = c("df_urine_as","df_urine_as_val","df_urine_mt","df_urine_mt_val","df_water_mt_val"))




