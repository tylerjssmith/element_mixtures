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
urine_metals  <- read_csv("assay_urinary_metals/pair_urinarymetals_2022_1029.csv")
urine_arsenic <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Prepare Urinary Elements #################################################
# Subset Urinary Elements
df_urine_metals_values <- urine_metals %>%
  select(UID, ends_with("_SG")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x)) %>%
  rename_with(~ gsub("_SG", "", .x)) %>%
  select(-As)

df_urine_metals_values %>% head()

# Subset Urinary Arsenic
df_urine_arsenic_values <- urine_arsenic %>%
  select(UID, As = PE_uAs_Sum_SG)

df_urine_arsenic_values %>% head()

# Join Urinary Elements and Arsenic
df_urine <- left_join(df_urine_metals_values, df_urine_arsenic_values, by = "UID")

df_urine %>% head()

# Prepare to Join
df_urine <- df_urine %>%
  pivot_longer(
    cols = -UID, 
    names_to = "ELEMENT", 
    values_to = "URINE"
  )

df_urine %>% head()

##### Prepare Drinking Water Elements ##########################################
df_water_long <- df_water %>%
  select(UID, starts_with("PE_wMetals_")) %>%
  select(-ends_with("_LTLOD")) %>%
  pivot_longer(-UID, names_to = "ELEMENT", values_to = "WATER") %>%
  mutate(ELEMENT = gsub("PE_wMetals_","",ELEMENT))

df_water_long %>% head()

##### Join Data ################################################################
# Join Data
df_urine <- left_join(df_water_long, df_urine, by = c("UID","ELEMENT"))

df_urine %>% head()

# Drop Incomplete Cases
df_urine <- df_urine %>% 
  na.omit()

# Check Counts
df_urine %>% summarise(n = n_distinct(UID))
df_urine %>% count(ELEMENT)

##### Summarize Values <LLOD ###################################################
tbl_ltllod_urine <- urine_metals %>%
  select(ends_with("_LTLOD")) %>%
  rename_with(~ gsub("PE_uMetals_", "", .x)) %>%
  rename_with(~ gsub("_LTLOD", "", .x)) %>%
  sapply(function(x) sum(x)) %>%
  as_tibble(rownames = "Element") %>%
  filter(Element %in% df_urine$ELEMENT) %>%
  rename(n = value) %>%
  mutate(p = n / n_distinct(df_urine$UID) * 100) %>%
  mutate(Category = ifelse(p < 10, 1, ifelse(p >= 10 & p < 30, 2, ifelse(p >= 30 & p < 50, 3, 4)))) %>%
  select(Element, Count = n, Percentage = p, Category) %>%
  arrange(Category, Element, Percentage)

##### Select Elements ##########################################################
# Exclude Elements with >50% of Values <LLOD (U)
df_urine <- df_urine %>%
  filter(ELEMENT != "U")

# Remove Source Data
rm(list = c("df_urine_arsenic_values","df_urine_metals_values","df_water_long","urine_arsenic","urine_metals"))




