################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Spearman's Correlations

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Prepare Data #############################################################
# Drinking Water Elements
df_water_impt_long <- df_water_impt %>%
  as_tibble() %>%
  select(-c(.imp,.id)) %>%
  pivot_longer(
    cols = -UID,
    names_to = "Element",
    values_to = "Water"
  )

# Urinary Elements
df_urine_impt_long <- df_urine_impt %>%
  as_tibble() %>%
  select(-c(.imp,.id,SPECIFICGRAVITY)) %>%
  pivot_longer(
    cols = -UID,
    names_to = "Element",
    values_to = "Urine"
  )

df_water_impt_long %>% head()
df_urine_impt_long %>% head()

df_water_impt_long %>%
  group_by(Element) %>%
  summarise(n = n()) %>%
  filter(n != 774)

df_urine_impt_long %>%
  group_by(Element) %>%
  summarise(n = n()) %>%
  filter(n != 774)

##### Estimate Spearman's Correlations #########################################
df_tblS10 <- inner_join(df_water_impt_long, df_urine_impt_long, 
  by = c("UID","Element"))

df_tblS10 <- df_tblS10 %>%
  group_by(Element) %>%
  summarise(
    n = n(),
    rho = cor(Water, Urine, method = "spearman"))

df_tblS10 %>% head()
