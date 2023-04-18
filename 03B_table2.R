################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 2

# Note: This script currently uses the first imputation as an example.

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Section: LLOD
(tbl2_water_lod_val <- df_water_llod_val %>%
  filter(!ELEMENT %in% c("Cd","Cu","K","Mg","Na","Pb","Zn")) %>%
  mutate(LLOD = ifelse(LLOD >= 1, round(LLOD, 1), signif(LLOD, 1))) %>%
  select(Element = ELEMENT, LLOD) %>%
  arrange(Element))

# Section: Values <LLOD
(tbl2_water_lod_np <- df_water_llod_ind %>% 
  summary_table_lod(filter = c("Cd","Cu","Pb","Zn")))

# Section: GM, GSD, and Percentiles
(tbl2_water_val <- df_water_impt %>%
  filter(.imp == 1) %>%
  summary_table_val())

# Join Table Sections
tbl2_water <- left_join(tbl2_water_lod_val, tbl2_water_lod_np, by = "Element")
tbl2_water <- left_join(tbl2_water, tbl2_water_val, by = "Element")

# Review Table
tbl2_water %>% head()