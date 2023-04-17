################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Section: LLOD
tblS1_water_lod_val <- df_water_llod_val %>%
  filter(!ELEMENT %in% c("Cd","Cu","K","Mg","Na","Pb","Zn")) %>%
  mutate(LLOD = ifelse(LLOD >= 1, round(LLOD, 1), signif(LLOD, 1))) %>%
  select(Element = ELEMENT, LLOD) %>%
  arrange(Element)

# Section: Values <LLOD
tblS1_water_lod_np <- df_water_llod_ind %>% 
  summary_table_lod(filter = c("Cd","Cu","Pb","Zn"))

# Section: GM, GSD, and Percentiles
tblS1_water_val <- df_water_sqt2 %>% 
  summary_table_val()

# Join Table Sections
tblS1_water <- left_join(tblS1_water_lod_val, tblS1_water_lod_np, by = "Element")
tblS1_water <- left_join(tblS1_water, tblS1_water_val, by = "Element")

# Review Table
tblS1_water %>% head()

