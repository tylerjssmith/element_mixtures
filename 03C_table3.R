################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 3

# Note: This script currently uses the first imputation as an example.

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Section: LLOD
(tbl3_urine_lod_val <- df_urine_llod_val %>%
  filter(!ELEMENT %in% c("U")) %>%
  mutate(LLOD = ifelse(LLOD >= 1, round(LLOD, 1), signif(LLOD, 1))) %>%
  select(Element = ELEMENT, LLOD) %>%
  arrange(Element))

# Section: Values <LLOD
(tbl3_urine_lod_np <- df_urine_llod_ind %>% 
  summary_table_lod(filter = "U"))

# Section: GM, GSD, and Percentiles
(tbl3_urine_val <- df_urine_impt %>%
  filter(.imp == 1) %>%
  select(-c(.imp,.id,SPECIFICGRAVITY)) %>%
  summary_table_val())

# Join Table Sections
tbl3_urine <- left_join(tbl3_urine_lod_val, tbl3_urine_lod_np, by = "Element")
tbl3_urine <- left_join(tbl3_urine, tbl3_urine_val, by = "Element")

# Set Arsenic to Missing
# (Note: Urinary arsenic is the sum of urinary inorganic, monomethyl, and dimethyl
#  arsenic. The LLOD for each species was 0.05 µg/L. Values of all three species
#  were ≥LLOD for all participants.)
tbl3_urine <- tbl3_urine %>%
  mutate(LLOD = ifelse(Element == "As", NA, LLOD)) %>%
  mutate(`<LLOD [n (%)]` = ifelse(Element == "As", "0 (0)", `<LLOD [n (%)]`))

# Review Table
tbl3_urine %>% head()
