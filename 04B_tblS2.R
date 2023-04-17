################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Section: LLOD
tblS2_urine_lod_val <- df_urine_llod_val %>%
  filter(!ELEMENT %in% c("U")) %>%
  mutate(LLOD = ifelse(LLOD >= 1, round(LLOD, 1), signif(LLOD, 1))) %>%
  select(Element = ELEMENT, LLOD) %>%
  arrange(Element)

# Section: Values <LLOD
tblS2_urine_lod_np <- df_urine_llod_ind %>% 
  summary_table_lod(filter = "U")

# Section: GM, GSD, and Percentiles
tblS2_urine_val <- df_urine_sqt2 %>% 
  summary_table_val()

# Join Table Sections
tblS2_urine <- left_join(tblS2_urine_lod_val, tblS2_urine_lod_np, by = "Element")
tblS2_urine <- left_join(tblS2_urine, tblS2_urine_val, by = "Element")

# Set Arsenic to Missing
# (Note: Urinary arsenic is the sum of urinary inorganic, monomethyl, and dimethyl
#  arsenic. The LLOD for each species was 0.05 µg/L. Values of all three species
#  were ≥LLOD for all participants.)
tblS2_urine <- tblS2_urine %>%
  mutate(LLOD = ifelse(Element == "As", NA, LLOD)) %>%
  mutate(`<LLOD [n (%)]` = ifelse(Element == "As", "0 (0)", `<LLOD [n (%)]`))

# Review Table
tblS2_urine %>% head()

