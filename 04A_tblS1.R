################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Drinking Water
# (LLOD)
(tmp_tblS1a <- df_water_llod_val %>%
  filter(!ELEMENT %in% c("K","Mg","Na")) %>%
  mutate(LLOD = ifelse(LLOD >= 1, round(LLOD, 1), signif(LLOD, 1))) %>%
  select(Element = ELEMENT, LLOD) %>%
  arrange(Element))

# (Values <LLOD)
(tmp_tblS1b <- df_water_llod_ind %>% 
  tbl_llod(filter = c("K","Mg","Na")))

# Urine
# (LLOD)
(tmp_tblS1c <- df_urine_llod_val %>%
  mutate(LLOD = ifelse(LLOD >= 1, round(LLOD, 1), signif(LLOD, 1))) %>%
  mutate(LLOD = ifelse(ELEMENT == "As", NA, LLOD)) %>%
  select(Element = ELEMENT, LLOD) %>%
  arrange(Element))

# (Values <LLOD)
(tmp_tblS1d <- df_urine_llod_ind %>% 
  tbl_llod(filter = NULL))

(tmp_tblS1_water <- left_join(tmp_tblS1a, tmp_tblS1b, by = "Element"))
(tmp_tblS1_urine <- left_join(tmp_tblS1c, tmp_tblS1d, by = "Element"))

# Join Table Sections
(tblS1 <- right_join(tmp_tblS1_water, tmp_tblS1_urine, by = "Element"))

rm(list = c("tmp_tblS1a","tmp_tblS1b","tmp_tblS1c","tmp_tblS1d",
  "tmp_tblS1_water","tmp_tblS1_urine"))
