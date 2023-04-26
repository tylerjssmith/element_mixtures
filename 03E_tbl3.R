################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 3

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
tbl3 <- df_tbl3 %>%
  select(-c(std.error,statistic)) %>%
  pivot_wider(id_cols = term, names_from = y, values_from = c(estimate,conf.low,conf.high,p.value)) %>%
  select(term, ends_with("PC1"), ends_with("PC2"), ends_with("PC3"), ends_with("PC4")) %>%
  mutate(across(-c(term, contains("p.value")), ~ format(round(.x, 2), nsmall = 2))) %>%
  mutate(PC1 = paste0(estimate_PC1, " (", conf.low_PC1, ", ", conf.high_PC1, ")")) %>%
  mutate(PC2 = paste0(estimate_PC2, " (", conf.low_PC2, ", ", conf.high_PC2, ")")) %>%
  mutate(PC3 = paste0(estimate_PC3, " (", conf.low_PC3, ", ", conf.high_PC3, ")")) %>%
  mutate(PC4 = paste0(estimate_PC4, " (", conf.low_PC4, ", ", conf.high_PC4, ")")) %>%
  select(term, PC1, p.value_PC1, PC2, p.value_PC2, PC3, p.value_PC3,  PC4, p.value_PC4)

tbl3 <- tbl3 %>%
  mutate(across(contains("p.value"), 
    ~ ifelse(.x >= 0.01,                   format(round(.x, 2), nsmall = 2),
      ifelse(.x <  0.01   & .x >= 0.001,  "<0.01",
      ifelse(.x <  0.001  & .x >= 0.0001, "<0.001",
      ifelse(.x < -0.0001,                "<0.0001", NA))))))
    

tbl3 %>% head()
tbl3 %>% dim()
