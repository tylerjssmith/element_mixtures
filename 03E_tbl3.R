################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 3

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(scales)

##### Generate Table ###########################################################
tbl3 <- df_tbl3 %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ round(.x, 2))) %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ format(.x, nsmall = 2))) %>%
  mutate(p.value = ifelse(p.value >= 0.01, round(p.value, 2), ifelse(p.value < 0.01 & p.value >= 0.001, "<0.01", ifelse(p.value < 0.001, "<0.001", p.value)))) %>%
  mutate(est = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  pivot_wider(id_cols = c(x,term), names_from = y, values_from = c(est,p.value)) %>%
  mutate(across(est_PC1:p.value_PC4, ~ ifelse(term == "(Intercept)", "Reference", .x))) %>%
  select(x, term, ends_with("PC1"), ends_with("PC2"), ends_with("PC3"), ends_with("PC4"))

tbl3 %>% head()


