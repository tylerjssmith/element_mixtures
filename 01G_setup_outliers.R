################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Outliers

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Check Outliers ###########################################################
df_water_long %>%
  group_by(Element) %>%
  mutate(Water = scale(log(Water))) %>%
  group_by(Element) %>%
  mutate(TukeyFenceLower = quantile(Water, 0.25) - 1.5 * IQR(Water)) %>%
  mutate(TukeyFenceUpper = quantile(Water, 0.75) + 1.5 * IQR(Water)) %>%
  ungroup() %>%
  mutate(Outlier = ifelse(Water < TukeyFenceLower | Water > TukeyFenceUpper, 1, 0)) %>%
  filter(Outlier == 1) %>%
  count(Element) %>%
  mutate(p = n / nrow(df_water) * 100) %>%
  arrange(desc(n))

##### Prepare 

impt_combined_no_outliers <- impt_combined %>%
  pivot_longer(cols = -c(.imp,.id,UID,As10), names_to = "Element", values_to = "Conc") %>%
  mutate(Conc = log(Conc)) %>%
  group_by(.imp, Element) %>%
  mutate(tukey_lw = quantile(Conc, 0.25) - 1.5 * IQR(Conc)) %>%
  mutate(tukey_up = quantile(Conc, 0.75) + 1.5 * IQR(Conc)) %>%
  ungroup() %>%
  mutate(outlier = ifelse(Conc < tukey_lw | Conc > tukey_up, 1, 0)) %>%
  filter(outlier == 0) %>%
  mutate(Conc = exp(Conc)) %>%
  pivot_wider(id_cols = c(.imp, .id, UID, As10), names_from = Element, values_from = Conc) %>%
  na.omit()
