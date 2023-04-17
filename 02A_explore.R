################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Exploratory Data Analysis

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Summarize Urinary Arsenobetaine ##########################################
df_covar %>% 
  select(uAsB) %>% 
  na.omit() %>% 
  summarise(
    n = n(),
    median = median(uAsB), 
    q1 = quantile(uAsB, 0.25), 
    q3 = quantile(uAsB, 0.75)
  )

##### Check Outliers: Drinking Water ###########################################
# LLOD/√2
# (Counts)
df_water_sqt2 %>%
  pivot_longer(-UID, names_to = "Element", values_to = "Water") %>%
  group_by(Element) %>%
  mutate(Water = scale(log(Water))) %>%
  mutate(TukeyFenceLower = quantile(Water, 0.25) - 1.5 * IQR(Water)) %>%
  mutate(TukeyFenceUpper = quantile(Water, 0.75) + 1.5 * IQR(Water)) %>%
  mutate(Outlier = ifelse(Water < TukeyFenceLower | Water > TukeyFenceUpper, 1, 0)) %>%
  filter(Outlier == 1) %>%
  summarise(n = n()) %>%
  mutate(p = n / nrow(df_water_sqt2) * 100) %>%
  arrange(desc(n))

# (Boxplots)
df_water_sqt2 %>%
  pivot_longer(-UID, names_to = "Element", values_to = "Water") %>%
  group_by(Element) %>%
  mutate(Water = scale(log(Water))) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Element), y = Water, group = Element)) +
  geom_boxplot() +
  labs(
    title = "Drinking Water Elements: LLOD/√2",
    x = "Element",
    y = "Log(Concentration)") +
  theme_bw() +
  th

# Imputation 1
# (Counts)
df_water_impt %>%
  filter(.imp == 1) %>%
  pivot_longer(-c(.imp,.id,UID), names_to = "Element", values_to = "Water") %>%
  group_by(Element) %>%
  mutate(Water = scale(log(Water))) %>%
  mutate(TukeyFenceLower = quantile(Water, 0.25) - 1.5 * IQR(Water)) %>%
  mutate(TukeyFenceUpper = quantile(Water, 0.75) + 1.5 * IQR(Water)) %>%
  mutate(Outlier = ifelse(Water < TukeyFenceLower | Water > TukeyFenceUpper, 1, 0)) %>%
  filter(Outlier == 1) %>%
  summarise(n = n()) %>%
  mutate(p = n / nrow(df_water_sqt2) * 100) %>%
  arrange(desc(n))

# (Boxplots)
df_water_impt %>%
  filter(.imp == 1) %>%
  pivot_longer(-c(.imp,.id,UID), names_to = "Element", values_to = "Water") %>%
  group_by(Element) %>%
  mutate(Water = scale(log(Water))) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Element), y = Water, group = Element)) +
  geom_boxplot() +
  labs(
    title = "Drinking Water Elements: Imputation 1",
    x = "Element",
    y = "Log(Concentration)") +
  theme_bw() +
  th

##### Check Outliers: Urine ####################################################
# LLOD/√2
# (Counts)
df_urine_sqt2 %>%
  pivot_longer(-c(UID,SPECIFICGRAVITY), names_to = "Element", values_to = "Urine") %>%
  group_by(Element) %>%
  mutate(Urine = scale(log(Urine))) %>%
  mutate(TukeyFenceLower = quantile(Urine, 0.25) - 1.5 * IQR(Urine)) %>%
  mutate(TukeyFenceUpper = quantile(Urine, 0.75) + 1.5 * IQR(Urine)) %>%
  mutate(Outlier = ifelse(Urine < TukeyFenceLower | Urine > TukeyFenceUpper, 1, 0)) %>%
  filter(Outlier == 1) %>%
  summarise(n = n()) %>%
  mutate(p = n / nrow(df_urine_sqt2) * 100) %>%
  arrange(desc(n))

# (Boxplots)
df_urine_sqt2 %>%
  pivot_longer(-c(UID,SPECIFICGRAVITY), names_to = "Element", values_to = "Urine") %>%
  group_by(Element) %>%
  mutate(Urine = scale(log(Urine))) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Element), y = Urine, group = Element)) +
  geom_boxplot() +
  labs(
    title = "Urinary Elements",
    x = "Element",
    y = "Log(Concentration)") +
  th

# Imputation 1
# (Counts)
df_urine_impt %>%
  filter(.imp == 1) %>%
  pivot_longer(-c(.imp,.id,UID,SPECIFICGRAVITY), names_to = "Element", values_to = "Urine") %>%
  group_by(Element) %>%
  mutate(Urine = scale(log(Urine))) %>%
  mutate(TukeyFenceLower = quantile(Urine, 0.25) - 1.5 * IQR(Urine)) %>%
  mutate(TukeyFenceUpper = quantile(Urine, 0.75) + 1.5 * IQR(Urine)) %>%
  mutate(Outlier = ifelse(Urine < TukeyFenceLower | Urine > TukeyFenceUpper, 1, 0)) %>%
  filter(Outlier == 1) %>%
  summarise(n = n()) %>%
  mutate(p = n / nrow(df_urine_sqt2) * 100) %>%
  arrange(desc(n))

# (Boxplots)
df_urine_impt %>%
  filter(.imp == 1) %>%
  pivot_longer(-c(.imp,.id,UID,SPECIFICGRAVITY), names_to = "Element", values_to = "Urine") %>%
  group_by(Element) %>%
  mutate(Urine = scale(log(Urine))) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Element), y = Urine, group = Element)) +
  geom_boxplot() +
  labs(
    title = "Urinary Elements: Imputation 1",
    x = "Element",
    y = "Log(Concentration)") +
  th
