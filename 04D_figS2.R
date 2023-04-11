################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure S2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Density Plots ###################################################
# Extract Imputation
tmp <- complete(impt, action = 1) %>% tibble()

# Add Arsenic Indicator; Log Transform Elements
tmp <- tmp %>%
  mutate(As1 = ifelse(As > 10, 1, 0)) %>%
  mutate(across(-c(UID,As1), ~ log(.x)))

tmp %>% head()

# Generate Plots
tmp %>%
  pivot_longer(-c(UID,As1)) %>%
  ggplot(aes(x = factor(As1), y = value, group = As1)) +
  geom_boxplot() +
  facet_wrap(. ~ name, scales = "free") +
  th
