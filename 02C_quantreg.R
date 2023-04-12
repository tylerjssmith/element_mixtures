################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Quantile Regression

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(quantreg)
library(broom)

##### Fit Models ###############################################################
# Check Data
df_water_impt %>% head()
df_water_impt %>% group_by(.imp) %>% count(As > 10)

# Specify Percentiles to Estimate
tau <- c(0.10,0.30,0.50,0.70,0.90)

# Select Imputation
df_water_impt1 <- df_water_impt %>%
  filter(.imp == 1) %>%
  mutate(As10 = ifelse(As < 10, 1, 0)) %>%
  mutate(across(Al:W, ~ log(.x)))

df_water_impt1 %>% head()
df_water_impt1 %>% nrow()

# Fit Model
quantreg_results_imp1 <- df_water_impt1 %>%
  select(-c(.imp,.id,UID,As,As10)) %>%
  map(~ rq(.x ~ As10, data = df_water_impt1, tau = tau)) %>%
  map_dfr(tidy, se.type = "boot", R = 1000, .id = "Element") %>%
  filter(term == "As10") %>%
  mutate(conf.low = estimate - 1.96 * std.error) %>%
  mutate(conf.high = estimate + 1.96 * std.error) %>%
  mutate(tau = tau * 100) %>%
  select(Element, term, tau, everything())

quantreg_results_imp1 %>% head()

##### Sensitivity Analysis #####################################################
# Check Data
impt_combined_no_outliers %>% head()
impt_combined_no_outliers %>% group_by(.imp) %>% count(As10) %>% filter(As10 == 1)

# Specify Percentiles to Estimate
tau <- c(0.10,0.30,0.50,0.70,0.90)

# Select Imputation
impt_combined1_no_outliers <- impt_combined_no_outliers %>%
  filter(.imp == 1)

impt_combined1_no_outliers %>% head()
impt_combined1_no_outliers %>% nrow()

# Fit Model
quantreg_results_imp1_no_outliers <- impt_combined1_no_outliers %>%
  select(-c(.imp,.id,UID,As,As10)) %>%
  map(~ rq(.x ~ As10, data = impt_combined1_no_outliers, tau = tau)) %>%
  map_dfr(tidy, se.type = "boot", R = 1000, .id = "Element") %>%
  filter(term == "As10") %>%
  mutate(conf.low = estimate - 1.96 * std.error) %>%
  mutate(conf.high = estimate + 1.96 * std.error) %>%
  mutate(tau = tau * 100) %>%
  select(Element, term, tau, everything())

quantreg_results_imp1_no_outliers %>% head()



