################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Exploratory Data Analysis

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Summarize Urinary Arsenobetaine ##########################################
tmp <- left_join(df_water, df_covar, by = "UID")

tmp %>% 
  select(uAsB) %>% 
  na.omit() %>% 
  summarise(
    n = n(),
    median = median(uAsB), 
    q1 = quantile(uAsB, 0.25), 
    q3 = quantile(uAsB, 0.75)
  )

rm(tmp)

##### Check Outliers ###########################################################

##### Generate Density Plots ###################################################

##### Generate Box Plots #######################################################