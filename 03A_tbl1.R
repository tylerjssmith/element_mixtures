################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 1

# Note: This script currently uses the first imputation as an example.

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
df_tbl1 <- left_join(df_covar, df_water_impt, by = "UID") %>%
  select(-c(wAs,uAs,wAs10,uAsB,.imp,.id))

df_tbl1 %>% head()

tmp_x <- c("AGE3","SEGSTAGE","PARITY","EDUCATION","LSI2","SEBMI3","PESTICIDE","PETOBAC","PEBETEL","PEHCIGAR")
tmp_y_water <- df_tbl1 %>% select(Al:W) %>% colnames()

tbl1 <- rbind(
  # Age
  df_tbl1 %>% tbl1_tbl2(x = AGE3, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "AGE3"),
  
  # Gestational Age
  df_tbl1 %>% tbl1_tbl2(x = SEGSTAGE, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "SEGSTAGE"),
  
  # Parity
  df_tbl1 %>% tbl1_tbl2(x = PARITY, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "PARITY"),
  
  # Education
  df_tbl1 %>% tbl1_tbl2(x = EDUCATION, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "EDUCATION"),
  
  # Living Standards Index
  df_tbl1 %>% tbl1_tbl2(x = LSI2, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "LSI2"),
  
  # Body Mass Index
  df_tbl1 %>% tbl1_tbl2(x = SEBMI3, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "SEBMI3"),
  
  # Pesticide Use
  df_tbl1 %>% tbl1_tbl2(x = PESTICIDE, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "PESTICIDE"),
  
  # Chewing Tobacco Use
  df_tbl1 %>% tbl1_tbl2(x = PETOBAC, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "PETOBAC"),
  
  # Betel Nut Use
  df_tbl1 %>% tbl1_tbl2(x = PEBETEL, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "PEBETEL"),
  
  # Husband's Smoking at Home
  df_tbl1 %>% tbl1_tbl2(x = PEHCIGAR, from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2_pval(y = tmp_y_water, x = "PEHCIGAR")
)

tbl1 %>% head()
tbl1 %>% dim()


