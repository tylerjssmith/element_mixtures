################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 2

# Note: This script currently uses the first imputation as an example.

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
df_tbl2 <- left_join(df_covar, df_urine_impt, by = "UID") %>%
  select(-c(wAs,uAs,wAs10,uAsB,.imp,.id))

df_tbl2 %>% head()

tmp_x <- c("AGE3","SEGSTAGE","PARITY","EDUCATION","LSI2","SEBMI3","PESTICIDE","PETOBAC","PEBETEL","PEHCIGAR")
tmp_y_urine <- df_tbl2 %>% select(Al:Zn) %>% colnames()

tbl2 <- rbind(
  # Age
  df_tbl2 %>% tbl1_tbl2(x = AGE3, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "AGE3"),
  
  # Gestational Age
  df_tbl2 %>% tbl1_tbl2(x = SEGSTAGE, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "SEGSTAGE"),
  
  # Parity
  df_tbl2 %>% tbl1_tbl2(x = PARITY, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "PARITY"),
  
  # Education
  df_tbl2 %>% tbl1_tbl2(x = EDUCATION, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "EDUCATION"),
  
  # Living Standards Index
  df_tbl2 %>% tbl1_tbl2(x = LSI2, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "LSI2"),
  
  # Body Mass Index
  df_tbl2 %>% tbl1_tbl2(x = SEBMI3, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "SEBMI3"),
  
  # Pesticide Use
  df_tbl2 %>% tbl1_tbl2(x = PESTICIDE, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "PESTICIDE"),
  
  # Chewing Tobacco Use
  df_tbl2 %>% tbl1_tbl2(x = PETOBAC, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "PETOBAC"),
  
  # Betel Nut Use
  df_tbl2 %>% tbl1_tbl2(x = PEBETEL, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "PEBETEL"),
  
  # Husband's Smoking at Home
  df_tbl2 %>% tbl1_tbl2(x = PEHCIGAR, from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2_pval(y = tmp_y_urine, x = "PEHCIGAR")
)

tbl2 %>% head()
tbl2 %>% dim()

