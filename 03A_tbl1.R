################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
df_tbl1 <- left_join(df_covar, df_water_impt, by = "UID")

df_tbl1 %>% head()

tmp_x <- c("AGE3","SEGSTAGE","PARITY","EDUCATION","LSI4","medSEMUAC4",
  "PESTICIDE","PETOBAC","PEBETEL","PEHCIGAR")
tmp_y_water <- df_tbl1 %>% select(Al:W) %>% colnames()

tbl1 <- rbind(
  # Age
  df_tbl1 %>% tbl_gm(x = AGE3, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "AGE3"),
  
  # Gestational Age
  df_tbl1 %>% tbl_gm(x = SEGSTAGE, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "SEGSTAGE"),
  
  # Parity
  df_tbl1 %>% tbl_gm(x = PARITY, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "PARITY"),
  
  # Education
  df_tbl1 %>% tbl_gm(x = EDUCATION, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "EDUCATION"),
  
  # Living Standards Index
  df_tbl1 %>% tbl_gm(x = LSI4, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "LSI4"),
  
  # Mid-upper Arm Circumference
  df_tbl1 %>% tbl_gm(x = medSEMUAC4, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "medSEMUAC4"),
  
  # Pesticide Use
  df_tbl1 %>% tbl_gm(x = PESTICIDE, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "PESTICIDE"),
  
  # Chewing Tobacco Use
  df_tbl1 %>% tbl_gm(x = PETOBAC, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "PETOBAC"),
  
  # Betel Nut Use
  df_tbl1 %>% tbl_gm(x = PEBETEL, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "PEBETEL"),
  
  # Husband's Smoking at Home
  df_tbl1 %>% tbl_gm(x = PEHCIGAR, from = Al, to = W),
  df_tbl1 %>% tbl_pval(y = tmp_y_water, x = "PEHCIGAR")
)

tbl1 %>% head()
tbl1 %>% dim()


