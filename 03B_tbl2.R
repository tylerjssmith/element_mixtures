################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
df_tbl2 <- left_join(df_covar, df_urine_impt, by = "UID")

df_tbl2 %>% head()

tmp_x <- c("AGE3","SEGSTAGE","PARITY","EDUCATION","LSI4","medSEMUAC4","PESTICIDE","PETOBAC","PEBETEL","PEHCIGAR")
tmp_y_urine <- df_tbl2 %>% select(Al:Zn) %>% colnames()

tbl2 <- rbind(
  # Age
  df_tbl2 %>% tbl_gm(x = AGE3, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "AGE3"),
  
  # Gestational Age
  df_tbl2 %>% tbl_gm(x = SEGSTAGE, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "SEGSTAGE"),
  
  # Parity
  df_tbl2 %>% tbl_gm(x = PARITY, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "PARITY"),
  
  # Education
  df_tbl2 %>% tbl_gm(x = EDUCATION, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "EDUCATION"),
  
  # Living Standards Index
  df_tbl2 %>% tbl_gm(x = LSI4, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "LSI4"),
  
  # Mid-upper Arm Circumference
  df_tbl2 %>% tbl_gm(x = medSEMUAC4, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "medSEMUAC4"),
  
  # Pesticide Use
  df_tbl2 %>% tbl_gm(x = PESTICIDE, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "PESTICIDE"),
  
  # Chewing Tobacco Use
  df_tbl2 %>% tbl_gm(x = PETOBAC, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "PETOBAC"),
  
  # Betel Nut Use
  df_tbl2 %>% tbl_gm(x = PEBETEL, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "PEBETEL"),
  
  # Husband's Smoking at Home
  df_tbl2 %>% tbl_gm(x = PEHCIGAR, from = Al, to = Zn),
  df_tbl2 %>% tbl_pval(y = tmp_y_urine, x = "PEHCIGAR")
)

tbl2 %>% head()
tbl2 %>% dim()

