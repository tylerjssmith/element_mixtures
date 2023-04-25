################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
df_tbl1 %>% head()

tmp_x <- c("AGE3","SEGSTAGE","PARITY","EDUCATION","LSI4","medSEMUAC4",
  "PESTICIDE","PETOBAC","PEBETEL","PEHCIGAR")
tmp_y_water <- df_tbl1 %>% select(Al:W) %>% colnames()

tblS2 <- rbind(
  # Age
  df_tbl1 %>% tbl_median_iqr(x = AGE3, 
    from = Al, to = W),

  # Gestational Age
  df_tbl1 %>% tbl_median_iqr(x = SEGSTAGE, 
    from = Al, to = W),

  # Parity
  df_tbl1 %>% tbl_median_iqr(x = PARITY, 
    from = Al, to = W),

  # Education
  df_tbl1 %>% tbl_median_iqr(x = EDUCATION, 
    from = Al, to = W),

  # Living Standards Index
  df_tbl1 %>% tbl_median_iqr(x = LSI4, 
    from = Al, to = W),

  # Mid-upper Arm Circumference
  df_tbl1 %>% tbl_median_iqr(x = medSEMUAC4, 
    from = Al, to = W),

  # Pesticide Use
  df_tbl1 %>% tbl_median_iqr(x = PESTICIDE, 
    from = Al, to = W),

  # Chewing Tobacco Use
  df_tbl1 %>% tbl_median_iqr(x = PETOBAC, 
    from = Al, to = W),

  # Betel Nut Use
  df_tbl1 %>% tbl_median_iqr(x = PEBETEL, 
    from = Al, to = W),

  # Husband's Smoking at Home
  df_tbl1 %>% tbl_median_iqr(x = PEHCIGAR, 
    from = Al, to = W)
)

tblS2 %>% head()
tblS2 %>% dim()
