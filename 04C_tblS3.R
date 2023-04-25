################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S3

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
df_tbl2 %>% head()

tmp_x <- c("AGE3","SEGSTAGE","PARITY","EDUCATION","LSI4","medSEMUAC4",
  "PESTICIDE","PETOBAC","PEBETEL","PEHCIGAR")
tmp_y_urine <- df_tbl2 %>% select(Al:Zn) %>% colnames()

tblS3 <- rbind(
  # Age
  df_tbl2 %>% tbl_median_iqr(x = AGE3, 
    from = Al, to = Zn),

  # Gestational Age
  df_tbl2 %>% tbl_median_iqr(x = SEGSTAGE, 
    from = Al, to = Zn),

  # Parity
  df_tbl2 %>% tbl_median_iqr(x = PARITY, 
    from = Al, to = Zn),

  # Education
  df_tbl2 %>% tbl_median_iqr(x = EDUCATION, 
    from = Al, to = Zn),

  # Living Standards Index
  df_tbl2 %>% tbl_median_iqr(x = LSI4, 
    from = Al, to = Zn),

  # Mid-upper Arm Circumference
  df_tbl2 %>% tbl_median_iqr(x = medSEMUAC4, 
    from = Al, to = Zn),

  # Pesticide Use
  df_tbl2 %>% tbl_median_iqr(x = PESTICIDE, 
    from = Al, to = Zn),

  # Chewing Tobacco Use
  df_tbl2 %>% tbl_median_iqr(x = PETOBAC, 
    from = Al, to = Zn),

  # Betel Nut Use
  df_tbl2 %>% tbl_median_iqr(x = PEBETEL, 
    from = Al, to = Zn),

  # Husband's Smoking at Home
  df_tbl2 %>% tbl_median_iqr(x = PEHCIGAR, 
    from = Al, to = Zn)
)

tblS3 %>% head()
tblS3 %>% dim()
