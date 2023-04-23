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

tbl2 <- rbind(
  df_tbl2 %>% tbl1_tbl2(AGE3, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(SEGSTAGE, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(PARITY, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(EDUCATION, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(LSI2, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(SEBMI3, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(PESTICIDE, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(PETOBAC, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(PEBETEL, 
    from = Al, to = Zn),
  df_tbl2 %>% tbl1_tbl2(PEHCIGAR, 
    from = Al, to = Zn)
)

tbl2 %>% head()
tbl2 %>% dim()
