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

tbl1 <- rbind(
  df_tbl1 %>% tbl1_tbl2(AGE3, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(SEGSTAGE, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(PARITY, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(EDUCATION, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(LSI2, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(SEBMI3, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(PESTICIDE, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(PETOBAC, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(PEBETEL, 
    from = Al, to = W),
  df_tbl1 %>% tbl1_tbl2(PEHCIGAR, 
    from = Al, to = W)
)

tbl1 %>% head()
tbl1 %>% dim()

