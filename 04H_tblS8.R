################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S8

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
(tblS8 <- df_tblS8 %>%
  mutate(rho = round(rho, 2)) %>%
  mutate(rho = format(rho, nsmall = 2)))
