################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S10

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
(tblS10 <- df_tblS10 %>%
  mutate(rho = round(rho, 2)) %>%
  mutate(rho = format(rho, nsmall = 2)))
