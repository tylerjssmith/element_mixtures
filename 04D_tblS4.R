################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
(tblS4 <- df_water_impt_ln_z %>% cor())

(tblS4 <- ifelse(lower.tri(tblS4), tblS4, NA))

diag(tblS4) <- rep(1, 16)

colnames(tblS4) <- colnames(df_water_impt_ln_z)

tblS4 <- tblS4 %>% as_tibble(rownames = "Element")

tblS4$Element <- colnames(tblS4)[-1]

tblS4
