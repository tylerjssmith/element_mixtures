################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S3

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
(tblS5 <- df_urine_impt_ln_z %>% cor())

(tblS5 <- ifelse(lower.tri(tblS5), tblS5, NA))

diag(tblS5) <- rep(1, 26)

colnames(tblS5) <- colnames(df_urine_impt_ln_z)

tblS5 <- tblS5 %>% as_tibble(rownames = "Element")

tblS5$Element <- colnames(tblS5)[-1]

tblS5
