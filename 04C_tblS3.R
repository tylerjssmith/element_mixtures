################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table S3

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
(tblS3 <- df_urine_impt_ln_z %>% cor())

(tblS3 <- ifelse(lower.tri(tblS3), tblS3, NA))

diag(tblS3) <- rep(1, 26)

colnames(tblS3) <- colnames(df_urine_impt_ln_z)

tblS3 <- tblS3 %>% as_tibble(rownames = "Element")

tblS3$Element <- colnames(tblS3)[-1]

tblS3
