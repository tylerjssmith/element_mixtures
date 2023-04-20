################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Table S3

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
(tblS3 <- df_tblS3 %>%
  group_by(Element) %>%
  summarise(
    Mean = mean(rho),
    Minimum = min(rho),
    Maximum = max(rho),
    Range = signif(Maximum - Minimum, 1)
  ))

