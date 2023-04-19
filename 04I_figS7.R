################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Figure S7

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(ggcorrplot)

##### Generate Figure ##########################################################
(figS7 <- corr_mat_urine_mean %>%
  ggcorrplot(lab = TRUE, digits = 2, show.legend = FALSE))
