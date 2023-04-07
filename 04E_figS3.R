################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Figure S3

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(ggcorrplot)

##### Generate Figure ##########################################################
(figS3 <- corr_mat_mean %>%
  ggcorrplot(lab = TRUE, digits = 2, show.legend = FALSE))

