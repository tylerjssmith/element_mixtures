################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Figure S3

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(patchwork)

##### Check Imputations ########################################################
df_water_miss %>% sapply(function(x) sum(is.na(x)) >= 1)

# Density Plots
figS3_Al <- check_impt_dens(Al, title = "Al")
figS3_As <- check_impt_dens(As, title = "As")
figS3_Br <- check_impt_dens(Br, title = "Br")
figS3_Fe <- check_impt_dens(Fe, title = "Fe")
figS3_Mo <- check_impt_dens(Mo, title = "Mo")
figS3_P  <- check_impt_dens(P,  title = "P")
figS3_S  <- check_impt_dens(S,  title = "S")
figS3_Sb <- check_impt_dens(Sb, title = "Sb")
figS3_U  <- check_impt_dens(U,  title = "U")
figS3_V  <- check_impt_dens(V,  title = "V")
figS3_W  <- check_impt_dens(W,  title = "W")

(figS3_pg1 <- (figS3_Al + figS3_As + figS3_Br) / (figS3_Fe + figS3_Mo + figS3_P))
(figS3_pg2 <- (figS3_S + figS3_Sb + figS3_U) / (figS3_V + figS3_W))

# Histograms
check_impt_hist(Al, title = "Aluminum")
check_impt_hist(Br, title = "Bromine")
check_impt_hist(S,  title = "Sulphur")
check_impt_hist(Sb, title = "Antimony")
check_impt_hist(U,  title = "Uranium")
check_impt_hist(W,  title = "Tungsten")