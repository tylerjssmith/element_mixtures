################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Figure S4

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(patchwork)

##### Check Imputations ########################################################
df_urine_miss_nosg %>% sapply(function(x) sum(is.na(x)) >= 1)

# Density Plots
figS4_Fe <- check_impt_dens(Fe, df_mi = df_urine_impt_nosg, 
  df_s2 = df_urine_sqt2_nosg, df_llod = df_urine_llod_val, title = "Fe")
figS4_Mn <- check_impt_dens(Mn, df_mi = df_urine_impt_nosg, 
  df_s2 = df_urine_sqt2_nosg, df_llod = df_urine_llod_val, title = "Mn")
figS4_Sb <- check_impt_dens(Sb, df_mi = df_urine_impt_nosg, 
  df_s2 = df_urine_sqt2_nosg, df_llod = df_urine_llod_val, title = "Sb")
figS4_V  <- check_impt_dens(V,  df_mi = df_urine_impt_nosg, 
  df_s2 = df_urine_sqt2_nosg, df_llod = df_urine_llod_val, title = "V")
figS4_W  <- check_impt_dens(W,  df_mi = df_urine_impt_nosg, 
  df_s2 = df_urine_sqt2_nosg, df_llod = df_urine_llod_val, title = "W")

# Histograms
check_impt_hist(Fe, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2_nosg, 
  df_llod = df_urine_llod_val, title = "Iron")
check_impt_hist(Mn, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2_nosg, 
  df_llod = df_urine_llod_val, title = "Manganese")
check_impt_hist(Sb, df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2_nosg, 
  df_llod = df_urine_llod_val, title = "Antimony")
check_impt_hist(V,  df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2_nosg, 
  df_llod = df_urine_llod_val, title = "Vanadium")
check_impt_hist(W,  df_mi = df_urine_impt_nosg, df_s2 = df_urine_sqt2_nosg, 
  df_llod = df_urine_llod_val, title = "Tungsten")
