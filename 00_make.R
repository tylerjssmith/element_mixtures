################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Make

# Tyler Smith
# April 4, 2023

##### 01 Setup #################################################################
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

# Functions
source("01A_setup_functions.R")

# Themes
source("01B_setup_themes.R")

# Data - Water
source("01C_setup_data_water.R")

# Data - Urine
source("01D_setup_data_urine.R")

# Data - Covariates
source("01E_setup_data_covar.R")

# Data - Reconcile
source("01F_setup_data_reconcile.R")

# Data - Water - Impute
source("01G_setup_impute_water.R")

# Data - Urine - Impute
source("01H_setup_impute_urine.R")

##### 02 Analysis ##############################################################
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

# Exploratory Data Analysis
source("02A_explore.R")

# Principal Component Analyses
source("02B_pca.R")

# Quantile Regression
source("02C_quantreg.R")

# Spearman's Correlations
source("02D_spearman.R")

##### 03 Tables and Figures ####################################################
# Table 1 
# (Participant Characteristics)
source("03A_table1.R")

# Table 2
# (Summary Statistics -- Multiple Imputation)
source("03B_table2.R")
source("03C_table3.R")

# Figures 1-2 
# (Loadings)
source("03D_fig1.R")
source("03E_fig2.R")

# Figure 3
# (Quantile Regression)

##### 04 Supplemental Tables and Figures #######################################
# Tables S1-S2 
# (Summary Statistics -- LLOD/√2)
source("04A_tblS1.R")
source("04B_tblS2.R")

# Figures S1-S2 
# (Percentages <LLOD)
source("04C_figS1.R")
source("04D_figS2.R")

# Figures S3-S4 
# (Imputation Checks)
source("04E_figS3.R")
source("04F_figS4.R")

# Figures S5-S6 
# (PCA -- Drinking Water)
source("04G_figS5.R")
source("04H_figS6.R")

# Figures S7-S8 
# (PCA -- Urine)
source("04I_figS7.R")
source("04J_figS8.R")

# Figure S9 
# (Spearman's Correlations)
source("04K_figS9.R")

##### Export Figures ###########################################################



