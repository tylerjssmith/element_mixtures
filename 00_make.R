################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Make

# Tyler Smith
# April 4, 2023

##### 01 Setup #################################################################
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

# Functions and Themes
source("01A_setup_functions.R")
source("01B_setup_themes.R")

# Data - Prepare
source("01C_setup_data_water.R")
source("01D_setup_data_urine.R")
source("01E_setup_data_covar.R")

# Data - Reconcile
source("01F_setup_data_reconcile.R")

# Data - Impute
source("01G_setup_impute_water.R")
source("01H_setup_impute_urine.R")

##### 02 Analysis ##############################################################
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

# Exploratory Data Analysis
source("02A_explore.R")

# Principal Components Analysis
source("02B_pca_loadings.R")
source("02C_pca_scores.R")

# Spearman's Correlations
source("02D_spearman.R")

##### 03 Tables and Figures ####################################################
# Table 1-2
# (Geometric Means)
source("03A_tbl1.R")
source("03B_tbl2.R")

# Figures 1-2
# (Principal Component Loadings)
source("03C_fig1.R")
source("03D_fig2.R")

# Tables 3-4
# (Principal Component Scores - Adjusted)
source("03E_tbl3.R")
source("03F_tbl4.R")

##### 04 Supplemental Tables and Figures #######################################
# Table S1
# (LLOD)
source("04A_tblS1.R")

# Tables S2-S3
# (Median [IQR])
source("04B_tblS2.R")
source("04C_tblS3.R")

# Tables S4-S5
# (Correlation Matrices)
source("04D_tblS4.R")
source("04E_tblS5.R")

# Tables S6-S7
# (Principal Component Loadings)
source("04F_tblS6.R")
source("04G_tblS7.R")

# Tables S8-S9
# (Principal Component Scores - Unadjusted)
source("04H_tblS8.R")
source("04I_tblS9.R")

# Table S10
# (Spearman's Correlations)
source("04J_tblS10.R")

##### Export Figures ###########################################################
# Export Tables and Figures
source("../tables_figures/export_tables_figures.R")



