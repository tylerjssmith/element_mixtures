################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Make

# Tyler Smith
# April 4, 2023

##### 01 Setup #################################################################
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

# 01A Functions
source("01A_setup_functions.R")

# 01B Themes
source("01B_setup_themes.R")

# 01C Data - Water
source("01C_setup_data_water.R")

# 01D Data - Urine
source("01D_setup_data_urine.R")

# 01E Data - Covariates
source("01E_setup_data_covar.R")

# 01F Data - Reconcile
source("01F_setup_data_reconcile.R")

# 01G Data - Water - Impute
source("01G_setup_impute_water.R")

# 01F Data - Urine - Impute
source("01H_setup_impute_urine.R")

##### 02 Analysis ##############################################################
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

# 02A Exploratory Data Analysis
source("02A_explore.R")

# 02B Principal Component Analyses
source("02B_pca.R")

# 02C Quantile Regression
source("02C_quantreg.R")

# 02D Spearman's Correlations
source("02D_spearman.R")

##### 03 Tables and Figures ####################################################
# 03A Table 1 (Participant Characteristics)
source("03A_table1.R")

# 03B Table 2 (Drinking Water Elements -- Multiple Imputation)
source("03B_table2.R")

# 03C Table 3 (Urinary Elements -- Multiple Imputation)
source("03C_table3.R")

# 03D Figure 1 (Loadings -- Drinking Water Elements)
source("03D_fig1.R")

# 03E Figure 2 (Loadings -- Urinary Elements)
source("03E_fig2.R")

##### 04 Supplemental Tables and Figures #######################################
# 04A Table S1 (Drinking Water Elements -- LLOD/√2)
source("04A_tblS1.R")

# 04B Table S2 (Urinary Elements -- LLOD/√2)
source("04B_tblS2.R")

# 04C Figure S1 (Percentages <LLOD -- Drinking Water)
source("04C_figS1.R")

# 04D Figure S2 (Percentages <LLOD -- Urine)
source("04D_figS2.R")

# 04E Figure S3 (Imputations -- Drinking Water)
source("04E_figS3.R")

# 04F Figure S4 (Imputations -- Urine)
source("04F_figS4.R")

# 04G Figure S5 (Mean Correlation Matrix -- Drinking Water)
source("04G_figS5.R")

# 04H Figure S6 (Scree Plot -- Drinking Water)
source("04H_figS6.R")

# 04I Figure S5 (Mean Correlation Matrix -- Urine)
source("04I_figS7.R")

# 04J Figure S5 (Scree Plot -- Urine)
source("04J_figS8.R")

# 04K Figure S5 (Spearman's Correlations)
source("04K_figS9.R")

##### Export Figures ###########################################################



