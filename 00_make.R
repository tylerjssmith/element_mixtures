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

# 01D Data - Water - Impute
source("01D_setup_impute_water.R")

# 01E Data - Urine
source("01E_setup_data_urine.R")

# 01F Data - Urine - Impute
source("01F_setup_impute_urine.R")

# 01G Data - Outliers
source("01G_setup_outliers.R")

# 01H Data - Covariates
source("01H_setup_data_covar.R")

##### 02 Analysis ##############################################################
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

# 02A Exploratory Data Analysis
source("02A_explore.R")

# 02E Principal Component Analyses
source("02B_pca.R")

# 02F Quantile Regression
source("02C_quantreg.R")

##### 03 Tables and Figures ####################################################
# 03A Table 1 (Drinking Water Elements)
source("03A_table1.R")

# 03B Table 2 (Participant Characteristics)
source("03B_table2.R")

# 03C Figure 1 (Loadings)
source("03C_fig1.R")

# 03D Figure 2 (Quantile Regression)
source("03D_fig2.R")

##### 04 Supplemental Tables and Figures #######################################
# 04A Table S1 (Urinary Elements)
source("04A_tblS1.R")

# 04B Table S2 (Spearman's Correlations)
source("04B_tblS2.R")

# 04C Figure S1 (Density Plots)
source("04C_figS1.R")

# 04D Figure S2 (Box Plots)
source("04D_figS2.R")

# 04E Figure S3 (Correlations)
source("04C_figS3.R")

# 04F Figure S4 (Scree Plot)
source("04D_figS4.R")

##### Export Figures ###########################################################



