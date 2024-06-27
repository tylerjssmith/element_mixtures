# Identifying Drinking Water and Urinary Element Mixtures among Pregnant Women in Rural Northern Bangladesh

This repository contains code for an analysis identifying drinking water and urinary element mixtures among pregnant women in rural northern Bangladesh at enrollment in the Pregnancy, Arsenic, and Immune Response (PAIR) Study. The primary analytic technique is principal components analysis (PCA). The analysis also uses stochastic imputation for values that are less than the lower limit of detection (LLOD) instead of the more traditional imputation of a constant like $LLOD/\sqrt{2}$. You can learn more about the PAIR Study [here](https://doi.org/10.1111/ppe.12949).

## 00 Make File
* [Make File](00_make.R)

## 01 Setup
* [Setup: Functions](01A_setup_functions.R)
* [Setup: Themes](01B_setup_themes.R)
* [Setup: Data - Water](01C_setup_data_water.R)
* [Setup: Data - Urine](01D_setup_data_urine.R)
* [Setup: Data - Covariates](01E_setup_data_covar.R)
* [Setup: Data - Reconcile](01F_setup_data_reconcile.R)
* [Setup: Impute - Water](01G_setup_impute_water.R)
* [Setup: Impute - Urine](01H_setup_impute_urine.R)

## 02 Analysis
* [Analysis: Explore](02A_explore.R)
* [Analysis: Principal Components Analysis - Loadings](02B_pca_loadings.R) 
* [Analysis: Principal Components Analysis - Scores](02C_pca_scores.R) 
* [Analysis: Spearman's Correlations](02D_spearman.R)

## 03 Tables and Figures
* [Table 1: Drinking Water Elements by Participant Characteristics](03A_tbl1.R)
* [Table 2: Urinary Elements by Participant Characteristics](03B_tbl2.R)
* [Table 3: Drinking Water Element Scores by Participant Characteristics](03E_tbl3.R)
* [Table 4: Urinary Element Scores by Participant Characteristics](03F_tbl4.R)
* [Figure 1: Drinking Water Element Loadings](03C_fig1.R)
* [Figure 2: Urinary Element Loadings](03D_fig2.R)

## 04 Supplemental Tables and Figures
* [Table S1](04A_tblS1.R)
* [Table S2](04B_tblS2.R)
* [Table S3](04C_tblS3.R)
* [Table S4](04D_tblS4.R)
* [Table S5](04E_tblS5.R)
* [Table S6](04F_tblS6.R)
* [Table S7](04G_tblS7.R)
* [Table S8](04H_tblS8.R)
* [Table S9](04I_tblS9.R)
* [Table S10](04J_tblS10.R)
