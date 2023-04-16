################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Table 1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(table1)

##### Generate Table ###########################################################
# Labels
label(df_covar$AGE)       <- "Age"
label(df_covar$SEGSTAGE)  <- "Gestational Age"
label(df_covar$PARITY)    <- "Parity"
label(df_covar$EDUCATION) <- "Education"
label(df_covar$LSI)       <- "Living Standard Index"
label(df_covar$SEBMI)     <- "Body Mass Index"
label(df_covar$medSEMUAC) <- "Mid-upper Arm Circumference"
label(df_covar$PESTICIDE) <- "Pesticide User"
label(df_covar$PETOBAC)   <- "Chewing Tobacco User"
label(df_covar$PEBETEL)   <- "Betel Nut User"
label(df_covar$PEHCIGAR)  <- "Husband Smokes at Home"

# Units
units(df_covar$AGE)       <- "years"
units(df_covar$SEGSTAGE)  <- "weeks"
units(df_covar$SEBMI)     <- "kg/m^2"
units(df_covar$medSEMUAC) <- "cm"

# Table
(tbl1 <- table1(~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + SEBMI + medSEMUAC + PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR | wAs10, 
  data = df_covar, overall = FALSE, render.continuous = c("Mean (SD)" = "MEAN (SD)"), extra.col = list(`p` = pval_tbl1)))
  
  