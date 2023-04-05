################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Load Functions

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mice)
library(qgcomp)
library(psych)
library(quantreg)

##### Read Data ################################################################
pregtrak <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
water_pe <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")
water_vx <- read_csv("assay_water_metals/pair_watermetals_vaxf_2022_1030.csv")

##### Prepare Data ##########################################
# Goal: Prepare two data sets, both with 784 rows and 23 elements, one with 
# values <LLOD set to NA and one with values <LLOD set to LLOD/√2. If supported
# by intra-class correlation analyses, use data from Visit 2 where data from
# Visit 1 are missing due to laboratory problems.

# Combine Data
df <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(UID)

water_pe <- water_pe %>%
  mutate(UID = as.numeric(UID)) %>%
  select(UID, starts_with("PE_wMetals_"))

water_vx <- water_vx %>%
  mutate(UID = as.numeric(UID)) %>%
  select(UID, starts_with("VX_wMetals_"))

df <- left_join(df, water_pe, by = "UID")
df <- left_join(df, water_vx, by = "UID")

# Summarize Values <LLOD

# Summarize Outliers

# Calculate Intra-class Correlation Coefficients

##### Strategy 1: Multiple Imputation ##########################################
# Set Parameters
m <- 50
i <- 1

# Multiply Impute Values
df_mice_impt <- mice(df_mice_miss, m = m, maxit = i, method = "leftcenslognorm")

# Calculate Correlation Matrices
df_mice_comp <- complete(df_mice_impt, action = "long")

# Calculate Mean Correlation Matrix

# Generate Scree Plot

# Run Principal Components Analysis

##### Strategy 2: Constant #####################################################
# Calculate Correlation Matrix

# Generate Scree Plot

# Run Principal Components Analysis

##### Compare Strategies #######################################################
# Compare Correlation Matrices
# Compare Scree Plots
# Compare Loadings

##### Quantile Regression ######################################################
tau <- c(0.10,0.25,0.50,0.75,0.90)

##### Tables and Figures #######################################################
# Table 1: Participant Characteristics by Drinking Water Arsenic Standards
# Table 2: Drinking Water Elements
# Figure 1: Principal Component Loadings
# Figure 2: Quantile Regression by Drinking Water Arsenic Standards

# Figure S1: Correlation Matrix (Multiple Imputation)
# Figure S2: Correlation Matrix (LLOD/√2)
# Figure S3: Principal Component Loadings (LLOD/√2)
# Figure S4: Density 
