################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Principal Components Analysis

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(psych)

##### Drinking Water Elements ##################################################
# Extract and Join Scores
pca_scores_water <- pca_fit_water$scores %>% 
  as_tibble() %>% 
  select(all_of(1:n_pc_water))

pca_scores_water %>% head()
pca_scores_water %>% dim()

df_covar_scores_water <- cbind(df_covar, pca_scores_water) %>%
  as_tibble()

df_covar_scores_water <- df_covar_scores_water %>%
  select(AGEIQR,SEGSTAGE,PARITY,EDUCATION,LSIIQR,medSEMUACIQR,PESTICIDE,
    PETOBAC,PEBETEL,PEHCIGAR,PC1:PC4)

df_covar_scores_water %>% head()
df_covar_scores_water %>% dim()

# Unadjusted Models
df_tblS8 <- rbind(
  df_covar_scores_water %>% lm_pca_scores(y = "PC1"),
  df_covar_scores_water %>% lm_pca_scores(y = "PC2"),
  df_covar_scores_water %>% lm_pca_scores(y = "PC3"),
  df_covar_scores_water %>% lm_pca_scores(y = "PC4")
)

# Adjusted Models
df_tbl3 <- rbind(
  # PC1
  tidy(lm(PC1 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_water), 
    conf.int = TRUE) %>% mutate(y = "PC1"),
  
  # PC2
  tidy(lm(PC2 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_water), 
    conf.int = TRUE) %>% mutate(y = "PC2"),
  
  # PC3
  tidy(lm(PC3 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_water), 
    conf.int = TRUE) %>% mutate(y = "PC3"),
  
  # PC4
  tidy(lm(PC4 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_water), 
    conf.int = TRUE) %>% mutate(y = "PC4")
)

##### Urinary Elements #########################################################
# Extract and Join Scores
pca_scores_urine <- pca_fit_urine$scores %>% 
  as_tibble() %>% 
  select(all_of(1:n_pc_urine))

pca_scores_urine %>% head()
pca_scores_urine %>% dim()

df_covar_scores_urine <- cbind(df_covar, pca_scores_urine) %>%
  as_tibble()

df_covar_scores_urine <- df_covar_scores_urine %>%
  select(AGEIQR,SEGSTAGE,PARITY,EDUCATION,LSIIQR,medSEMUACIQR,PESTICIDE,
    PETOBAC,PEBETEL,PEHCIGAR,PC1:PC7)

# (Reverse Sign on PC5)
# (Notes: In PCA, the solution is unique up to a sign flip. Here, the signs
#  on PC5 loadings and scores were reversed so that As, Mo, and W load
#  positively, which improves interpretability.)
df_covar_scores_urine <- df_covar_scores_urine %>%
  mutate(PC5 = PC5 * -1)

df_covar_scores_urine %>% head()
df_covar_scores_urine %>% dim()

# Unadjusted Models
df_tblS9 <- rbind(
  df_covar_scores_urine %>% lm_pca_scores(y = "PC1", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC2", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC3", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC4", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC5", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC6", ymax = PC7),
  df_covar_scores_urine %>% lm_pca_scores(y = "PC7", ymax = PC7)
)

# Adjusted Models
df_tbl4 <- rbind(
  # PC1
  tidy(lm(PC1 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_urine), 
    conf.int = TRUE) %>% mutate(y = "PC1"),
  
  # PC2
  tidy(lm(PC2 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_urine), 
    conf.int = TRUE) %>% mutate(y = "PC2"),
  
  # PC3
  tidy(lm(PC3 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_urine), 
    conf.int = TRUE) %>% mutate(y = "PC3"),
  
  # PC4
  tidy(lm(PC4 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_urine), 
    conf.int = TRUE) %>% mutate(y = "PC4"),
  
  # PC5
  tidy(lm(PC5 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_urine), 
    conf.int = TRUE) %>% mutate(y = "PC5"),
  
  # PC6
  tidy(lm(PC6 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_urine), 
    conf.int = TRUE) %>% mutate(y = "PC6"),
  
  # PC7
  tidy(lm(PC7 ~ AGEIQR + SEGSTAGE + PARITY + EDUCATION + LSIIQR + medSEMUACIQR + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_covar_scores_urine), 
    conf.int = TRUE) %>% mutate(y = "PC7")
)

