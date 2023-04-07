################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Table S2

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Generate Table
tblS2 <- df_urine %>%
  group_by(ELEMENT) %>%
  summarise(
    n = n(),
    rho = cor(WATER, URINE, method = "spearman"),
    p = cor_pval(WATER, URINE, method = "spearman")
  )

# Format rho
tblS2 <- tblS2 %>%
  mutate(rho = round(rho, 2))

# Format p-values
tblS2 <- tblS2 %>%
  mutate(
    p =
      ifelse(p >= 0.01,                 round(p, 2),
      ifelse(p <  0.01    & p > 0.001,  "<0.01",
      ifelse(p <  0.001   & p > 0.0001, "<0.001",
      ifelse(p <  0.00001,              "<0.0001", NA)))))
  
