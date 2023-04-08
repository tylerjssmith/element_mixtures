################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Table S2

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Join Water and Urine Data
df_tblS2 <- left_join(df_water_long, df_urine_long, by = c("UID","Element"))

df_tblS2 %>%
  group_by(Element) %>%
  count(is.na(Urine)) %>% arrange(desc(Element))

# Generate Table
(tblS2 <- df_tblS2 %>%
  na.omit() %>%
  group_by(Element) %>%
  summarise(
    n = n(),
    rho = cor(Water, Urine, method = "spearman"),
    p = cor_pval(Water, Urine, method = "spearman")
  ))

# Format rho
(tblS2 <- tblS2 %>%
  mutate(rho = round(rho, 2)))

# Format p-values
(tblS2 <- tblS2 %>%
  mutate(
    p =
      ifelse(p >= 0.01,                 round(p, 2),
      ifelse(p <  0.01    & p > 0.001,  "<0.01",
      ifelse(p <  0.001   & p > 0.0001, "<0.001",
      ifelse(p <  0.00001,              "<0.0001", NA))))))
  
