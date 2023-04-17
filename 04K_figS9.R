################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Figure S9

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
(figS9 <- df_figS9 %>%
  ggplot(aes(x = Element, y = rho, group = Element)) +
  geom_point(alpha = 0.4) +
  scale_y_continuous(breaks = seq(-1,1,0.1)) +
  labs(
    x = "Element",
    y = expression("Spearman's " * rho)) +
  th)
  
