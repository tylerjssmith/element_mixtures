################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Figure S8

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
# Extract Eigenvalues
figS8_eigenvalues <- pca_fit_urine$values %>%
  as_tibble(rownames = "PC") %>%
  mutate(PC = as.numeric(PC))

# Generate Figure
(fig_S8 <- figS8_eigenvalues %>%
  ggplot(aes(x = PC, y = value)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1,26), breaks = seq(1,26,1)) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,10,1)) +
  labs(
    x = "Principal Component",
    y = "Eigenvalue") +
  th)
