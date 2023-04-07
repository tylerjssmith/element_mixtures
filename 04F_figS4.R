################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Figure S3

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
# Extract Eigenvalues
figS3_eigenvalues <- pca_fit_full$values %>%
  as_tibble(rownames = "PC") %>%
  mutate(PC = as.numeric(PC))

# Generate Figure
(fig_S3 <- figS3_eigenvalues %>%
  ggplot(aes(x = PC, y = value)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1,16,1)) +
  scale_y_continuous(limits = c(0,6), breaks = seq(0,10,1)) +
  labs(
    x = "Principal Component",
    y = "Eigenvalue") +
  th)
