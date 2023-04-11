################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure 1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
# Extract and Prepare Loadings
pca_loadings <- unclass(pca_fit$loadings)[,1:4]

pca_loadings <- pca_loadings %>%
  as_tibble(rownames = "Element")

pca_loadings %>% head()  

# Generate Figure
(fig1 <- pca_loadings %>%
  pivot_longer(-Element, names_to = "Component", values_to = "Loading") %>%
  ggplot(aes(x = Loading, y = Element)) +
  geom_vline(xintercept = -0.4, linetype = "dashed") +
  geom_vline(xintercept = 0.4, linetype = "dashed") +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-0.4,0.4,0.4)) +
  scale_y_discrete(limits = rev) +
  facet_wrap(. ~ Component, nrow = 1) +
  th + theme(panel.grid.major.y = element_blank()))



