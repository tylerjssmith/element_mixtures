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
  mutate(Loading04 = ifelse(Loading > 0.4, 1, 0)) %>%
  mutate(Loading04 = factor(Loading04, levels = c(0,1), labels = c("No","Yes"))) %>%
  ggplot(aes(x = Loading, y = Element, fill = Loading04)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-0.8,0.8,0.4)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("lightgray","red")) +
  facet_wrap(. ~ Component, nrow = 1) +
  labs(
    title = "Principal Components Analysis with Mutiple Imputation",
    fill = "Loading >0.4",
    caption = "m=25") +
  th + 
  theme(
    panel.grid.major.x = element_line(),
    legend.position = "bottom"))



