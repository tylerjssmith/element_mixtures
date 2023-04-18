################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure 2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(scales)

##### Extract and Prepare Loadings #############################################
# Get Loadings for Principal Components with Eigenvalues >1
n_pc_urine <- sum(pca_fit_urine$values > 1)
pca_loadings_urine <- unclass(pca_fit_urine$loadings)[,1:n_pc_urine]

# Convert Loadings to Tibble
pca_loadings_urine <- pca_loadings_urine %>%
  as_tibble(rownames = "Element")

# Label Absolute Value(Loading) >0.4
pca_loadings_urine <- pca_loadings_urine %>%
  pivot_longer(
    cols = -Element, 
    names_to = "Component", 
    values_to = "Loading"
  )
  
pca_loadings_urine <- pca_loadings_urine %>%
  mutate(LoadingCat = ifelse(abs(Loading) > 0.4, 1, 0)) %>%
  mutate(LoadingCat = factor(LoadingCat, levels = c(0,1), 
    labels = c("No","Yes")))

##### Extract and Prepare Percentages of Variance Explained ####################
pca_pervar_urine <- (pca_fit_urine$values / sum(pca_fit_urine$values))[1:n_pc_urine]
pca_pervar_urine <- round(pca_pervar_urine * 100, 1)

pca_pervar_urine <- tibble(
  Component = paste0("PC",1:n_pc_urine),
  Percentage = pca_pervar_urine
)

# Combine Loadings and Percentages of Variance Explained
df_fig2 <- left_join(pca_loadings_urine, pca_pervar_urine, by = "Component")

df_fig2 <- df_fig2 %>%
  mutate(ComponentLab = paste0(Component, " (", Percentage, "%)"))

df_fig2 %>% head()

##### Generate Figure ##########################################################
(fig2 <- df_fig2 %>%
  ggplot(aes(x = Loading, y = Element, fill = LoadingCat)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(-1,1), breaks = c(-0.4,0.4)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("#d9d9d9","#48494B")) +
  facet_wrap(. ~ ComponentLab, nrow = 1) +
  labs(
    fill = "Absolute Value(Loading) >0.4") +
  th + 
  theme(
    panel.grid.major.x = element_line(),
    legend.position = "bottom"))


