################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure 1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
# Label Absolute Value[Loading] >0.4
pca_loadings_water_long <- pca_loadings_water %>%
  pivot_longer(
    cols = -Element, 
    names_to = "Component", 
    values_to = "Loading"
  )

pca_loadings_water_long <- pca_loadings_water_long %>%
  mutate(LoadingCat = ifelse(abs(Loading) > 0.4, 1, 0)) %>%
  mutate(LoadingCat = factor(LoadingCat, levels = c(0,1), 
    labels = c("No","Yes")))

# Extract and Prepare Percentages of Variance Explained
pca_pervar_water <- (pca_fit_water$values / sum(pca_fit_water$values))[1:n_pc_water]
pca_pervar_water <- format(round(pca_pervar_water * 100, 1), nsmall = 1)

pca_pervar_water <- tibble(
  Component = paste0("PC",1:n_pc_water),
  Percentage = pca_pervar_water
)

# Combine Loadings and Percentages of Variance Explained
df_fig1 <- left_join(pca_loadings_water_long, pca_pervar_water, by = "Component")

df_fig1 <- df_fig1 %>%
  mutate(ComponentLab = paste0(Component, " (", Percentage, "%)")) %>%
  mutate(ComponentLab = gsub("\\( ", "(", ComponentLab))

df_fig1 %>% head()

# Generate Figure
(fig1 <- df_fig1 %>%
  ggplot(aes(x = Loading, y = Element, fill = LoadingCat)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-0.8,0.8,0.4)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("#d9d9d9","#48494B")) +
  facet_wrap(. ~ ComponentLab, nrow = 1) +
  labs(
    fill = "Absolute Value(Loading) >0.4") +
  th + 
  theme(
    panel.grid.major.x = element_line(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"))


