################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Load Functions

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Load Functions ###########################################################
# Function: Compare Imputations
check_impt <- function(data_mi, data_cons, element) {
  # Data: Multiple Imputation
  tmp1 <- complete(data_mi, action = "long")
  
  tmp1 <- tmp1 %>% 
    tibble() %>% 
    mutate(type = "Multiple Imputation") %>% 
    select(type, .imp, {{ element }})

  # Data: Constant Imputation
  tmp2 <- data_cons %>% 
    mutate(type = "LLOD/√2") %>% 
    mutate(.imp = "Constant") %>% 
    select(type, .imp, {{ element }})

  # Stack Data
  tmp3 <- rbind(tmp1, tmp2)

  # Generate Plot
  plot <- tmp3 %>%
    ggplot(aes(x = log({{ element }}), color = factor(.imp))) +
    geom_density() +
    facet_wrap(. ~ type, scales = "free_y", ncol = 1) +
    labs(
      x = "Concentration (Natural Log)",
      y = "Density",
      color = "Imputation") +
    th
  
  # Return plot
  return(plot)
}

# Function: Calculate p-value for Spearman's Correlation
cor_pval <- function(x, y, ...) {
  suppressWarnings(fit <- cor.test(x, y, ...))
  out <- fit$p.value
  return(out)
}