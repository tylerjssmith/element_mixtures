################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Load Functions

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Functions: Check Data ####################################################
# Function: Check Continuous Variables
check_continuous <- function(data, x, xlab, title) 
{
 data %>%
    ggplot(aes(x = {{ x }})) +
    geom_density() +
    labs(
      title = paste("Distribution:", title),
      x = xlab,
      y = "Density") +
    th
}

# Function: Check Discrete Variables
check_discrete <- function(data, x) 
{
  data %>%
    count({{ x }}) %>%
    mutate(p = n / sum(n) * 100)
}

##### Functions: Check Imputations #############################################
check_impt_hist <- function(element, df_mi = df_water_impt, df_s2 = df_water, 
  df_llod = water_llod, title = NULL)
{
  # Data: Multiple Imputation
  df_mi <- df_mi %>% 
    tibble() %>% 
    mutate(type = "Multiple Imputation") %>%
    select(type, .imp, As, {{ element }})

  # Data: LLOD/√2
  df_s2 <- df_s2 %>% 
    tibble() %>%
    mutate(type = "LLOD/√2") %>% 
    mutate(.imp = "LLOD/√2") %>% 
    select(type, .imp, As, {{ element }})

  # Stack Data
  df <- rbind(df_mi, df_s2)

  # Get LLOD
  element_name <- deparse(substitute(element))
  
  llod <- water_llod %>%
    filter(ELEMENT == element_name) %>%
    pull(LLOD)
  
  # Generate Plot
  df %>%
    mutate(.imp = factor(.imp, levels = c("LLOD/√2", 1:m), 
      labels = c("LLOD/√2",paste("Imputation",1:m)))) %>%
    ggplot(aes(x = log({{ element }}))) +
    geom_vline(xintercept = log(llod), linetype = "dashed") +
    geom_histogram(fill = "white", color = "black") +
    facet_wrap(. ~ .imp) +
    labs(
      title = title,
      x = "Log(Concentration)",
      y = "Number of Values",
      color = "Imputation") +
    th + theme(legend.position = "none")
}

check_impt_dens <- function(element, df_mi = df_water_impt, df_s2 = df_water, 
  df_llod = water_llod, title = NULL)
{
  # Data: Multiple Imputation
  df_mi <- df_mi %>% 
    tibble() %>% 
    mutate(type = "Multiple Imputation") %>% 
    select(type, .imp, As, {{ element }})

  # Data: LLOD/√2
  df_s2 <- df_s2 %>% 
    tibble() %>%
    mutate(type = "LLOD/√2") %>% 
    mutate(.imp = "LLOD/√2") %>% 
    select(type, .imp, As, {{ element }})

  # Stack Data
  df <- rbind(df_mi, df_s2)

  # Get LLOD
  element_name <- deparse(substitute(element))
  
  llod <- water_llod %>%
    filter(ELEMENT == element_name) %>%
    pull(LLOD)
  
  # Set Colors for Plot
  scale_m5 <- c("black","#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
  
  # Generate Plot
  df %>%
    mutate(.imp = factor(.imp, levels = c("Constant",1:m))) %>%
    ggplot(aes(x = log({{ element }}), color = factor(.imp))) +
    geom_vline(xintercept = log(llod), linetype = "dashed") +
    geom_density() +
    scale_color_manual(values = scale_m5) +
    facet_grid(type ~ .) +
    labs(
      title = title,
      x = "Log(Concentration)",
      y = "Density",
      color = "Imputation") +
    th + theme(legend.position = "none")
}

##### Functions: Tables and Figures ############################################
# Function: Exponentiate Log 10 Values
base10 <- function(x) 10 ^ x

# Function: Calculate p-value for Spearman's Correlation
cor_pval <- function(x, y, ...) 
{
  suppressWarnings(fit <- cor.test(x, y, ...))
  out <- fit$p.value
  return(out)
}

