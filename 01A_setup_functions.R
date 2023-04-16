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
check_impt_dens <- function(element, df_mi = df_water_impt, df_s2 = df_water_sqt2, 
  df_llod = df_water_llod_val, title = NULL)
{
  require(viridis)
  
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
  
  llod <- df_llod %>%
    filter(ELEMENT == element_name) %>%
    pull(LLOD)
  
  # Set Colors for Plot
  color_scale <- magma(26)
  color_scale[1] <- "black"
  
  # Generate Plot
  df %>%
    mutate(.imp = factor(.imp, levels = c("Constant",1:m))) %>%
    ggplot(aes(x = log({{ element }}), color = factor(.imp))) +
    geom_vline(xintercept = log(llod), linetype = "dashed") +
    geom_density() +
    scale_color_manual(values = color_scale) +
    facet_grid(type ~ .) +
    labs(
      title = title,
      x = "Log(Concentration)",
      y = "Density",
      color = "Imputation") +
    th + theme(legend.position = "none")
}

check_impt_hist <- function(element, df_mi = df_water_impt, df_s2 = df_water_sqt2, 
  df_llod = df_water_llod_val, title = NULL)
{
  # Data: Multiple Imputation
  df_mi <- df_mi %>% 
    tibble() %>% 
    mutate(type = "Multiple Imputation") %>%
    select(type, .imp, {{ element }})

  # Data: LLOD/√2
  df_s2 <- df_s2 %>% 
    tibble() %>%
    mutate(type = "LLOD/√2") %>% 
    mutate(.imp = "LLOD/√2") %>% 
    select(type, .imp, {{ element }})

  # Stack Data
  df <- rbind(df_mi, df_s2)

  # Get LLOD
  element_name <- deparse(substitute(element))
  
  llod <- df_llod %>%
    filter(ELEMENT == element_name) %>%
    pull(LLOD)
  
  # Generate Plot
  df %>%
    mutate(.imp = factor(.imp, levels = c("LLOD/√2", 1:m), 
      labels = c("LLOD/√2",paste("Impute",1:m)))) %>%
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

##### Functions: Tables and Figures ############################################
# Function: Calculate p-value for Table 1
pval_tbl1 <- function(x, ...) {
  # Source: https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
    
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
    
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# Function: Calculate p-value for Spearman's Correlations
pval_corr <- function(x, y, ...) 
{
  suppressWarnings(fit <- cor.test(x, y, ...))
  out <- fit$p.value
  return(out)
}

