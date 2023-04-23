################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Load Functions

# Tyler Smith
# April 23, 2023

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
# Function: Density Plots for Imputations
check_impute <- function(element, df_mi = df_water_impt, df_s2 = df_water_sqt2, 
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

##### Functions: Tables and Figures ############################################
# Function: Generate Tables 1-2
tbl1_tbl2 <- function(data, x, from, to) {
  # Select and Pivot Data
  data <- data %>%
    select({{ x }}, {{ from }}:{{ to }}) %>%
    pivot_longer(
      cols = -{{ x }}, 
      names_to = "Element", 
      values_to = "Conc"
    )
  
  # Calculate Median (IQR)
  data <- data %>%
    group_by(Element, {{ x }}) %>%
    summarise(
      n = n(),
      median = median(Conc),
      q1 = quantile(Conc, 0.25),
      q3 = quantile(Conc, 0.75)
    )
  
  # Format and Concatenate Median (IQR)
  data <- data %>%
    mutate(across(c(median,q1,q3), ~ format(ifelse(.x > 1, round(.x, 1), signif(.x, 1)), nsmall = 1))) %>%
    mutate(value = paste0(median, " (", q1, ", ", q3, ")"))
  
  # Pivot Data; Calculate Proportions
  data <- data %>%
    select(Element, {{ x }}, n, value) %>%
    pivot_wider(id_cols = c({{ x }}, n), names_from = Element, values_from = value) %>%
    mutate(p = n / sum(n) * 100) %>%
    mutate(p = round(p, 1)) %>%
    mutate(p = paste0(n, " (", p, ")"))
  
  # Select and Arrange Columns
  data <- data %>%
    select(-n) %>%
    select(covar = {{ x }}, p, everything())
  
  return(data)
}

# Function: Summary Statistics (LLOD) for Table S1
tblS2_llod <- function(data, filter, group = Element, indicator = Indicator)
{
  # Get Denominator for Percentages
  d <- data %>%
    summarise(n = n_distinct(UID)) %>%
    pull(n)
  
  # Get Counts and Percentages
  data <- data %>%
    filter(!{{ group }} %in% filter) %>%
    group_by({{ group }}) %>%
    count({{ indicator }}) %>%
    arrange({{ group }}, desc({{ indicator }})) %>%
    slice_head() %>%
    mutate(n = ifelse({{ indicator }} == 0, 0, n)) %>%
    mutate(p = round(n / d * 100, 1))
  
  # Format Results
  data %>%
    mutate(`<LLOD [n (%)]` = paste0(n, " (", p, ")")) %>%
    select({{ group }}, `<LLOD [n (%)]`)
}

# Function: Linear Models for Principal Component Scores
lm_pca_scores <- function(data, y, ymin = PC1, ymax = PC4) {
  data %>%
    select(-c({{ ymin }}:{{ ymax }})) %>%
    map(~ lm(get(y) ~ .x, data = data)) %>%
    map_dfr(tidy, conf.int = TRUE, .id = "x") %>%
    mutate(y = y) %>%
    select(y, x, term, estimate, conf.low, conf.high, p.value)
}


