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
# Function: Density Plots for Imputations
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

# Function: Histograms for Imputations
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

##### Functions: Quantile Regression ###########################################
# Quantile Regression: Point Estimates
quant_reg_point <- function(data, y, x = "As10", tau = seq(0.1,0.9,0.1), m = 25)
{
  require(quantreg)
  
  # Initialize Results Matrix
  out <- matrix(nrow = m, ncol = length(tau))
  
  # Fit Model to Each Data Set
  # Store Result in Matrix
  for(i in 1:m) {
    df <- data[data$.imp == i,]
    fit <- rq(get(y) ~ get(x), tau = tau, data = df)
    res <- coef(fit)[2,]
    out[i,] <- res
  }
  
  # Format Matrix
  rownames(out) <- 1:m
  colnames(out) <- tau
  
  out <- as_tibble(out, rownames = ".imp")
  out <- out %>% 
    mutate(element = y)
  out <- out %>% 
    pivot_longer(
      cols = -c(element,.imp), 
      names_to = "tau", 
      values_to = "estimate"
    )
  
  # Return Matrix
  return(out)
}

# Quantile Regression: Bootstrap Estimates
quant_reg_boot <- function(data, y, x = "As10", tau = seq(0.1,0.9,0.1), m = 25, 
  R = 1000, seed = 7023, verbose = TRUE)
{
  require(quantreg)
  
  # Set Random Seed
  set.seed(seed)
  
  # Initialize Results List (All tau)
  out <- list()
  
  # Loop over tau
  for(t in seq_along(tau)) {
    
    # Initialize Results Matrix (Current tau)
    out_t <- matrix(nrow = R, ncol = m)
  
    # Loop over m
    for(i in 1:m) {
      df <- data[data$.imp == i,]
      fit <- with(df, boot.rq(model.matrix(get(y) ~ get(x)), y = get(y), 
        tau = tau[t], R = R))
      res <- fit$B[,2]
      out_t[,i] <- res
      
      # Send Update
      if(verbose) {
        message(paste(y, ":", i, "/", m, "Imputations for tau =", tau[t]))
      }
    }
    
    # Store Bootstrap Estimates (Current tau)
    out[[t]] <- out_t
  }
  
  # Format List (All tau)
  names(out) <- tau
  
  # Return List (All tau)
  return(out)
}

# Quantile Regression: Get Estimates for Plotting
quant_reg <- function(data, y, ...) {
  # Get Point Estimates
  tmp_pt <- quant_reg_point(data, y, ...)
  
  # Get Bootstrap Estimates
  tmp_bs  <- quant_reg_boot(data, y, ...)
 
  # Prepare Estimates for Plotting
  tmp <- cbind(
    # Point Estimates
    tmp_pt %>% 
      group_by(tau) %>% 
      summarize(mean = mean(estimate)),
    # Lower Bounds
    tmp_bs %>% 
      map_dfr(~ quantile(.x, 0.025), .id = "tau") %>% 
      select(-tau) %>% 
      rename(lb = `2.5%`),
    # Upper Bounds
    tmp_bs %>% 
      map_dfr(~ quantile(.x, 0.975), .id = "tau") %>% 
      select(-tau) %>% 
      rename(ub = `97.5%`)
  )
  
  tmp <- tmp %>%
    mutate(y = y)
  
  return(tmp)
}

##### Functions: Tables and Figures ############################################
# Function: Calculate p-value for Table 1
pval_tbl1 <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
    
  if (is.numeric(y)) {
    p <- kruskal.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }

  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# Function: Summary Statistics for Tables 2-3, Tables S1-S2
summary_table_lod <- function(data, filter, group = Element, indicator = Indicator)
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

# Function: Summary Statistics for Tables 2-3, Tables S1-S2
summary_table_val <- function(data)
{
  # Pivot Longer
  data <- data %>%
    pivot_longer(
      cols = -UID,
      names_to = "Element"
    )
  
  # Calculate Summary Statistics
  data <- data %>%
    group_by(Element) %>%
    summarise(
      # Geometric Mean
       GM    = exp(mean(log(value))),
      # Geometric Standard Deviation
       GSD   = exp(sd(log(value))),
      # Deciles
      `10th` = quantile(value, 0.10),
      `20th` = quantile(value, 0.20),
      `30th` = quantile(value, 0.30),
      `40th` = quantile(value, 0.40),
      `50th` = quantile(value, 0.50),
      `60th` = quantile(value, 0.60),
      `70th` = quantile(value, 0.70),
      `80th` = quantile(value, 0.80),
      `90th` = quantile(value, 0.90)
    )
  
  # Format Output
  data %>%
    mutate(across(-Element, ~ ifelse(.x >= 1, round(.x, 1), signif(.x, 1)))) %>%
    mutate(`GM (GSD)` = paste0(GM, " (", GSD, ")")) %>%
    select(Element, `GM (GSD)`, everything()) %>%
    select(-c(GM,GSD))
}

