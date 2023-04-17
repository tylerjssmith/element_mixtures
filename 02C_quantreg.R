################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Quantile Regression

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(quantreg)
library(broom)

##### Prepare Data #############################################################
# Check Data
df_water_impt %>% head()

df_water_impt %>% 
  group_by(.imp) %>% 
  count(As > 10) %>%
  filter(`As > 10` == TRUE) %>%
  pull(n)

# Select Imputation
df_water_impt <- df_water_impt %>%
  mutate(As10 = ifelse(As > 10, 1, 0))

##### Set Model Parameters #####################################################
# Specify Percentiles to Estimate
tau <- c(0.10,0.30,0.50,0.70,0.90)

# Set Number of Bootstrap Estimates per Imputed Data Set
R <- 1000

quant_reg <- function(data, y, x = "As10", tau = seq(0.1,0.9,0.1), m = 25)
{
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
    pivot_longer(-c(element,.imp), names_to = "tau", values_to = "estimate")
  
  # Return Matrix
  return(out)
}

quant_reg_boot_element <- function(data, y, x = "As10", tau = seq(0.1,0.9,0.1), m = 25, R = 1000)
{
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
    }
    
    # Store Bootstrap Estimates (Current tau)
    out[[t]] <- out_t
  }
  
  # Format List (All tau)
  names(out) <- tau
  
  # Return List (All tau)
  return(out)
}

##### Drinking Water Iron ######################################################
tmp_pt_fe <- quant_reg(df_water_impt, y = "Fe")
tmp_bs_fe <- quant_reg_boot_element(df_water_impt, y = "Fe")

tmp_fe <- cbind(
  tmp_pt_fe %>% group_by(tau) %>% summarize(mean = mean(estimate)),
  tmp_bs_fe %>% map_dfr(~ quantile(.x, 0.025), .id = "tau") %>% select(-tau) %>% rename(lb = `2.5%`),
  tmp_bs_fe %>% map_dfr(~ quantile(.x, 0.975), .id = "tau") %>% select(-tau) %>% rename(ub = `97.5%`)
)

(fig2_fe <- tmp_fe %>%
  as_tibble() %>%
  mutate(tau = as.numeric(tau) * 100) %>%
  ggplot() +
  geom_ribbon(aes(x = tau, y = mean, ymin = lb, ymax = ub), fill = "lightgray") +
  geom_line(aes(x = tau, y = mean), inherit.aes = FALSE) +
  geom_point(aes(x = tau, y = mean), inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(10,90,10)) +
  scale_y_continuous(breaks = seq(0,25000,5000), limits = c(0,25000)) +
  labs(
    title = "Quantile Regression: Drinking Water Iron",
    x = "Percentile",
    y = "Expected Difference (µg/L)\n(95% Confidence Interval)") +
  th)

##### Drinking Water Manganese #################################################
tmp_pt_mn <- quant_reg(df_water_impt, y = "Mn")
tmp_bs_mn <- quant_reg_boot_element(df_water_impt, y = "Mn")

tmp_mn <- cbind(
  tmp_pt_mn %>% group_by(tau) %>% summarize(mean = mean(estimate)),
  tmp_bs_mn %>% map_dfr(~ quantile(.x, 0.025), .id = "tau") %>% select(-tau) %>% rename(lb = `2.5%`),
  tmp_bs_mn %>% map_dfr(~ quantile(.x, 0.975), .id = "tau") %>% select(-tau) %>% rename(ub = `97.5%`)
)

(fig2_mn <- tmp_mn %>%
  as_tibble() %>%
  mutate(tau = as.numeric(tau) * 100) %>%
  ggplot() +
  geom_ribbon(aes(x = tau, y = mean, ymin = lb, ymax = ub), fill = "lightgray") +
  geom_line(aes(x = tau, y = mean), inherit.aes = FALSE) +
  geom_point(aes(x = tau, y = mean), inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(10,90,10)) +
  #scale_y_continuous(breaks = seq(0,25000,5000), limits = c(0,25000)) +
  labs(
    title = "Quantile Regression: Drinking Water Manganese",
    x = "Percentile",
    y = "Expected Difference (µg/L)\n(95% Confidence Interval)") +
  th)

##### Drinking Water Molybdenum ################################################
tmp_pt_mo <- quant_reg(df_water_impt, y = "Mo")
tmp_bs_mo <- quant_reg_boot_element(df_water_impt, y = "Mo")

tmp_mo <- cbind(
  tmp_pt_mo %>% group_by(tau) %>% summarize(mean = mean(estimate)),
  tmp_bs_mo %>% map_dfr(~ quantile(.x, 0.025), .id = "tau") %>% select(-tau) %>% rename(lb = `2.5%`),
  tmp_bs_mo %>% map_dfr(~ quantile(.x, 0.975), .id = "tau") %>% select(-tau) %>% rename(ub = `97.5%`)
)

(fig2_mo <- tmp_mo %>%
  as_tibble() %>%
  mutate(tau = as.numeric(tau) * 100) %>%
  ggplot() +
  geom_ribbon(aes(x = tau, y = mean, ymin = lb, ymax = ub), fill = "lightgray") +
  geom_line(aes(x = tau, y = mean), inherit.aes = FALSE) +
  geom_point(aes(x = tau, y = mean), inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(10,90,10)) +
  #scale_y_continuous(breaks = seq(0,25000,5000), limits = c(0,25000)) +
  labs(
    title = "Quantile Regression: Drinking Water Molybdenum",
    x = "Percentile",
    y = "Expected Difference (µg/L)\n(95% Confidence Interval)") +
  th)

##### Drinking Water Tungsten ##################################################
tmp_pt_w <- quant_reg(df_water_impt, y = "W")
tmp_bs_w <- quant_reg_boot_element(df_water_impt, y = "W")

tmp_w <- cbind(
  tmp_pt_w %>% group_by(tau) %>% summarize(mean = mean(estimate)),
  tmp_bs_w %>% map_dfr(~ quantile(.x, 0.025), .id = "tau") %>% select(-tau) %>% rename(lb = `2.5%`),
  tmp_bs_w %>% map_dfr(~ quantile(.x, 0.975), .id = "tau") %>% select(-tau) %>% rename(ub = `97.5%`)
)

(fig2_w <- tmp_w %>%
  as_tibble() %>%
  mutate(tau = as.numeric(tau) * 100) %>%
  ggplot() +
  geom_ribbon(aes(x = tau, y = mean, ymin = lb, ymax = ub), fill = "lightgray") +
  geom_line(aes(x = tau, y = mean), inherit.aes = FALSE) +
  geom_point(aes(x = tau, y = mean), inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(10,90,10)) +
  #scale_y_continuous(breaks = seq(0,25000,5000), limits = c(0,25000)) +
  labs(
    title = "Quantile Regression: Drinking Water Tungsten",
    x = "Percentile",
    y = "Expected Difference (µg/L)\n(95% Confidence Interval)") +
  th)

