################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Quantile Regression

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(quantreg)
library(broom)
library(patchwork)

##### Prepare Data #############################################################
# Check Data
df_water_impt %>% head()

df_water_impt %>% 
  group_by(.imp) %>% 
  count(As > 10) %>%
  filter(`As > 10` == TRUE) %>%
  pull(n)

# Indicate Drinking Water Arsenic >10 µg/L
df_water_impt <- df_water_impt %>%
  mutate(As10 = ifelse(As > 10, 1, 0))

##### Generate Figure ##########################################################
# Track System Time
system.time({
  
  # Get Point Estimates and 95% Confidence Intervals
  df_fig3 <- rbind(
    quant_reg(df_water_impt, y = "Al"),
    quant_reg(df_water_impt, y = "Ba"),
    quant_reg(df_water_impt, y = "Br"),
    quant_reg(df_water_impt, y = "Ca"),
    quant_reg(df_water_impt, y = "Fe"),
    quant_reg(df_water_impt, y = "Mn"),
    quant_reg(df_water_impt, y = "Mo"),
    quant_reg(df_water_impt, y = "P"),
    quant_reg(df_water_impt, y = "S"),
    quant_reg(df_water_impt, y = "Sb"),
    quant_reg(df_water_impt, y = "Si"),
    quant_reg(df_water_impt, y = "Sr"),
    quant_reg(df_water_impt, y = "U"),
    quant_reg(df_water_impt, y = "V"),
    quant_reg(df_water_impt, y = "W")
  )

})


