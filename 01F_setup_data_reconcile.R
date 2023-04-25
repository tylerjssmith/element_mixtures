################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Prepare Data - Reconcile

# The goal of this script is to confirm the number of drinking water and urinary
# elements and number of observations to include in the analysis. Data sets are
# updated to reflect the these decisions.

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Check Variables ##########################################################
# Check Consistency within Drinking Water
unique(colnames(df_water_sqt2) == colnames(df_water_miss)) == 1

# Check Consistency within Urine
unique(colnames(df_urine_sqt2_nosg) == colnames(df_urine_miss_nosg)) == 1

# Check Included Elements
# (Note: There are 16 drinking water elements and 26 urinary elements. Of the
#  16 drinking water elements, 15 were measured in urine too.)
col_water <- df_water_miss %>% 
  select(-UID) %>%
  colnames()

col_urine <- df_urine_miss_nosg %>% 
  select(-c(UID,SPECIFICGRAVITY)) %>%
  colnames()

length(col_water)
length(col_urine)

sum(col_water %in% col_urine)
sum(col_urine %in% col_water)

##### Reconcile Observations ###################################################
# (Note: The goal is to include a consistent number [n=778] of observations with
#  drinking water and urinary element data at enrollment.)

# Check Consistency within Drinking Water
sum(!df_water_miss$UID %in% df_water_sqt2$UID)

# Check Consistency within Urine
sum(!df_urine_miss_nosg$UID %in% df_urine_sqt2_nosg$UID)

# Get List of Included UID
included_uid <- inner_join(df_water_miss %>% select(UID), 
  df_urine_miss_nosg %>% select(UID), by = "UID")

included_uid <- inner_join(included_uid,
  df_covar %>% na.omit() %>% select(UID), by = "UID")

# Restrict Data to Included UID
# (Drinking Water Values)
df_water_miss <- left_join(included_uid, df_water_miss, by = "UID")
df_water_sqt2 <- left_join(included_uid, df_water_sqt2, by = "UID")

# (Urinary Values)
df_urine_miss_nosg <- left_join(included_uid, df_urine_miss_nosg, by = "UID")
df_urine_sqt2_nosg <- left_join(included_uid, df_urine_sqt2_nosg, by = "UID")

# (<LLOD Indicators)
df_water_llod_ind <- left_join(included_uid, df_water_llod_ind, by = "UID")
df_urine_llod_ind <- left_join(included_uid, df_urine_llod_ind, by = "UID")

# (Participant Characteristics)
df_covar <- left_join(included_uid, df_covar, by = "UID")

##### Calculate Relative Measures on Final Sample ##############################
# Age: IQR Units
df_covar <- df_covar %>%
  mutate(AGEIQR = AGE / IQR(AGE, na.rm = TRUE))

df_covar %>%
  check_continuous(
    x = AGEIQR, 
    xlab = "Age (IQR Units)", 
    title = "Age"
  )

# Living Standards Index: Quartiles
df_covar <- df_covar %>%
  mutate(LSI4 = ntile(LSI, 4))

df_covar <- df_covar %>%
  mutate(LSI4 = factor(LSI4, levels = c(1:4), 
    labels = c("Quartile 1","Quartile 2","Quartile 3","Quartile 4")))

df_covar %>%
  check_discrete(LSI4)

# Living Standards Index: IQR Units
df_covar <- df_covar %>%
  mutate(LSIIQR = LSI / IQR(LSI))

df_covar %>%
  check_continuous(
    x = LSIIQR, 
    xlab = "Living Standards Index (IQR Units)", 
    title = "Living Standards Index"
  )

# Mid-upper Arm Circumference: Quartiles
df_covar <- df_covar %>%
  mutate(medSEMUAC4 = ntile(medSEMUAC, 4))

df_covar <- df_covar %>%
  mutate(medSEMUAC4 = factor(medSEMUAC4, levels = c(1:4), 
    labels = c("Quartile 1","Quartile 2","Quartile 3","Quartile 4")))

df_covar %>%
  check_discrete(medSEMUAC4)

# Mid-upper Arm Circumference: IQR Units
df_covar <- df_covar %>%
  mutate(medSEMUACIQR = medSEMUAC / IQR(medSEMUAC))

df_covar %>%
  check_continuous(
    x = medSEMUACIQR, 
    xlab = "Mid-upper Arm Circumference (IQR Units)", 
    title = "Mid-upper Arm Circumference"
  )




