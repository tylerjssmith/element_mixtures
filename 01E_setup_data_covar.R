################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Prepare Data - Covariates

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(lubridate)

# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

##### Read Data ################################################################
pregtrak <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
pefsst   <- read_csv("pefsst/pair_pefsst_2022_0310.csv")
parity   <- read_csv("pair_reprohistory/pair_reprohistory_2022_0328.csv")
ses      <- read_csv("ses/pair_ses_2022_0310.csv")
pef      <- read_csv("pef/pair_pef_2022_0310.csv")
urine    <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")
water    <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
# PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(
    UID, 
    DOBYY, 
    BGLMPWK
  )

# PEFSST
pefsst <- pefsst %>%
  select(
    UID, 
    SEDATE, 
    SEWKINT,
    SEBMI,
    medSEMUAC
  )

# Parity
parity <- parity %>%
  select(
    UID, 
    PARITY = FDPSR_PARITY
  )

# SES
ses <- ses %>%
  select(
    UID, 
    EDUCATION = wehclass_mc2, 
    LSI = lsi
  )

# PEF
pef <- pef %>%
  select(
    UID,
    PEMIXPEST,
    PESPRAYPEST,
    PETOBAC,
    PEBETEL,
    PEHCIGAR
  )

# Urinary Arsenic
urine <- urine %>%
  select(
    UID,
    uAs = PE_uAs_Sum_SG,
    uAsB = PE_uAs_Ab_SG
  )

# Drinking Water Arsenic
water <- water %>%
  select(
    UID,
    wAs = PE_wMetals_As
  )

water <- water %>%
  mutate(UID = as.numeric(UID))

##### Join Data ################################################################
df_covar <- left_join(pregtrak, pefsst, by = "UID")
df_covar <- left_join(df_covar, parity, by = "UID")
df_covar <- left_join(df_covar, ses, by = "UID")
df_covar <- left_join(df_covar, pef, by = "UID")
df_covar <- left_join(df_covar, urine, by = "UID")
df_covar <- left_join(df_covar, water, by = "UID")

df_covar %>% head()

# Remove Source Data
rm(list = c("pregtrak","pefsst","parity","ses","pef","urine","water"))

##### Age ######################################################################
# Age
df_covar <- df_covar %>%
  mutate(AGE = year(SEDATE) - DOBYY)

df_covar %>%
  check_continuous(
    x = AGE, 
    xlab = "Age (years)", 
    title = "Age"
  )

df_covar %>%
  select(AGE) %>%
  na.omit() %>%
  summarise(
    n = n(),
    min = min(AGE),
    max = max(AGE)
  )

# Categories: Age
df_covar <- df_covar %>%
  mutate(
    AGE3 = 
      ifelse(AGE  < 20,            0,
      ifelse(AGE >= 20 & AGE < 30, 1,
      ifelse(AGE >= 30,            2, NA)))
  )

df_covar <- df_covar %>%
  mutate(AGE3 = factor(AGE3,
    levels = c(0,1,2),
    labels = c("14-19","20-29","30-43")))

df_covar %>%
  check_discrete(AGE3)

##### Gestational Age ##########################################################
# Gestational Age
df_covar <- df_covar %>%
  mutate(SEGSTAGE = SEWKINT - BGLMPWK)

df_covar %>%
  count(SEGSTAGE)

# Categories: Gestational Age
# (Note: Variable was winsorized to limit influence of extreme values.)
df_covar <- df_covar %>%
  mutate(SEGSTAGE = ifelse(SEGSTAGE < 13, 13, SEGSTAGE)) %>%
  mutate(SEGSTAGE = ifelse(SEGSTAGE > 16, 16, SEGSTAGE))

df_covar <- df_covar %>%
  mutate(SEGSTAGE = factor(SEGSTAGE,
    levels = c(13,14,15,16),
    labels = c("11-13","14","15","16-17")))

df_covar %>%
  check_discrete(SEGSTAGE)

##### Parity ###################################################################
# Parity
df_covar <- df_covar %>%
  mutate(PARITY = ifelse(PARITY > 2, 2, PARITY))

# Categories: Parity
df_covar <- df_covar %>%
  mutate(PARITY = factor(PARITY, levels = c(0:2), 
    labels = c("Nulliparous","Primiparous","Multiparous")))

df_covar %>%
  check_discrete(PARITY)

##### Education ################################################################
# Education
df_covar <- df_covar %>%
  mutate(EDUCATION = ifelse(EDUCATION > 2, 2, EDUCATION))

# Categories: Education
df_covar <- df_covar %>%
  mutate(EDUCATION = factor(EDUCATION, levels = c(0:2), 
    labels = c("None","Class 1-9","Class ≥10")))

df_covar %>%
  check_discrete(EDUCATION)

##### Living Standards Index ###################################################
# Living Standards Index
df_covar %>%
  check_continuous(
    x = LSI, 
    xlab = "Living Standards Index", 
    title = "Living Standards Index"
  )

##### Mid-upper Arm Circumference (MUAC) #######################################
# Mid-upper Arm Circumference
df_covar %>%
  check_continuous(
    x = medSEMUAC,
    xlab = expression("Mid-upper Arm Circumference (MUAC)"),
    title = "Mid-upper Arm Circumference"
  )

##### Pesticide Use ############################################################
# Source Variables
df_covar %>%
  check_discrete(PEMIXPEST)

df_covar %>%
  check_discrete(PESPRAYPEST)

# Pesticide Use
df_covar <- df_covar %>%
  mutate(PESTICIDE = ifelse(PEMIXPEST == 1 | PESPRAYPEST == 1, 1, 0))

df_covar <- df_covar %>%
  mutate(PESTICIDE = factor(PESTICIDE, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PESTICIDE)

##### Chewing Tobacco ##########################################################
df_covar <- df_covar %>%
  mutate(PETOBAC = as.numeric(PETOBAC))

df_covar <- df_covar %>%
  mutate(PETOBAC = factor(PETOBAC, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PETOBAC)

##### Betel Nut ################################################################
df_covar <- df_covar %>%
  mutate(PEBETEL = factor(PEBETEL, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PEBETEL)

##### Husband's Smoking ########################################################
df_covar <- df_covar %>%
  mutate(PEHCIGAR = factor(PEHCIGAR, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PEHCIGAR)

##### Urinary Arsenobetaine ####################################################
df_covar %>%
  check_continuous(
    log(uAsB), 
    xlab = "Log(Urinary Arsenobetaine)", 
    title = "Urinary Arsenobetaine"
  )

##### Select and Arrange Data ##################################################
df_covar <- df_covar %>%
  select(UID, AGE, AGE3, SEGSTAGE, PARITY, EDUCATION, LSI, medSEMUAC, 
    PESTICIDE, PETOBAC, PEBETEL, PEHCIGAR, uAsB)

df_covar %>%
  sapply(function(x) sum(is.na(x)))

df_covar %>% head()
df_covar %>% dim()

