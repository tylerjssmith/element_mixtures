################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures among Pregnant Women in Rural Northern Bangladesh

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

# Urinary Arsenobetaine
urine <- urine %>%
  select(
    UID,
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

##### Prepare Data #############################################################
# Age
df_covar <- df_covar %>%
  mutate(AGE = year(SEDATE) - DOBYY)

df_covar %>%
  check_continuous(
    x = AGE, 
    xlab = "Age (years)", 
    title = "Age"
  )

# Gestational Age
df_covar <- df_covar %>%
  mutate(SEGSTAGE = SEWKINT - BGLMPWK)

df_covar %>%
  check_discrete(SEGSTAGE)

# Parity
df_covar <- df_covar %>%
  mutate(PARITY = ifelse(PARITY > 2, 2, PARITY))

df_covar <- df_covar %>%
  mutate(PARITY = factor(PARITY, levels = c(0:2), 
    labels = c("Nulliparous","Primiparous","Multiparous")))

df_covar %>%
  check_discrete(PARITY)

# Education
df_covar <- df_covar %>%
  mutate(EDUCATION = ifelse(EDUCATION > 2, 2, EDUCATION))

df_covar <- df_covar %>%
  mutate(EDUCATION = factor(EDUCATION, levels = c(0:2), 
    labels = c("None","Class 1-9","Class ≥10")))

df_covar %>%
  check_discrete(EDUCATION)

# Living Standards Index
df_covar %>%
  check_continuous(
    x = LSI, 
    xlab = "Living Standards Index", 
    title = "Living Standards Index"
  )

# Body Mass Index (BMI)
df_covar %>%
  check_continuous(
    x = SEBMI,
    xlab = expression("Body Mass Index (kg/m" ^ 2 * ")"),
    title = "Body Mass Index"
  )

# Mid-upper Arm Circumference (MUAC)
df_covar %>%
  check_continuous(
    x = medSEMUAC,
    xlab = expression("Mid-upper Arm Circumference (MUAC)"),
    title = "Mid-upper Arm Circumference"
  )

# Pesticide Use
df_covar <- df_covar %>%
  mutate(PESTICIDE = ifelse(PEMIXPEST == 1 | PESPRAYPEST == 1, 1, 0))

df_covar <- df_covar %>%
  mutate(PESTICIDE = factor(PESTICIDE, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PEMIXPEST)
df_covar %>%
  check_discrete(PESPRAYPEST)
df_covar %>%
  check_discrete(PESTICIDE)

# Chewing Tobacco
df_covar <- df_covar %>%
  mutate(PETOBAC = as.numeric(PETOBAC))

df_covar <- df_covar %>%
  mutate(PETOBAC = factor(PETOBAC, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PETOBAC)

# Betel Nut
df_covar <- df_covar %>%
  mutate(PEBETEL = factor(PEBETEL, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PEBETEL)

# Husband's Smoking
df_covar <- df_covar %>%
  mutate(PEHCIGAR = factor(PEHCIGAR, levels = c(0:1), 
    labels = c("No","Yes")))

df_covar %>%
  check_discrete(PEHCIGAR)

# Urinary Arsenobetaine
df_covar %>%
  check_continuous(
    log(uAsB), 
    xlab = "Log(Urinary Arsenobetaine)", 
    title = "Urinary Arsenobetaine"
  )

# Drinking Water Arsenic
df_covar %>%
  check_continuous(
    log(wAs), 
    xlab = "Log(Drinking Water Arsenic)", 
    title = "Drinking Water Arsenic"
  )

df_covar <- df_covar %>%
  mutate(wAs10 = ifelse(wAs > 10, 1, 0))

df_covar <- df_covar %>%
  mutate(wAs10 = factor(wAs10, levels = c(0:1), 
    labels = c("≤10",">10")))

df_covar %>%
  check_discrete(wAs10)

##### Select and Arrange Data ##################################################
df_covar <- df_covar %>%
  select(UID, AGE, SEGSTAGE, PARITY, EDUCATION, LSI, SEBMI, medSEMUAC,
    PESTICIDE, PETOBAC, PEBETEL, PEHCIGAR, uAsB, wAs10)

df_covar %>%
  sapply(function(x) sum(is.na(x)))

df_covar %>% head()


