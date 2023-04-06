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
    SEWKINT
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
    PETOBAC,
    PEBETEL,
    PEHCIGAR
  )

##### Join Data ################################################################
df_covar <- left_join(pregtrak, pefsst, by = "UID")
df_covar <- left_join(df_covar, parity, by = "UID")
df_covar <- left_join(df_covar, ses, by = "UID")
df_covar <- left_join(df_covar, pef, by = "UID")

# Remove Source Data
rm(list = c("pregtrak","pefsst","parity","ses","pef"))

##### Prepare Data #############################################################
# Age
df_covar <- df_covar %>%
  mutate(AGE = year(SEDATE) - DOBYY)

# Gestational Age
df_covar <- df_covar %>%
  mutate(SEGSTAGE = SEWKINT - BGLMPWK)

# Parity
df_covar <- df_covar %>%
  mutate(PARITY = ifelse(PARITY > 2, 2, PARITY))

# Education
df_covar <- df_covar %>%
  mutate(EDUCATION = ifelse(EDUCATION > 2, 2, EDUCATION))

# Living Standards Index

# Maternal Occupation

# Paternal Occupation

# Chewing Tobacco
df_covar <- df_covar %>%
  mutate(PETOBAC = as.numeric(PETOBAC))

# Betel Nut

# Husband's Smoking

##### Select and Arrange Data ##################################################
df_covar <- df_covar %>%
  select(UID, AGE, SEGSTAGE, PARITY, EDUCATION, LSI, PETOBAC, PEBETEL, PEHCIGAR)

df_covar %>% head()


