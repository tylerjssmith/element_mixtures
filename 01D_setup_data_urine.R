################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Urinary Elements

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

##### Read Data ################################################################
umt <- read_csv("assay_urinary_metals/pair_urinarymetals_2022_1029.csv")
uas <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")

# Reset Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_mixtures/code/")

##### Select Data ##############################################################
umt <- umt %>%
  select(
    UID,
    ends_with("_SG"),
    ends_with("_LTLOD")
  )

umt %>%
  select(PE_uMetals_

umt <- umt %>%
  select(
    UID,
    starts_with("PE_uMetals_Al"),
    starts_with("PE_uMetals_Ba"),
    starts_with("PE_uMetals_Br"),
    starts_with("PE_uMetals_Ca"),
    starts_with("PE_uMetals_Fe"),
    starts_with("PE_uMetals_Mn"),
    starts_with("PE_uMetals_Mo"),
    starts_with("PE_uMetals_P"),
    starts_with("PE_uMetals_S"),
    starts_with("PE_uMetals_Sb"),
    starts_with("PE_uMetals_Si"),
    starts_with("PE_uMetals_Sr"),
    starts_with("PE_uMetals_U"),
    starts_with("PE_uMetals_V"),
    starts_with("PE_uMetals_W")
  )

umt %>%
  select(
    -starts_with("PE_uMetals_Pb")
  )

