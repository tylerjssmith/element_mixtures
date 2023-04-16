################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identifying Element Mixtures -- Spearman's Correlations

# Tyler Smith
# April 7, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Prepare Data #############################################################
# Drinking Water Elements
df_water_impt_long <- df_water_impt %>%
  as_tibble() %>%
  select(-.id) %>%
  pivot_longer(
    cols = -c(.imp,UID),
    names_to = "Element",
    values_to = "Water"
  )

# Urinary Elements
df_urine_impt_long <- df_urine_impt %>%
  as_tibble() %>%
  select(-c(.id,SPECIFICGRAVITY)) %>%
  pivot_longer(
    cols = -c(.imp,UID),
    names_to = "Element",
    values_to = "Urine"
  )

df_water_impt_long %>% head()
df_urine_impt_long %>% head()

##### Estimate Spearman's Correlations #########################################
# Initialize Results Data Frame
df_figS9 <- data.frame()

# Loop Over 1 to ith Drinking Water Data Sets
# Loop Over 1 to jth Urinary Data Sets
for(i in 1:m) {
  
  i <- as.numeric(i)
  
  out_j <- data.frame()
  
  for(j in 1:m) {
    
    j <- as.numeric(j)
    
    tmp_water <- df_water_impt_long[df_water_impt_long$.imp == i,]
    tmp_urine <- df_urine_impt_long[df_urine_impt_long$.imp == j,]
    
    tmp_ij <- left_join(tmp_water, tmp_urine, 
      by = c("UID","Element"))
    
    tmp_ij <- tmp_ij %>%
      na.omit() %>%
      group_by(Element) %>%
      summarise(
        n = n(),
        rho = cor(Water, Urine, method = "spearman")
      )
    
    tmp_ij <- tmp_ij %>%
      mutate(i = i) %>%
      mutate(j = j)
    
    out_j <- rbind(out_j, tmp_ij)
    
  }
  
  df_figS9 <- rbind(df_figS9, out_j)
  
}

##### Summarize Results ########################################################
df_figS9 %>%
  group_by(Element) %>%
  summarise(
    n = n(),
    mean = mean(rho),
    min = min(rho),
    max = max(rho),
    range = max - min
  ) %>%
  arrange(desc(range))

