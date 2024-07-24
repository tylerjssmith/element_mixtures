################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure 3

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Fit Models ###############################################################
# Prepare Data
df_tmp_fig4 <- df_tbl2 %>%
  select(W,Mo,As) %>%
  mutate(across(everything(), ~ log(.x))) %>%
  mutate(across(everything(), ~ scale(.x)))

df_tmp_fig4 %>% head()

# Fit Models: Unadjusted
# (Chewing Tobacco)
df_tmp_fig4_unaj_petobac <- df_tmp_fig4 %>%
  map(~ lm(.x ~ PETOBAC, data = df_tbl2)) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "Element") %>%
  mutate(type = "Unadjusted") %>%
  filter(grepl("PETOBAC",term))

df_tmp_fig4_unaj_petobac %>% head()

# (Betel Nut)
df_tmp_fig4_unaj_pebetel <- df_tmp_fig4 %>%
  map(~ lm(.x ~ PEBETEL, data = df_tbl2)) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "Element") %>%
  mutate(type = "Unadjusted") %>%
  filter(grepl("PEBETEL",term))

df_tmp_fig4_unaj_pebetel %>% head()

# (Husband's Smoking)
df_tmp_fig4_unaj_pehcigar <- df_tmp_fig4 %>%
  map(~ lm(.x ~ PEHCIGAR, data = df_tbl2)) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "Element") %>%
  mutate(type = "Unadjusted") %>%
  filter(grepl("PEHCIGAR",term))

df_tmp_fig4_unaj_pehcigar %>% head()

# Fit Models: Adjusted
df_tmp_fig4_adju <- df_tmp_fig4 %>%
  map(~ lm(.x ~ AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + 
    PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_tbl2)) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "Element") %>%
  mutate(type = "Adjusted")

df_tmp_fig4_adju %>% head()

# Combine Estimates
df_fig4 <- rbind(
  df_tmp_fig4_unaj_petobac,
  df_tmp_fig4_unaj_pebetel,
  df_tmp_fig4_unaj_pehcigar,
  df_tmp_fig4_adju %>% filter(grepl("PETOBAC", term)),
  df_tmp_fig4_adju %>% filter(grepl("PEBETEL", term)),
  df_tmp_fig4_adju %>% filter(grepl("PEHCIGAR",term))
)

df_fig4 <- df_fig4 %>%
  select(Element, type, term, estimate, conf.low, conf.high, p.value)

df_fig4 %>% head()

# Make Placeholders for Reference Categories
df_fig4_ref <- tibble(
  Element = df_fig4$Element,
  type = df_fig4$type,
  term = df_fig4$term,
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  p.value = NA
)

df_fig4_ref <- df_fig4_ref %>%
  mutate(term = gsub("Yes","No",term))

df_fig4_ref %>% head()

# Combine Estimates and Placeholders
df_fig4 <- rbind(
  df_fig4,
  df_fig4_ref
)

df_fig4 %>% head()

rm(list = c("df_tmp_fig4","df_tmp_fig4_unaj_petobac","df_tmp_fig4_unaj_pebetel",
  "df_tmp_fig4_unaj_pehcigar","df_tmp_fig4_adju","df_fig4_ref"))

# Prepare Variables
# (Models)
df_fig4 <- df_fig4 %>%
  mutate(type = factor(type,
    levels = c("Unadjusted","Adjusted")))

# (Values)
df_fig4 <- df_fig4 %>%
  mutate(value = ifelse(grepl("Yes",term), "Yes", "No (Ref)")) %>%
  mutate(term = gsub("Yes", "", term)) %>%
  mutate(term = gsub("No", "", term))

# (Variables)
df_fig4 <- df_fig4 %>%
  mutate(term = factor(term,
    levels = c("PETOBAC","PEBETEL","PEHCIGAR"),
    labels = c("Chewing Tobacco","Betel Nut","Husband Smokes")
  ))

df_fig4 %>% head()

##### Generate Figure ##########################################################
(fig4 <- df_fig4 %>%
  ggplot(aes(x = value, y = estimate, ymin = conf.low, ymax = conf.high,
    color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +
  scale_y_continuous(breaks = seq(-5,5,0.2), limits = c(-0.9,0.4)) +
  facet_grid(term ~ Element, scales = "free_x") +
  labs(
    x = "Behavior",
    y = "Expected Difference (z[ln(µg/L)])\n(95% Confidence Interval)",
    color = "Model") +
  th)
  
df_fig4 %>%
  filter(type == "Adjusted") %>%
  filter(value == "Yes")
  

