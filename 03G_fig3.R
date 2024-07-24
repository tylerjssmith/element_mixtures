################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure 3

# Tyler Smith
# April 23, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Fit Models ###############################################################
# Unadjusted
(df_fig3_unadj <- df_tbl1 %>%
  select(As,P,W,Fe,Si,Mn,Mo,Ba) %>%
  mutate(across(everything(), ~ log(.x))) %>%
  mutate(across(everything(), ~ scale(.x))) %>%
  map(~ lm(.x ~ EDUCATION, data = df_tbl1)) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "Element") %>%
  filter(grepl("EDUCATION",term)) %>%
  mutate(type = "Unadjusted") %>%
  select(Element, type, term, estimate, conf.low, conf.high, p.value))

# (Prepare Placeholders for Reference Group)
(df_fig3_unadj_ref <- tibble(
  Element = c("As","P","W","Fe","Si","Mn","Mo","Ba"),
  type = "Unadjusted",
  term = "EDUCATIONNone",
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  p.value = NA
))

# Adjusted
(df_fig3_adj <- df_tbl1 %>%
  select(As,P,W,Fe,Si,Mn,Mo,Ba) %>%
  mutate(across(everything(), ~ log(.x))) %>%
  mutate(across(everything(), ~ scale(.x))) %>%
  map(~ lm(.x ~ EDUCATION + AGE + SEGSTAGE + PARITY + LSI + medSEMUAC + 
      PESTICIDE + PETOBAC + PEBETEL + PEHCIGAR, data = df_tbl1)) %>%
  map_dfr(tidy, conf.int = TRUE, .id = "Element") %>%
  filter(grepl("EDUCATION",term)) %>%
  mutate(type = "Adjusted") %>%
  select(Element, type, term, estimate, conf.low, conf.high, p.value))

# (Prepare Placeholders for Reference Group)
(df_fig3_adj_ref <- tibble(
  Element = c("As","P","W","Fe","Si","Mn","Mo","Ba"),
  type = "Adjusted",
  term = "EDUCATIONNone",
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  p.value = NA
))

# Combine Estimates
(df_fig3 <- rbind(
  df_fig3_unadj,
  df_fig3_unadj_ref,
  df_fig3_adj,
  df_fig3_adj_ref)
)

rm(list = c("df_fig3_unadj","df_fig3_unadj_ref","df_fig3_adj","df_fig3_adj_ref"))

# Label Variables
df_fig3 <- df_fig3 %>%
  mutate(type = factor(type,
    levels = c("Unadjusted","Adjusted")))

df_fig3 <- df_fig3 %>%
  mutate(term = factor(term, 
    levels = c("EDUCATIONNone","EDUCATIONClass 1-9","EDUCATIONClass ≥10"),
    labels = c("None (Ref)","1-9","≥10"))) 

##### Generate Figure ##########################################################
(fig3 <- df_fig3 %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high,
    color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +
  scale_y_continuous(limits = c(-1,0.5), breaks = seq(-2,2,0.5)) +
  facet_wrap(. ~ Element, nrow = 3) +
  labs(
    x = "Education (Class)",
    y = "Expected Difference (z[ln(µg/L)])\n(95% Confidence Interval)",
    color = "Model") +
  th)

##### Get Results ##############################################################
df_fig3 %>%
  filter(type == "Adjusted") %>%
  filter(term == "≥10") %>%
  arrange(estimate)

df_fig3 %>%
  filter(Element %in% c("As","W","Fe")) %>%
  filter(type == "Adjusted") %>%
  filter(term == "≥10")


