################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure S1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
df_figS1 <- df_water_llod_ind %>%
  group_by(Element) %>%
  count(Indicator) %>%
  mutate(p = n / sum(n) * 100) %>%
  arrange(Element, desc(Indicator)) %>%
  slice_head() %>%
  mutate(p = ifelse(Indicator == 0, 0, p)) %>%
  ungroup()

df_figS1 %>%
  mutate(category = 
      ifelse(p == 0,           "0", 
      ifelse(p > 0  & p <= 10, ">0-10", 
      ifelse(p > 10 & p <= 30, ">10-30", 
      ifelse(p > 30 & p <= 50, ">30-50", 
      ifelse(p > 50,           ">50", NA)))))
  ) %>%
  arrange(p, Element)

(figS1 <- df_figS1 %>%
  ggplot(aes(x = Element, y = p)) +
  geom_hline(yintercept = 50, color = "red") +
  geom_bar(stat = "identity", fill = "lightgray") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(
    title = "Drinking Water Elements <LLOD",
    x = "Element",
    y = "<LLOD (%)") +
  th)
