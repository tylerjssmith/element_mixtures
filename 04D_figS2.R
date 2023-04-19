################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure S2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Figure ##########################################################
(df_figS2 <- df_urine_llod_ind %>%
  group_by(Element) %>%
  count(Indicator) %>%
  mutate(p = n / sum(n) * 100) %>%
  arrange(Element, desc(Indicator)) %>%
  slice_head() %>%
  mutate(p = ifelse(Indicator == 0, 0, p)) %>%
  ungroup())

(df_figS2_as <- tibble(Element = "As", Indicator = 0, n = 778, p = 0))

(df_figS2 <- bind_rows(df_figS2, df_figS2_as))

rm(df_figS2_as)

df_figS2 %>%
  mutate(category = 
      ifelse(p == 0,           "0", 
      ifelse(p > 0  & p <= 10, ">0-10", 
      ifelse(p > 10 & p <= 30, ">10-30", 
      ifelse(p > 30 & p <= 50, ">30-50", 
      ifelse(p > 50,           ">50", NA)))))
  ) %>%
  arrange(desc(p), Element)

(figS2 <- df_figS2 %>%
  ggplot(aes(x = Element, y = p)) +
  geom_hline(yintercept = 50, color = "red") +
  geom_bar(stat = "identity", fill = "lightgray") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(
    x = "Element",
    y = "<LLOD (%)") +
  th)
