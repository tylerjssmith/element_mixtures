################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Identify Element Mixtures -- Figure 2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(scales)

##### Generate Figure ##########################################################
# Plot Estimates
quantreg_results_imp1 %>%
  ggplot(aes(x = factor(tau), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  facet_wrap(. ~ Element, scales = "free_y") +
  labs(
    x = "Percentile (%)",
    y = "Expected Difference (µg/L)") +
  th

tmp <- impt_complete_imp1 %>%
  group_by(As1) %>%
  summarise(
    p10 = quantile(Fe, 0.1),
    p30 = quantile(Fe, 0.3)) %>%
  pivot_longer(-As1) %>%
  rename(y = value) %>%
  mutate(yend = y) %>%
  mutate(across(c(y,yend), ~ log(.x))) %>%
  mutate(x = log(1)) %>%
  mutate(xend = ifelse(As1 == "1", log(0.006), ifelse(As1 == "0", log(620), NA))) %>%
  mutate(color = ifelse(name == "p10", "red", ifelse(name == "p30", "blue", NA)))

quantreg_results_imp1 %>%
  filter(Element == "Fe")

impt_complete_imp1 %>% 
  summarise(min = min(As), max = max(As))

tmp

impt_complete_imp1 %>%
  ggplot(aes(x = log(As), y = log(Fe))) +
  geom_vline(xintercept = log(1)) +
  geom_point(alpha = 0.4) +
  geom_segment(data = tmp, aes(x = x, xend = xend, y = y, yend = yend, color = color), inherit.aes = FALSE) +
  scale_x_continuous(limits = c(log(0.006),log(620))) +
  theme_bw()

