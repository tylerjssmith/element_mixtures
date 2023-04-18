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
(fig3 <- df_fig3 %>%
  tibble() %>%
  mutate(tau = as.numeric(tau) * 100) %>%
  ggplot() +
  geom_ribbon(aes(x = tau, y = mean, ymin = lb, ymax = ub), fill = "lightgray") +
  geom_line(aes(x = tau, y = mean), inherit.aes = FALSE) +
  geom_point(aes(x = tau, y = mean), inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(10,90,20)) +
  facet_wrap(. ~ y, scales = "free_y", ncol = 3, nrow = 5) +
  labs(
    x = "Percentile",
    y = "Expected Difference (µg/L)") +
  th)
