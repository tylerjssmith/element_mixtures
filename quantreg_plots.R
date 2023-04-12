
df_water_impt1 %>% colnames()
df_water_impt1

df_water_impt1 <- df_water_impt1 %>% mutate(across(Al:W, ~ log(.x)))

plot_qr_As10 <- function(tau = seq(0.1,0.9,0.1))
{
  # Quantile Regression
  fit_qr <- rq(Fe ~ As10, data = df_water_impt1, tau = tau)
  fit_qr <- tidy(fit_qr, se.type = "boot", R = 1000)
  fit_qr <- fit_qr %>%
    mutate(conf.low  = estimate - 1.96 * std.error) %>%
    mutate(conf.high = estimate + 1.96 * std.error)
  fit_qr <- fit_qr %>%
    filter(term == "As10")
  fit_qr <- fit_qr %>%
    mutate(tau = tau * 100)

  # Linear Regression
  fit_lm <- lm(Fe ~ As10, data = df_water_impt1)
  fit_lm <- tidy(fit_lm, conf.int = TRUE)
  fit_lm <- fit_lm %>% filter(term == "As10")

  y_limits <- c(floor(min(fit_qr$conf.low)),ceiling(max(fit_qr$conf.high)))
  
  # Plot
  fit_qr %>%
    ggplot() +
    geom_hline(data = fit_lm, aes(yintercept = conf.low), linetype = "dashed", 
      inherit.aes = FALSE) +
    geom_hline(data = fit_lm, aes(yintercept = estimate)) +
    geom_hline(data = fit_lm, aes(yintercept = conf.high), 
      linetype = "dashed") +
    geom_ribbon(aes(x = tau, y = estimate, ymin = conf.low, ymax = conf.high), fill = "#ebebeb") +
    geom_line(aes(x = tau, y = estimate), inherit.aes = FALSE) +
    geom_point(aes(x = tau, y = estimate)) +
    scale_x_continuous(breaks = seq(10,90,20)) +
    scale_y_continuous(limits = y_limits) +
    labs(
      x = "Quantile",
      y = "Iron") +
    th + theme(panel.grid = element_blank())
}

plot_qr_As10(tau = seq(0.1,0.9,0.1))


