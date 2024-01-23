## Building a covariance matrix of returns

covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix,4)

sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)
sd_matrix_algebra_percent <-
  round(sd_matrix_algebra * 100, 2) %>%
  `colnames<-`("standard deviation")

sd_matrix_algebra_percent[1,]  #6.74


## Standard Deviation in the xts world

portfolio_sd_xts_builtin <-
  StdDev(asset_returns_xts, weights = w)  #We use StdDev from Performance Analytics

portfolio_sd_xts_builtin_percent <-
  round(portfolio_sd_xts_builtin * 100, 2)

portfolio_sd_xts_builtin_percent[1,1]
#6.74


## Standard Deviation in Tidyquant

portfolio_sd_tidyquant_builtin_percent <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_performance(Ra = returns,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Stdev) %>%
  mutate(tq_sd = round(Stdev, 4) * 100)  ##We specifically take out Sd from table.Stats function
#6.69




## Visualizing Standard Deviation

sd_plot <- sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot <- mean(portfolio_returns_tq_rebalanced_monthly$returns)



portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") +
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
             color = "purple",
             linetype = "dotted") +
  labs(title = "Colored Scatter with Line", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

## The above plot depicts returns 1 Sd above and 1 Sd below the mean.


## Visualizing asset vs portfolio Risk-Return

asset_returns_long %>%
  group_by(asset) %>%
  summarise(expected_return = mean(returns, na.rm = TRUE),
            stand_dev = sd(returns, na.rm = TRUE)) %>%   ##na.rm removes Null values from returns column
  add_row(asset = "Portfolio",
          stand_dev =
            sd(portfolio_returns_tq_rebalanced_monthly$returns),
          expected_return =
            mean(portfolio_returns_tq_rebalanced_monthly$returns)) %>%
  ggplot(aes(x = stand_dev,
             y = expected_return,
             color = asset)) +
  geom_point(size = 5) +
  geom_text(
    aes(x =
          sd(portfolio_returns_tq_rebalanced_monthly$returns)*1.11,
        y =
          mean(portfolio_returns_tq_rebalanced_monthly$returns),
        label = "Portfolio")) +
  ylab("expected return") +
  xlab("standard deviation") +
  ggtitle("Expected Monthly Returns versus Risk") +
  scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
  # The next line centers the title
  theme_update(plot.title = element_text(hjust = 0.5))




## Rolling Standard Deviation using Xts

window <- 24  ##using window width as 24 months


port_rolling_sd_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = sd,
            width = window) %>%
  na.omit() %>%
  `colnames<-`("rolling_sd")


tail(port_rolling_sd_xts)


## Rolling Standard Deviation in the tidyquant world

port_rolling_sd_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>%
  select(date, rolling_sd) %>%
  na.omit()


tail(port_rolling_sd_tq)



## Visualizing rolling sd using xts

port_rolling_sd_xts_hc <-
  round(port_rolling_sd_xts, 4) * 100 ##Rounding the % for easy charting



highchart(type = "stock") %>%
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc,
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"),
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled= TRUE) %>%
  hc_legend(enabled = TRUE)


##Visualizing Rolling Standard Deviation in the tidyverse

port_rolling_sd_tq %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = rolling_sd), color = "cornflowerblue") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Standard Deviation", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

