## Skewness in the xts world

skew_xts <-
  skewness(portfolio_returns_xts_rebalanced_monthly$returns)

skew_xts   #-1.036

## Skewness in tidyerse

skew_tidy <-
  portfolio_returns_tq_rebalanced_monthly %>%
  summarise(skew_builtin = skewness(returns),
            skew_byhand =
              (sum((returns - mean(returns))^3)/length(returns))/
              ((sum((returns - mean(returns))^2)/length(returns)))^(3/2)) %>%
  select(skew_builtin, skew_byhand)

skew_tidy   ##-1.03 



## Visualizing Skewness
portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(alpha = .7,
                 binwidth = .003,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  scale_x_continuous(breaks =
                       pretty_breaks(n = 10))


## Highlighting key drivers for skewness 
## Using returns that fall 2sd below the mean

portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red =
           if_else(returns < (mean(returns) - 2*sd(returns)),
                   returns, as.numeric(NA)),
         returns =
           if_else(returns > (mean(returns) - 2*sd(returns)),
                   returns, as.numeric(NA))) %>%
  ggplot() +
  geom_histogram(aes(x = hist_col_red),
                 alpha = .7,
                 binwidth = .003,
                 fill = "red",
                 color = "red") +
  geom_histogram(aes(x = returns),
                 alpha = .7,
                 binwidth = .003,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("monthly returns")


## Density Plot


portfolio_density_plot <-
  portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  stat_density(geom = "line",
               alpha = 1,
               colour = "cornflowerblue")

portfolio_density_plot ## Creates a simple density plot


## Shading area falling below mean

shaded_area_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x <
           mean(portfolio_returns_tq_rebalanced_monthly$returns))

portfolio_density_plot_shaded <-
  portfolio_density_plot +
  geom_area(data = shaded_area_data,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5)

portfolio_density_plot_shaded


## Adding mean and median lines to the density plot

median <-
  median(portfolio_returns_tq_rebalanced_monthly$returns)
mean <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

median_line_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x <= median)

portfolio_density_plot_shaded +
  geom_segment(data = shaded_area_data,
               aes(x = mean,
                   y = 0,
                   xend = mean,
                   yend = density),
               color = "red",
               linetype = "dotted") +
  annotate(geom = "text",
           x = mean,
           y = 5,
           label = "mean",
           color = "red",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = -1.75) +
  geom_segment(data = median_line_data,
               aes(x = median,
                   y = 0,
                   xend = median,
                   yend = density),
               color = "black",
               linetype = "dotted") +
  annotate(geom = "text",
           x = median,
           y = 5,
           label = "median",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = 1.75) +
  ggtitle("Density Plot Illustrating Skewness")



## Portfolio vs Individual asset skewness

asset_returns_long %>%
  summarize(skew_assets = skewness(returns)) %>%
  add_row(asset = "Portfolio",
          skew_assets = skew_tidy$skew_byhand)%>%
  ggplot(aes(x = asset,
             y = skew_assets,
             colour = asset)) +
  geom_point() +
  geom_text(
    aes(x = "Portfolio",
        y =
          skew_tidy$skew_builtin + .04),
    label = "Portfolio",
    color = "cornflowerblue") +
  labs(y = "skewness")




## Rolling skewness in Xts
window <- 24

rolling_skew_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = skewness,
            width = window) %>%
  na.omit()


## Rolling Skewness in the tidyquant world

rolling_skew_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_mutate(select = returns,
            mutate_fun = rollapply,
            width = window,
            FUN = skewness,
            col_rename = "tq") %>%
  na.omit()



## Vizualising Rolling Skewness

highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month Skewness") %>%
  hc_add_series(rolling_skew_xts,
                name = "Rolling skewness",
                color = "cornflowerblue") %>%
  hc_yAxis(title = list(text = "skewness"),
           opposite = FALSE,
           max = 2,
           min = -2) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)    ###Using Highcharter in XTS


rolling_skew_tq %>%
  ggplot(aes(x = date, y = tq)) +
  geom_line(color = "cornflowerblue") +
  ggtitle("Rolling 24-Month Skew ") +
  ylab(paste("Rolling ", window, " month skewness",
             sep = " ")) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = pretty_breaks(n = 8)) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme_update(plot.title = element_text(hjust = 0.5))   ## Using ggplot


