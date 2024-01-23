## Our portfolio will have the following asset mix

# BEL.NS weighted 15%
# IOC.NS weighted 15%
# TVSMOTOR.NS weighted 15%
# TATAPOWER.NS weighted 15%
# TRENT.NS weighted 10%
# BAJAJHLDNG.NS weighted 15%
# ZYDUSLIFE.NS weighted 15%

w <- c(0.15,
       0.15,
       0.15,
       0.15,
       0.10,
       0.15,
       0.15)

## Re-checking the weights

tibble(w, symbols)

tibble(w, symbols) %>%
  summarise(total_weight = sum(w))  ## Returns 1, which means weights have been correctly assigned


## Portfolio returns by hand
## assigning weights to assets as per the vector 'w'

w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]
w_6 <- w[6]
w_7 <- w[7]

## Assigning returns from columns in asset_returns_xts

asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]
asset6 <- asset_returns_xts[,6]
asset7 <- asset_returns_xts[,7]

portfolio_returns_byhand <-
  (w_1 * asset1) +
  (w_2 * asset2) +
  (w_3 * asset3) +
  (w_4 * asset4) +
  (w_5 * asset5) +
  (w_6 * asset6) +
  (w_7 * asset7) 

names(portfolio_returns_byhand) <- "returns" ##Changing the name of column containing returns


## Portfolio Returns in the xts world

portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")

head(portfolio_returns_xts_rebalanced_monthly)


## Portfolio Returns in the tidyverse

asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5],
                             asset == symbols[6] ~ w[6],
                             asset == symbols[7] ~ w[7])) %>%
  head()


## Portfolio Returns in the tidyquant world

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")


###  Visualizing Portfolio Returns in the xts world

highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)



## Histogram 

hc_portfolio <-
  hist(portfolio_returns_xts_rebalanced_monthly$returns,
       breaks = 50,
       plot = FALSE)

hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
  hc_title(text = "Portfolio Returns Distribution") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)




## Visualizing Portfolio Returns in the tidyverse

## ScatterPlot

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(colour = "cornflowerblue")+
  xlab("date") +
  ylab("monthly return") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Returns Scatter") +
  scale_x_date(breaks = pretty_breaks(n=6))



portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .01,
                 colour = "cornflowerblue",
                 fill = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Histogram and Density")



