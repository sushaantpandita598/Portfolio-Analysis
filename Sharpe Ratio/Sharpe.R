## Sharpe Ratio in the xts world

rfr <- 0.06

sharpe_xts <-
  SharpeRatio(portfolio_returns_xts_rebalanced_monthly,
              Rf = rfr,
              FUN = "StdDev") %>%
  `colnames<-`("sharpe_xts")

sharpe_xts  #-0.5625


##Sharpe Ratio in the tidyquant

sharpe_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_performance(Ra = returns,
                 performance_fun = SharpeRatio,
                 Rf = rfr,
                 FUN = "StdDev") %>%
  `colnames<-`("sharpe_tq")

sharpe_tq  #-0.575


## We mow compare sharpe ratio of the Nifty 50 for the same time period 

market_returns_xts <-
  getSymbols("^NSEI",
             src = 'yahoo',
             from = "2018-12-31",
             to = "2023-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`("Nifty") %>%
  to.monthly(indexAt = "lastof",
             OHLC = FALSE) %>%
  Return.calculate(.,
                   method = "log") %>%
  na.omit()   ## Importing Nifty 50 data from yahoo


## Sharpe

market_returns_xts %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  mutate(returns =
           (log(Nifty) - log(lag(Nifty)))) %>%
  na.omit() %>%
  summarise(ratio =
              mean(returns - rfr)/sd(returns - rfr))

market_sharpe$ratio #-0.268

## Portfolio's sharpe ratio is -0.575, which is no better than market's sharpe ratio during this period.


## Visualizing Sharpe Ratio

sharpe_byhand_with_return_columns <-
  portfolio_returns_tq_rebalanced_monthly %>%
  mutate(ratio =
           mean(returns - rfr)/sd(returns - rfr)) %>%
  mutate(returns_below_rfr =
           if_else(returns < rfr, returns, as.numeric(NA))) %>%
  mutate(returns_above_rfr =
           if_else(returns > rfr, returns, as.numeric(NA))) %>%
  mutate_if(is.numeric, funs(round(.,4)))   ## We have calculated additional columns containg Returns above and below Rfr
                                            ## this helps us later in visualizing

sharpe_byhand_with_return_columns %>%
  head(3)

## Creating a scatter plot 

sharpe_byhand_with_return_columns %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = returns_below_rfr),
             colour = "red") +
  geom_point(aes(y = returns_above_rfr),
             colour = "green") +
  geom_vline(xintercept =
               as.numeric(as.Date("2020-03-25")),
             color = "blue") +
  geom_hline(yintercept = rfr,
             color = "purple",
             linetype = "dotted") +
  annotate(geom = "text",
           x = as.Date("2020-03-25"),
           y = -.04,
           label = "Covid",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1.5) +
  ylab("percent monthly returns") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_date(breaks = pretty_breaks( n = 8))


## Sharpe Histogram

sharpe_byhand_with_return_columns %>%
  ggplot(aes(x = returns)) +
  geom_histogram(alpha = 0.45,
                 binwidth = .01,
                 fill = "cornflowerblue") +
  geom_vline(xintercept = rfr,
             color = "green") +
  annotate(geom = "text",
           x = rfr,
           y = 13,
           label = "rfr",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1)


## Rolling Sharpe Ratio in the xts world

window <- 24

rolling_sharpe_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            window,
            function(x)
              SharpeRatio(x,
                          Rf = rfr,
                          FUN = "StdDev")) %>%
  na.omit() %>%
  `colnames<-`("xts")

## Rolling Sharpe of Market 
rolling_sharpe_market_xts <-
  rollapply(market_returns_xts,
            window,
            function(x)
              SharpeRatio(x,
                          Rf = rfr,
                          FUN = "StdDev")) %>%
  na.omit() %>%
  `colnames<-`("xts")


## Rolling Sharpe Ratio with tidyquant

sharpe_tq_roll <- function(df){
  SharpeRatio(df,
              Rf = rfr,
              FUN = "StdDev")}  
##first build a custom function where we can specify the RFR and an argument to the SharpeRatio() function

rolling_sharpe_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_mutate(
    select = returns,
    mutate_fun = rollapply,
    width = window,
    align = "right",
    FUN = sharpe_tq_roll,
    col_rename = "tq_sharpe"
  ) %>%
  na.omit()       ## we use tq_mutate() to wrap rollapply() and our custom function, and
                  ## apply them to portfolio_returns_tq_rebalanced_monthly


rolling_sharpe_tq
rolling_sharpe_xts


## Visualizing the Rolling Sharpe Ratio

## XTS Highcharter

highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month Sharpe") %>%
  hc_add_series(rolling_sharpe_xts,
                name = "sharpe",
                color = "blue") %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)

## Visualizing both market and portfolio rolling sharpe

highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month Sharpe") %>%
  hc_add_series(rolling_sharpe_xts,
                name = "Portfolio Sharpe",
                color = "blue") %>%
  hc_add_series(rolling_sharpe_market_xts,
                name = "Market Sharpe",
                color = "green") %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)


## Visualising using ggplot

rolling_sharpe_xts %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  rename(rolling_sharpe = xts) %>%
  ggplot(aes(x = date,
             y = rolling_sharpe)) +
  geom_line(color = "cornflowerblue") +
  ggtitle("Rolling 24-Month Sharpe Ratio") +
  labs(y = "rolling sharpe ratio") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))



