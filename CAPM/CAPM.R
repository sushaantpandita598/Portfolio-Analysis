## 1.1 We imported prices of Nifty 50(^NSEI) earlier "market_return_xts"


market_returns_tidy <-
  market_returns_xts %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  na.omit() %>%
  select(date, returns = Nifty)

## Since we will be regressing portfolio returns on market returns, let’s ensure
## that the number of portfolio returns observations is equal to the number of
## market returns observations.


portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns = market_returns_tidy$returns) %>%
  head()

## Since it throws an error in mutate, it means we have to remove an extra observation and then check again

portfolio_returns_tq_rebalanced_monthly <- 
  slice(portfolio_returns_tq_rebalanced_monthly, -1) ##60 observation wef 01-31-2019

portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns = market_returns_tidy$returns) %>%
  head()


## 1.2 Calculating CAPM Beta

cov(portfolio_returns_xts_rebalanced_monthly,
    market_returns_tidy$returns)/
  var(market_returns_tidy$returns)  ##0.97

## 1.3 Beta using Xts

beta_builtin_xts <-
  CAPM.beta(portfolio_returns_xts_rebalanced_monthly,
            market_returns_xts)

beta_builtin_xts  ##0.97


## 1.4 Calculating CAPM Beta in the tidyquant

beta_builtin_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_return =
           market_returns_tidy$returns) %>%
  na.omit() %>%
  tq_performance(Ra = returns,
                 Rb = market_return,
                 performance_fun = CAPM.beta) %>%
  `colnames<-`("beta_tq")

beta_builtin_tq  ##0.97


##1.5 Visualizing CAPM using ggplot

portfolio_returns_tq_rebalanced_monthly %>%
  mutate(market_returns =
           market_returns_tidy$returns) %>%
  ggplot(aes(x = market_returns,
             y = returns)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "green") +
  ylab("portfolio returns") +
  xlab("market returns")


## 1.6 Visualizing CAPM in Highcharter
## Will need to understand Augment function first


