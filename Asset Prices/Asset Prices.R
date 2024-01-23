## Our portfolio consists of 8 companiesfrom Nifty Next 50 index
## Bharat Electronics Ltd.(BEL.NS),
## Indian Oil Corporation Ltd.(IOC.NS), TVS Motor Company Ltd.(TVSMOTOR.NS),Tata Power Co. Ltd.(TATAPOWER.NS),
## Trent Ltd.(TRENT.NS),Bajaj Holdings & Investment Ltd.(BAJAJHLDNG.NS) & Zydus Lifesciences Ltd.(ZYDUSLIFE.NS)
## Portfolio is equally weighted for all assets.

# The following packages are installed 
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")


library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)


symbols <- c("BEL.NS","IOC.NS","TVSMOTOR.NS","TATAPOWER.NS","TRENT.NS","BAJAJHLDNG.NS","ZYDUSLIFE.NS")

## We now use getSymbols() function from the quantmod package return an object with the opening price,
## closing price, adjusted price, daily high, daily low and daily volume for each ticker.

prices <-
  getSymbols(symbols,
             src = 'yahoo',
             from = "2018-12-31",
             to = "2023-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)


head(prices)

## Converting Daily Prices to Monthly Returns in the xts world.
## Converting daily prices into monthly log returns

prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)

head(prices_monthly)

## We now calculate log returns from the monthly asset prices

asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method = "log") %>%
  na.omit()


head(asset_returns_xts)



## Converting Daily Prices to Monthly Returns in the tidyverse

asset_returns_dplyr_byhand <-
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>% 
# now remove the index because it got converted to row names
remove_rownames() %>% 
  gather(asset, prices, -date) %>% 
group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>% 
select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols)


head(asset_returns_dplyr_byhand,3)

asset_returns_dplyr_byhand <-  
  asset_returns_dplyr_byhand %>%
  na.omit()

## Tidyverse format
asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)





head(asset_returns_dplyr_byhand,4)
head(asset_returns_xts,4)


## Visualizing Asset Returns in the xts world

highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(asset_returns_xts[, symbols[1]],
                name = symbols[1]) %>%
  hc_add_series(asset_returns_xts[, symbols[2]],
                name = symbols[2]) %>%
  hc_add_series(asset_returns_xts[, symbols[3]],
                name = symbols[3]) %>%
  hc_add_series(asset_returns_xts[, symbols[4]],
                name = symbols[4]) %>% 
  hc_add_series(asset_returns_xts[, symbols[5]],
                name = symbols[5]) %>%
  hc_add_series(asset_returns_xts[, symbols[6]],
                name = symbols[6]) %>%
  hc_add_series(asset_returns_xts[, symbols[7]],
                name = symbols[7]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)


## Histograms

hc_hist <- hist(asset_returns_xts[, symbols[1]],
                breaks = 50,
                plot = FALSE)

hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text =
             paste(symbols[1],
                   "Log Returns Distribution",
                   sep = " ")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)


## Visualizing Asset Returns in the tidyverse

asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 2, binwidth = .05) +
  ggtitle("Monthly Returns Since 2019")


asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 1, binwidth = .05) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2019") +
  theme_update(plot.title = element_text(hjust = 0.5))


## Density line


asset_returns_long %>%
ggplot(aes(x = returns, colour = asset)) +
  geom_density(alpha = 1) +
  ggtitle("Monthly Returns Density Since 2019") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

## Combining histogram and density

asset_returns_long %>%
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha = 0.5) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2019") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))



