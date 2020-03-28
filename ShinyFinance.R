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
library(purrr)


symbols = c("SPY","EFA","IJS","EEM","AGG")

prices = getSymbols(symbols,
                    src = 'yahoo',
                    from = "2012-12-31",
                    to = "2019-12-31",
                    auto.assign = TRUE,
                    warnings = FALSE) %>% map(~Ad(get(.))) %>% reduce(merge) %>% 'colnames<-'(symbols)


head(prices,3)

# ----------------------- XTS -------------------------------------
prices_monthly = to.monthly(prices,indexAt = "lastof",OHLC = FALSE)
head(prices_monthly,3)

asset_returns_xts = Return.calculate(prices_monthly,method = "log") %>% na.omit()
head(asset_returns_xts,3)
index(asset_returns_xts)

highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log Returns") %>% 
  hc_add_series(asset_returns_xts[,symbols[1]],name = symbols[1]) %>%
  hc_add_series(asset_returns_xts[,symbols[2]],name = symbols[2]) %>%
  hc_add_series(asset_returns_xts[,symbols[3]],name = symbols[3]) %>%
  hc_add_series(asset_returns_xts[,symbols[4]],name = symbols[4]) %>%
  hc_add_series(asset_returns_xts[,symbols[5]],name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

hc_hist = hist(asset_returns_xts[,symbols[1]],breaks = 50, plot = FALSE)
hchart(hc_hist,color = "cornflowerblue") %>% hc_title(text = paste(symbols[1],"Log Returns Distribution",sep = " ")) %>% 
  hc_add_theme(hc_theme_flat()) %>% hc_exporting(enabled = TRUE) %>% hc_legend(enabled = FALSE)

hc_hist_fun  = function(n = 1,object,color) {
  hc_hist = hist(object[,symbols[n]], breaks = 50, plot = FALSE)
  hchart(hc_hist,color = color) %>% hc_title(text = paste(symbols[n],"Log Returns Distribution",sep = " ")) %>% 
  hc_add_theme(hc_theme_flat()) %>% hc_exporting(enabled = TRUE) %>% hc_legend(enabled = FALSE)
}

hc_hist_fun(1,asset_returns_xts,"cornflowerblue")
hc_hist_fun(2, asset_returns_xts, "green")
hc_hist_fun(3, asset_returns_xts, "pink")
hc_hist_fun(4, asset_returns_xts, "purple")
hc_hist_fun(5, asset_returns_xts, "yellow")

map(1:5,hc_hist_fun,asset_returns_xts,"blue")

portfolio_returns_xts_rebalanced_monthly = Return.portfolio(asset_returns_xts,weights = w, rebalance_on = "months") %>% 'colnames<-'("returns")
head(portfolio_returns_xts_rebalanced_monthly,3)
# -----------------------------------------------------------------

# ----------------------- Tidyverse -------------------------------
asset_returns_dplyr_byhand = prices %>% to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  data.frame(date = index(.)) %>% remove_rownames() %>% gather(asset,prices,-date) %>%  
  group_by(asset) %>% mutate(returns=(log(prices) - log(lag(prices)))) %>% select(-prices) %>% spread(asset,returns) %>% 
  select(date,symbols) %>% na.omit()

head(asset_returns_dplyr_byhand,3)
asset_returns_long = asset_returns_dplyr_byhand %>% gather(asset,returns,-date) %>% group_by(asset)
tail(asset_returns_long,3)

asset_returns_long %>% ggplot(aes(x=returns,fill = asset)) + geom_histogram(alpha = 0.45,binwidth = 0.005) + ggtitle("Monthly Retruns Since 2013")
asset_returns_long %>% ggplot(aes(x=returns,fill = asset)) + geom_histogram(alpha = 0.45,binwidth = 0.005) + facet_wrap(~asset) + ggtitle("Monthly Retruns Since 2013") + theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>% ggplot(aes(x=returns,colour = asset)) + geom_density(alpha = 1) + ggtitle("Monthly Returns Density Since 2013") + 
  xlab("monthly returns") + ylab("distribution") + theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>% ggplot(aes(x = returns)) + geom_density(aes(color = asset), alpha = 1) + 
  geom_histogram(aes(fill=asset),alpha = 0.45,bindwidth = 0.01) + guides(fill = FALSE) + 
  facet_wrap(~asset) + ggtitle("Monthly Returns Since 2013") + xlab("monthly returns") + ylab("distribution") + 
  theme_update(plot.title = element_text(hjust = 0.5))

# ------------------------------------------------------------------

# ----------------------- Tidyquant --------------------------------
asset_returns_tq_builtin = prices %>% tk_tbl(preserve_index = TRUE,rename_index = "date") %>% 
  gather(asset,prices,-date) %>% group_by(asset) %>% tq_transmute(mutate_fun = periodReturn,period = "monthly",
                                                                  type = "log") %>% spread(asset,monthly.returns) %>% 
                                                                    select(date,symbols) %>% slice(-1)

head(asset_returns_tq_builtin,3)
# -------------------------------------------------------------------

#------------------------- tibbletime -------------------------------

asset_returns_tbltime = prices %>% tk_tbl(preserve_index = TRUE,rename_index = "date") %>% 
  as_tbl_time(index=date) %>% as_period(period = "month",side = "end") %>% gather(asset,returns,-date) %>% 
  group_by(asset) %>% tq_transmute(mutate_fun = periodReturn,type = "log") %>% spread(asset,monthly.returns) %>% 
  select(date,symbols) %>% slice(-1)

head(asset_returns_tbltime,3)


# ------------------------- Making of Portfolio ---------------------
w = c(0.25,0.25,0.2,0.2,0.1)
tibble(w,symbols)

tibble(w,symbols) %>% summarise(total_weight = sum(w))

w_1 = w[1]
w_2 = w[2]
w_3 = w[3]
w_4 = w[4]
w_5 = w[5]

asset1 = asset_returns_xts[,1]
asset2 = asset_returns_xts[,2]
asset3 = asset_returns_xts[,3]
asset4 = asset_returns_xts[,4]
asset5 = asset_returns_xts[,5]

portfolio_returns_byhand = (w_1*asset1) + (w_2*asset2) + (w_3*asset3) + (w_4*asset4) + (w_5*asset5) 
names(portfolio_returns_byhand) = "returns"

portfolio_returns_byhand















