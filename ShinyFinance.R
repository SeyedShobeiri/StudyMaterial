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

highchart(type="stock") %>% hc_title(text = "Portfolio Monthly Returns") %>% hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                                                                                           name = "Rebalanced Monthly",
                                                                                           color = "cornflowerblue") %>% 
  hc_add_theme(hc_theme_flat()) %>% hc_navigator(enabled = FALSE) %>% hc_scrollbar(enabled = FALSE) %>% hc_legend(enabled = TRUE) %>% hc_exporting(enabled = TRUE)

hc_portfolio = hist(portfolio_returns_xts_rebalanced_monthly$returns, breaks = 50,plot = FALSE)
hchart(hc_portfolio,color = "cornflowerblue",name = "Portfolio") %>% hc_title(text = "Portfolio Returns Distribution") %>% 
  hc_add_theme(hc_theme_flat()) %>% hc_exporting(enabled = TRUE)

portfolio_sd_xts_builtin = StdDev(asset_returns_xts,weights = w)
portfolio_sd_xts_builtin_percent = round(portfolio_sd_xts_builtin*100,2)
portfolio_sd_xts_builtin_percent[1,1]



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

asset_returns_long %>% group_by(asset) %>% mutate(weights = case_when(asset == symbols[1] ~ w[1],
                                                                      asset == symbols[2] ~w[2],
                                                                      asset == symbols[3] ~w[3],
                                                                      asset == symbols[4] ~w[4],
                                                                      asset == symbols[5] ~ w[5])) %>% head(3)

portfolio_returns_dplyr_byhand = asset_returns_long %>% group_by(asset) %>% mutate(weights = case_when(asset == symbols[1] ~ w[1],
                                                                                                       asset == symbols[2] ~w[2],
                                                                                                       asset == symbols[3] ~w[3],
                                                                                                       asset == symbols[4] ~w[4],
                                                                                                       asset == symbols[5] ~ w[5]),
                                                                                                       weighted_returns = returns * weights) %>% 
                                                                                     group_by(date) %>% 
                                                                                     summarise(returns = sum(weighted_returns))
head(portfolio_returns_dplyr_byhand,3)

portfolio_returns_tq_rebalanced_monthly %>% ggplot(aes(x=date,y=returns)) + geom_point(colour = "cornflowerblue") +
  xlab("date") + ylab("monthly return") + theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Returns Scatter") + scale_x_date(breaks = pretty_breaks(n=6))

portfolio_returns_tq_rebalanced_monthly %>% ggplot(aes(x=returns)) + geom_histogram(binwidth = 0.005,
                                                                                    fill = "cornflowerblue",
                                                                                    color = "cornflowerblue") +
  ggtitle("Portfolio Returns Distribution") + theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>% ggplot(aes(x=returns,fill = asset)) + geom_histogram(alpha=0.15,binwidth = 0.01) + 
  geom_histogram(data = portfolio_returns_tq_rebalanced_monthly,fill = "cornflowerblue", binwidth = 0.01) +
  ggtitle("Portfolio and Asset Monthly Returns") + theme_update(plot.title = element_text(hjust = 0.5))

portfolio_returns_tq_rebalanced_monthly %>% ggplot(aes(x=returns)) + geom_histogram(binwidth = 0.01,colour = "cornflowerblue",
                                                                                    fill = "cornflowerblue")+
  geom_density(alpha = 1,color = "red") +xlab("monthly returns") + ylab("distribution") + theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Histogram and Density")

portfolio_sd_tidy_builtin_percent = portfolio_returns_dplyr_byhand %>% summarise(
  sd = sd(returns),
  sd_byhand = sqrt(sum((returns-mean(returns))^2)/(nrow(.)-1))) %>% mutate(dplyr = round(sd,4)*100,
                                                                           dplyr_byhand = round(sd_byhand,4) *100)

portfolio_sd_tidy_builtin_percent %>% select(dplyr,dplyr_byhand)






                                                                                   
# ------------------------------------------------------------------

# ----------------------- Tidyquant --------------------------------
asset_returns_tq_builtin = prices %>% tk_tbl(preserve_index = TRUE,rename_index = "date") %>% 
  gather(asset,prices,-date) %>% group_by(asset) %>% tq_transmute(mutate_fun = periodReturn,period = "monthly",
                                                                  type = "log") %>% spread(asset,monthly.returns) %>% 
                                                                    select(date,symbols) %>% slice(-1)

head(asset_returns_tq_builtin,3)

portfolio_returns_tq_rebalanced_monthly = asset_returns_long %>% 
  tq_portfolio(assets_col = asset,returns_col = returns,weights = w, col_rename = "returns",rebalance_on = "months")

portfolio_returns_dplyr_byhand %>% rename(tidyverse = returns) %>% mutate(equation = coredata(portfolio_returns_byhand),
                                                                          tq = portfolio_returns_tq_rebalanced_monthly$returns,
                                                                          xts = coredata(portfolio_returns_xts_rebalanced_monthly)) %>%
  mutate_if(is.numeric,funs(round(.,3))) %>% head(3)

portfolio_sd_tidyquant_builtin_percent = portfolio_returns_tq_rebalanced_monthly %>% 
  tq_performance(Ra = returns,Rb = NULL,performance_fun = table.Stats)%>%
  select(Stdev) %>% mutate(tq_sd = round(Stdev,4)*100)

portfolio_sd_tidy_builtin_percent %>% select(dplyr,dplyr_byhand) %>% mutate(xts_builtin = portfolio_sd_xts_builtin_percent,
                                                                            matrix = sd_matrix_algebra_percent,tq = portfolio_sd_tidyquant_builtin_percent$tq_sd)

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


# ----------------------- Standard Deviation ---------------------------

covariance_matrix = cov(asset_returns_xts)
round(covariance_matrix,5)

sd_matrix_algebra = sqrt(t(w) %*% covariance_matrix %*% w)

sd_matrix_algebra_percent = round(sd_matrix_algebra * 100,2) %>% 'colnames<-'("standard deviation")
sd_matrix_algebra_percent

portfolio_returns_dplyr_byhand %>% ggplot(aes(x=date,y=returns)) + 
  geom_point(color = "cornflowerblue") + scale_x_date(breaks = pretty_breaks(n=6)) +
  ggtitle("Scatterplot of Returns by Date") + theme(plot.title = element_text(hjust = 0.5))

sd_plot = sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot = mean(portfolio_returns_tq_rebalanced_monthly$returns)

portfolio_returns_tq_rebalanced_monthly %>% mutate(hist_col_red =
                                                     if_else(returns < (mean_plot - sd_plot),
                                                             returns,as.numeric(NA)),
                                                   hist_col_green = 
                                                     if_else(returns > (mean_plot + sd_plot),
                                                             returns,as.numeric(NA)),
                                                   hist_col_blue = 
                                                     if_else(returns > (mean_plot - sd_plot) & returns < (mean_plot + sd_plot),
                                                             returns,as.numeric(NA))) %>%
  ggplot(aes(x=date)) + geom_point(aes(y=hist_col_red),color = "red") + geom_point(aes(y=hist_col_green),color = "green") +
  geom_point(aes(y=hist_col_blue),color="blue") + geom_hline(yintercept = (mean_plot + sd_plot),color="purple",linetype = "dotted") +
  geom_hline(yintercept = (mean_plot - sd_plot),color="purple",linetype="dotted") + labs(title = "Colored Scatter",y="monthly returns") +
  scale_x_date(breaks = pretty_breaks(n=8)) + theme(plot.title = element_text(hjust=0.5))

  asset_returns_long %>% group_by(asset) %>% summarize(sd=100*sd(returns)) %>% add_row(asset = "Portfolio",
                                                                                       sd = portfolio_sd_tidy_builtin_percent$dplyr) %>% 
    ggplot(aes(x=asset,y = sd,colour = asset)) + geom_point() + scale_y_continuous(labels = function(x) paste0(x,"%")) +
    geom_text(aes(x = "Portfolio",y= portfolio_sd_tidy_builtin_percent$dplyr + .2),label = "Portfolio",color = "cornflowerblue")+
    labs(y="Standard deviation")

  
  asset_returns_long %>%
    group_by(asset) %>%
    summarise(expected_return = mean(returns),
              stand_dev = sd(returns)) %>%
    add_row(asset = "Portfolio",
            stand_dev =
              sd(portfolio_returns_tq_rebalanced_monthly$returns),
            expected_return =
              mean(portfolio_returns_tq_rebalanced_monthly$returns)) %>%
    ggplot(aes(x = stand_dev,
               y = expected_return,
               color = asset)) +
    geom_point(size = 2) +
    geom_text(
      aes(x =
            sd(portfolio_returns_tq_rebalanced_monthly$returns) * 1.11,
          y =
            mean(portfolio_returns_tq_rebalanced_monthly$returns),
          label = "Portfolio")) +
    ylab("expected return") +
    xlab("standard deviation") +
    ggtitle("Expected Monthly Returns versus Risk") +
    scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
    # The next line centers the title
    theme_update(plot.title = element_text(hjust = 0.5))
  


















































