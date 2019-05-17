library("tidyverse")
library("PerformanceAnalytics")
library("readxl")
library("highcharter")
library("tidyquant")
library("timetk")
library("tibbletime")
library("quantmod")
library("scales")
library("lubridate")


#Read TSP Data (Need to figure out how to download as a scraper)

prices <- 
  read_csv("~/DATA/FINANCE/ReproducibleFinance/shareprices.csv",col_types =
             cols(date = 
                    col_date(format = "%m/%d/%Y")))%>% 
            tk_xts(date_var = date)
  
colnames(prices)
symbols <- c("Income", "L2020", "L2030", "L2040", "L2050", 
             "G", "F", "C", "S", "I")

#xts monthly returns
prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)
head(prices_monthly)
asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method ="log")%>%
  na.omit()
head(asset_returns_xts,3)
tail(asset_returns_xts,3)
#Tidy by hand returns
asset_returns_dplyr_byhand <- 
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>%
  # now remove the index because it got converted to row names
  remove_rownames() %>% 
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  spread(asset, returns) %>% 
  select(date, symbols) %>% 
  na.omit()
#tibble time asset returns
asset_returns_tbltime <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  as_tbl_time(index = date) %>%
  as_period(period = "month",
            side = "end") %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn, 
               type = "log") %>%
  spread(asset, monthly.returns)%>%
  select(date, symbols) %>%
  slice(-1)

#long version of DPLYR

asset_returns_long <-
  asset_returns_dplyr_byhand%>%
  gather(asset,returns, -date)%>%
  group_by(asset)%>%
  na.omit

# Data Visualization of Returns
highchart(type  ="stock")%>%
  hc_title(text = " Monthly Log Returns Thrift Savings Plan")%>%
  hc_add_series(asset_returns_xts[,symbols[6]],
                name =symbols[6]) %>%
  hc_add_series(asset_returns_xts[,symbols[7]],
                name =symbols[7]) %>%
  hc_add_series(asset_returns_xts[,symbols[8]],
                name =symbols[8]) %>%
  hc_add_series(asset_returns_xts[,symbols[9]],
                name =symbols[9]) %>%
  hc_add_series(asset_returns_xts[,symbols[10]],
                name =symbols[10]) %>%
  hc_add_theme(hc_theme_flat())%>%
  hc_navigator(enabled = T)%>%
  hc_scrollbar(enabled = F)%>%
  hc_exporting(enabled = F) %>%
  hc_legend(enabled = T)
#ggplot asset returns

asset_returns_long%>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = 0.005) +
  ggtitle("Monthly Returns TSP")
