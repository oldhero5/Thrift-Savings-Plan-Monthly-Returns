w <- c(0.10,
       0.15,
       0.15,
       0.15,
       0.10,
       0.15,
       0.05,
       0.05,
       0.05,
       0.05)
tibble(w,symbols)%>%
  summarize(total_weight = sum(w))

#portfolio returns by hand
w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]
w_6 <- w[6]
w_7 <- w[7]
w_8 <- w[8]
w_9 <- w[9]
w_10 <- w[10]

asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]
asset6 <- asset_returns_xts[,6]
asset7 <- asset_returns_xts[,7]
asset8 <- asset_returns_xts[,8]
asset9 <- asset_returns_xts[,9]
asset10 <- asset_returns_xts[,10]

portfolio_returns_byhand <-   
  (w_1 * asset1) + 
  (w_2 * asset2) + 
  (w_3 * asset3) +
  (w_4 * asset4) + 
  (w_5 * asset5) +
  (w_6 * asset6) +
  (w_7 * asset7) +
  (w_8 * asset8) +
  (w_9 * asset9) +
  (w_10 * asset10) 

names(portfolio_returns_byhand) <- "returns"
#calculating portfolio returns xts

portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-` ("returns")
head(portfolio_returns_xts_rebalanced_monthly,3)

# calculating portfolio returns tidyverse
portfolio_returns_dplyr_rebalanced_monthly <-
asset_returns_long %>%
  group_by(asset)%>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5],
                             asset == symbols[6] ~ w[6],
                             asset == symbols[7] ~ w[7],
                             asset == symbols[8] ~ w[8],
                             asset == symbols[9] ~ w[9],
                             asset == symbols[10] ~ w[10]),
         weighted_returns = returns * weights) %>%
  group_by(date) %>%
  summarize(returns = sum(weighted_returns))

head(portfolio_returns_dplyr_rebalanced_monthly,3)
#dplyr by hand
portfolio_returns_dplyr_byhand <- 
  asset_returns_long %>%
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5],
                             asset == symbols[6] ~ w[6],
                             asset == symbols[7] ~ w[7],
                             asset == symbols[8] ~ w[8],
                             asset == symbols[9] ~ w[9],
                             asset == symbols[10] ~ w[10]),
         weighted_returns = returns * weights) %>% 
  group_by(date) %>% 
  summarise(returns = sum(weighted_returns))
# Portfolio Returns Tidy Quant
portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")
# comparing methods
portfolio_returns_dplyr_rebalanced_monthly %>%
  rename(tidyverse = returns)%>%
  mutate(tq = portfolio_returns_tq_rebalanced_monthly$returns,
         xts = coredata(portfolio_returns_xts_rebalanced_monthly)) %>%
  mutate_if(is.numeric, funs(round(., 3)))%>%
 tail(3)
# visualizing monthly returns XTS

highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns")%>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue")%>%
  hc_add_theme(hc_theme_flat())%>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = F) %>%
  hc_legend(enabled = F) %>%
  hc_exporting(enabled = F)
# visualing in tidyverse scatter

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x =date, y = returns)) +
  geom_point(color = "cornflowerblue") +
  xlab('date') +
  ylab('monthly return') +
  theme_update(plot.title = element_text(hjust= 0.5)) +
  ggtitle('Portfolio Returns Scatter') +
  scale_x_date(breaks = pretty_breaks(n=6))
# visualising hist in tidyverse
portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .005,
                 fill = "cornflowerblue",
                 color = 'cornflowerblue') +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust= 0.5))
