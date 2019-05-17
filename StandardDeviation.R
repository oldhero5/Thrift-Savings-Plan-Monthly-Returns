
#building the covariance matrix 
covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix, 5)

sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)
sd_matrix_algebra_percent <- 
  round(sd_matrix_algebra*100, 2) %>%
  `colnames<-`("standard deviation")
sd_matrix_algebra_percent[1,]


#standard deviation in XTS
portfolio_sd_xts_builtin <-
  StdDev(asset_returns_xts, weights = w)
portfolio_sd_xts_builtin_percent <- 
  round(portfolio_sd_xts_builtin * 100, 2)

portfolio_sd_xts_builtin_percent[1,]

#tidyverse method
portfolio_sd_tidy_builtin_percent <-
  portfolio_returns_dplyr_byhand %>% 
  summarise(
    sd = sd(returns,na.rm = T)) %>% 
  mutate(dplyr = round(sd, 4) * 100)  

#tidyquant method

portfolio_sd_tq_builtin_percent <-
  portfolio_returns_tq_rebalanced_monthly %>%
    tq_performance(Ra = returns,
                   Rb = NULL,
                   performance_fun = table.Stats) %>%
    select(Stdev) %>%
    mutate(tq_sq =round(Stdev, 4) * 100)

#Visualising SD

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x =date, y = returns)) +
  geom_point(color = "cornflowerblue") +
  xlab('date') +
  ylab('monthly return') +
  theme_update(plot.title = element_text(hjust= 0.5)) +
  ggtitle('Scatterplot of Returns by Date') +
  scale_x_date(breaks = pretty_breaks(n=6))

sd_plot <-
  sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

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
  labs(title = "Colored Scatter", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))
#add a line
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
#comparing portfolio risk with asset risk

asset_returns_long %>%
  group_by(asset) %>% 
  summarize(sd = 100 * sd(returns, na.rm = TRUE)) %>% 
  add_row(asset = "Portfolio", 
          sd = portfolio_sd_tidy_builtin_percent$dplyr) %>% 
  ggplot(aes(x = asset, 
             y = sd, 
             colour = asset)) +
  geom_point() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(
    aes(x = "Portfolio", 
        y = 
          portfolio_sd_tidy_builtin_percent$dplyr + .2), 
    label = "Portfolio",
    color = "cornflowerblue")

#risk versus return

asset_returns_long %>%
  group_by(asset) %>%
  summarize(expected_return = mean(returns, na.rm = T),
            stand_dev = sd(returns, na.rm = T)) %>%
  add_row(asset = 'Portfolio',
          stand_dev = 
            sd(portfolio_returns_tq_rebalanced_monthly$returns, na.rm = T),
          expected_return = mean(portfolio_returns_tq_rebalanced_monthly$returns, na.rm = T)) %>%
  ggplot(aes( x = stand_dev,
              y = expected_return,
              color = asset)) + 
  geom_point(size = 2) +
  geom_text(
    aes(x = 
          sd(portfolio_returns_tq_rebalanced_monthly$returns, na.rm = T) * 1.11,
        y =
          mean(portfolio_returns_tq_rebalanced_monthly$returns, na.rm = T),
            label = "Portfolio")) +
  ylab("expected return") +
  xlab("standard deviation") +
  ggtitle("Expected Monthly Return Versus Risk") +
  scale_y_continuous(labels = function(x){ paste0(x, '%')}) +
  theme_update(plot.title = element_text(hjust = .5))
  # rolling SD
window <- 24
#XTS world unrealiable
port_rolling_sd_xts <- 
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = sd,
            width = window) %>% 
  # omit the 23 months for which there is no rolling 24
  # month standard deviation
  na.omit() %>% 
  `colnames<-`("rolling_sd")

tail(port_rolling_sd_xts, 3)
#tibble time world
sd_roll_24 <- 
  rollify(sd, window = window)

port_rolling_sd_tidy_tibbletime <- 
  portfolio_returns_tq_rebalanced_monthly %>%
  as_tbl_time(index = date) %>% 
  mutate(sd = sd_roll_24(returns)) %>% 
  select(-returns) %>% 
  na.omit()

tail(port_rolling_sd_tidy_tibbletime, 3)
# tidy quant

port_rolling_sd_tq <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>%
  select(date, rolling_sd) %>% 
  na.omit()
port_rolling_sd_tidy_tibbletime %>% 
  mutate(sd_tq = port_rolling_sd_tq$rolling_sd,
         sd_xts = round(port_rolling_sd_xts$rolling_sd, 4)) %>% 
  tail(3)
#visualizing
port_rolling_sd_tq_hc<- 
  round(port_rolling_sd_xts, 4) * 100
highchart(type = "stock") %>% 
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_tq_hc, 
                color = "cornflowerblue") %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"), 
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled= TRUE) %>% 
  hc_legend(enabled = TRUE)
