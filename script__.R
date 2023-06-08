# Import data with csv file
library(readr)
coding_test <- read_csv("coding test.csv")
View(coding_test)

# Convert data into time series format
ts <- data.frame(date = as.Date(coding_test$Date, format = "%d/%m/%Y"), usdzar = coding_test$USDZAR)
library(zoo)
library(xts)
ts_xts <- as.xts(ts$usdzar, order.by = ts$date)

library(TTR)
library(quantmod)
library(PerformanceAnalytics)

sma_X <- SMA(ts_xts, n = 20)
sma_Y <- SMA(ts_xts, n = 100)
lineChart(ts_xts, theme = 'white')
addSMA(n = 20, col = 'blue')
addSMA(n = 100, col = 'red')
legend('left', col = c('green','blue','red'),
       legend = c('USDZAR','SMA_X','SMAX_Y'),lty = 1,
       bty = 'n', text.col = 'black',cex = 0.8)

  
# Simulate SMA model
sma <- function(prices, x, y, z){
  # Simple Moving Average
  sma_x <- SMA(prices, n = x)
  sma_y <- SMA(prices, n = y)
  
  #SMA Trading signals__long(1): sma_x > sma_y, otherwise:short(-1), remaining(0)
  sma_ts <- Lag(
    ifelse(Lag(sma_x) < Lag(sma_y) & sma_x >= sma_y, 1,
           ifelse(Lag(sma_x) > Lag(sma_y) & sma_x <= sma_y, -1, 0)))
  sma_ts[is.na(sma_ts)]<- 0
  
  #Security position (1: hold stock, 0: don't own stock)
  sma_strat <- ifelse(sma_ts > 1, 0, 1)
  for (i in 2 : length(sma_ts)) {
    sma_strat[i] <- ifelse(sma_ts[i] == 1, 1, ifelse(sma_ts[i] == -1, 0, sma_strat[i-1]))
  }
  sma_strat[is.na(sma_strat)] <- 1
  
  #daily profits and losses
  bh_return <- periodReturn(prices,period = "daily",type = "log")
  sma_return <- bh_return * sma_strat - z * abs(diff(c(0, sma_strat)))
  
  out_put <- cbind(bh_return,sma_return)
  colnames(out_put) <- c("buy&hold_returns","sma_returns")
  out_put
}

sma_performance <- sma(ts_xts, 20, 100, 0.0005)
charts.PerformanceSummary(sma_performance, main = 'Performance of SMA Strategy')





