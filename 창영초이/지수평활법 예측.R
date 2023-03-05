
install.packages("TTR", repos = "http://cran.us.r-project.org")


library(TTR)
library(forecast)
library(tseries)



readRdsFile <- readRDS(
  file = "./dataSet/aurora_날짜_시간_분리된_파일.rds"
)

kp.ts <- ts(
  readRdsFile$KP
)


kp.diff <- diff(
  kp.ts, difference = 1210)


plot(kp.diff)

acf(
  kp.diff, lag.max = 20
)

pacf(kp.diff, lag.max = 20)


testKP <- readRdsFile$KP[1:100]


testKP.ts <- ts(testKP)

plot(testKP.ts)


testKP.ts.diff <- diff(testKP.ts, difference=1)
plot(testKP.ts.diff)


# 12
acf(testKP.ts.diff, lag.max = 20)


# 13
pacf(testKP.ts.diff, lag.max = 20)



auto.arima(
  testKP
)

# ARIMA(1,0,3)
# ARIMA(12, 1, 0)
# ARIMA(13, 1, 1)



fit1 <- Arima(testKP.ts, order = c(12, 1, 1))
# AICc=129.63

Arima(testKP.ts, order = c(12, 1, 0))
# AICc=148.06

Arima(testKP.ts, order = c(12, 1, 1))
#  AICc=150.78

fit1 %>% forecast(h = 8) %>% autoplot()

fit1 %>% forecast(h = 18, c(90))

forecast.Arima(fit1)
