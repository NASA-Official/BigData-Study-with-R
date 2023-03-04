# https://otexts.com/fppkr/holt-winters.html
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

library(tseries)
library(forecast)
library("forecast")
library(xts)
library(tidyverse)
library(zoo)
library(MTS)
library(TTR)

all <- readRDS(
    file = "all_orderby_time.rds"
)

# 3674 = 2002.1.1 0시 ~ 187752 = 2022.12.31 23시
all_2002 <- all[3673:187752, ]

KP <- all_2002$KP

KP.ts <- ts(KP, start = c(2002, 1), frequency = 24 * 365.25)

# auto.arima
KP.arima <- auto.arima(KP.ts)
KP.arima

KP.arima.pred <- forecast(KP.arima, h = 10000)

plot(KP.arima.pred)

# MA = 3 으로 간소화
KP.ts.SMA3 <- SMA(KP.ts,
    start = c(2002, 1),
    frequency = 24 * 365.25,
    n = 3
)

# MA = 12 로 간소화
KP.ts.SMA12 <- SMA(KP.ts,
    start = c(2002, 1),
    frequency = 24 * 365.25,
    n = 12
)

# seasonal
KPcomponents <- decompose(KP.ts)
plot(KPcomponents)

KP.ts.adjust <- KP.ts - KPcomponents$seasonal
plot(KP.ts.adjust)

# beta: If set to FALSE , the function will do exponential smoothing.
# gamma: If set to FALSE , an non-seasonal model is fitted.
KPforecasts <- HoltWinters(KP.ts)

KPforecasts

summary(KPforecasts$alpha)

str(KPforecasts)

plot(KPforecasts)

KPforecasts2 <- forecast(KPforecasts, h = 8766)

plot(KPforecasts2)

head(KPforecasts2$mean, 12)
