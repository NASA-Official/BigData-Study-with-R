install.packages("tseries", repos = "http://cran.us.r-project.org")
install.packages("forecast", repos = "http://cran.us.r-project.org")
install.packages("MTS", repos = "http://cran.us.r-project.org")

library(tseries)
library(forecast)
library(xts)
library(tidyverse)
library(zoo)
library(MTS)

all <- readRDS(
    file = "all_orderby_time.rds"
)

head(all$DATE, 26)

# only KP
DATE <- all$DATE
KP <- all$KP

train <- as.data.frame(DATE)
train$KP <- KP

trainTS <- ts(
    train[, -1],
    frequency = 8760,
    start = c(2001, 5089)
)

# 정상성 검정
## Augmented Dickey-Fuller Test
## data:  trainTS
## Dickey-Fuller = -38.169, Lag order = 57, p-value = 0.01
## alternative hypothesis: stationary
## 경고메시지(들):
## adf.test(trainTS)에서: p-value smaller than printed p-value
adf.test(trainTS)

# 몇 번의 차분?
ndiffs(trainTS)

dTrainTS <- diff(trainTS)

## Augmented Dickey-Fuller Test
## data:  dTrainTS
## Dickey-Fuller = -76.531, Lag order = 57, p-value = 0.01
## alternative hypothesis: stationary
## adf.test(dTrainTS)에서: p-value smaller than printed p-value
adf.test(dTrainTS)

trainTS.arima <- auto.arima(trainTS)
trainTS.arima

forecast(trainTS.arima, h = 500)
plot(forecast(trainTS.arima, h = 8760),
    col = "darkorange", lwd = 2,
    flty = 1, flwd = 3,
    fcol = "orangered",
    shadecols = c("lavender", "skyblue"),
    xlab = "Year", ylab = "KP",
    main = "Forecast for KP"
)



##########################################



all_2002 <- all %>% filter(
    all$DATE$year + 1900 == 2002 | all$DATE$year + 1900 == 2003
)

View(all_2002)

str(all_2002)

# all$DATE <- as.Date(all$DATE)

# allTS <- xts(
#     x = all[, -1],
#     order.by = all$DATE,
#     frequency = 8760,
#     tzone = "UTC",
# )

head(all_2002[, -1])

allTS_2002 <- ts(
    all_2002,
    frequency = 8760,
    start = c(2002, 1)
)

mySelect <- allTS_2002[, 18:22]

mySelect.VARselect <- VARselect(mySelect, lag.max = 10, type = "const", season = 8760)

mySelect.VARselect

lags <- embed(
    mySelect,
    10
)

mySelect.model <- VAR(
    mySelect,
    p = 0,
    type = "const"
)

summary(mySelect.model)
