install.packages("tseries", repos = "http://cran.us.r-project.org")
install.packages("forecast", repos = "http://cran.us.r-project.org")
install.packages("xts", repos = "http://cran.us.r-project.org")
install.packages("fpp3", repos = "http://cran.us.r-project.org")

# Load packages
install.packages("keras", repos = "http://cran.us.r-project.org")
install.packages("tensorflow", repos = "http://cran.us.r-project.org")
install.packages("kableExtra", repos = "http://cran.us.r-project.org")

install.packages("MAPA", repos = "http://cran.us.r-project.org")


library(keras)
library(tensorflow)
library(rstudioapi)
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(xts)
library(fpp3)
library(fable)
library(ggplot2)      # For visualization
library(lubridate)    # For date functions
library(forecast)     # For time series forecasting
library(tseries)      # Time series functions
library(kableExtra)   # Table formatting
library(dplyr)
library(dplyr)
library(tsibbledata)
library(MAPA)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

all <- readRDS(
  "./dataSet/all_orderby_time.rds"
)



all.csv <- read.csv(
    file = "./dataset/aurora_regulated.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)


all.csv$DATE <- paste(
    as.character(all.csv$YEAR),
    as.character(all.csv$MONTH),
    as.character(all.csv$DAY),
    sep="-"
)

# write.csv(  
#     all.csv,
#     'aurora_날짜_시간_분리된_파일.csv',
#     row.names = FALSE
# )

# saveRDS(
#     readRdsFile,
#     file = "./dataSet/aurora_날짜_시간_분리된_파일.rds"
# )


readRdsFile <- readRDS(
  file = "./dataSet/aurora_날짜_시간_분리된_파일.rds"
)


str(readRdsFile)

readRdsFile$FLUX_PROTON_STATE1 <- as.factor(readRdsFile$FLUX_PROTON_STATE1)
readRdsFile$FLUX_PROTON_STATE2 <- as.factor(readRdsFile$FLUX_PROTON_STATE2)
readRdsFile$ELECTRON_DATA_STATE <- as.factor(readRdsFile$ELECTRON_DATA_STATE)
readRdsFile$PROTONS_DATA_STATE <- as.factor(readRdsFile$PROTONS_DATA_STATE)


# str(readRdsFile)
# all$FLUX_PROTON_STATE1 <- as.factor(all$FLUX_PROTON_STATE1)
# all$FLUX_PROTON_STATE2 <- as.factor(all$FLUX_PROTON_STATE2)
# all$ELECTRON_DATA_STATE <- as.factor(all$ELECTRON_DATA_STATE)
# all$PROTONS_DATA_STATE <- as.factor(all$PROTONS_DATA_STATE)


head(readRdsFile$KP, 2)
tail(readRdsFile$DATE, 2)

temp <- ts(
  readRdsFile$KP,
  start = c(2001, 5089),
  end = c(2023),
  frequency = 24*365
)

plot(temp)


autoAr <- auto.arima(
  temp,
    trace = TRUE,
)


forecast::forecast(
  autoAr,
  h = 12,
  level = c(95)
)



# ar <- arima(
#   temp,
#   c(0, 1, 0),
#   seasonal = list(order= c(0,0,1))
# )



# test <- forecast(
#   auto.arima(temp),
#   level = c("mins"),
#   h = 10
# )



# temp <- as_datetime(
#   all$DATE,
#   tz = 'UTC'
# )

tsibble <- readRdsFile %>% as_tsibble(
  index = DATE,
  key = c(TIME, SW_BS)
)

head(readRdsFile)


tsibble
print(n = 130, tsibble)



autoplot(ts(tsibble$KP))
auto.arima(ts(tsibble$KP))

ar <- arima(tsibble$KP, order = c(0, 1, 0))



temp <- data.frame(
  all$DATE,
  all$SW_BS
)


