Nile

# 시계열 데이터의 정상성 평가
plot(Nile,
    col = "darkviolet", lwd = 2,
    xlab = "Year", ylab = "Flow",
    main = "Flow of the River Nile"
)

## 정상성 검정, p-value를 보면 된다.
library(tseries)
adf.test(Nile) # p-value: 0.0642

## 몇 번의 차분이 필요할까?
library(forecast)
ndiffs(Nile)

dNile <- diff(Nile)
plot(dNile,
    col = "dodgerblue", lwd = 2,
    xlab = "Year", ylab = "Diff. Flow",
    main = "Flow of the River Nile: Diff."
)
adf.test(dNile) # p-value: 0.01

# 예측모델 생성
Acf(dNile,
    lwd = 2,
    main = "Autocorrelation for the River Nile"
)

Pacf(dNile,
    lwd = 2,
    main = "Partial Autocorrelation for the River Nile"
)

# 위 그래프로 p와 q를 결정할 수 있다.
# 감명도 원칙: 비슷한 값이라면 더 적은 값의 파라미터를 선택
Nile.arima <- arima(Nile, order = c(0, 1, 1))
Nile.arima
## 작을 수록 좋다.
accuracy(Nile.arima)

# 예측모델 평가와 예측
qqnorm(Nile.arima$residuals,
    pch = 21,
    col = "black",
    bg = "gold",
    main = "Q-Q Plot of Residuals"
)
qqline(Nile.arima$residuals,
    col = "royalblue",
    lwd = 2
)

Box.test(Nile.arima$residuals, type = "Ljung-Box")

Nile.arima.pred <- forecast(Nile.arima, h = 5)

Nile.arima.pred

plot(Nile.arima.pred,
    col = "darkgreen", lwd = 2,
    flty = 1, flwd = 3,
    fcol = "royalblue",
    shadecols = c("mistyrose", "salmon"),
    xlab = "Year", ylab = "Flow",
    main = "Forecast for flow of the River Nile"
)

# 자동으로 ARIMA
gas.arima <- auto.arima(gas)
gas.arima

arima(gas,
    order = c(2, 1, 1),
    seasonal = list(order = c(0, 1, 1), period = 12)
)

head(Nile)

forecast(gas.arima, h = 5 * 12)
plot(forecast(gas.arima, h = 5 * 12),
    col = "darkorange", lwd = 2,
    flty = 1, flwd = 3,
    fcol = "orangered",
    shadecols = c("lavender", "skyblue"),
    xlab = "Year", ylab = "Monthly Production",
    main = "Forecast for Australian Monthly Gas Production"
)
