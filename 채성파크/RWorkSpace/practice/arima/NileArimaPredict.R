#시계열 데이터 정상성 평가
Nile
plot(Nile, col="darkviolet", lwd=2,
     xlab="Year", ylab="Flow", main="Flow of the River Nile")

install.packages("tseries")
library(tseries)
adf.test(Nile)
#p-value > 0.05 초과 => 차분 필요

#차분 얼마나 해야하냐?
install.packages("forecast")
library(forecast)
ndiffs(Nile)

#차분 진행
dNile <- diff(Nile)
plot(dNile, col="dodgerblue", lwd=2,
     xlab="Year", ylab="Differenced Flow",
     main="Flow of the River Nild: Differenced")

#차분한 값은 p-value 적절함
adf.test(dNile)

# 예측모델 생성

Acf(dNile, lwd=2,
    main="Autocorrelation for the River Nile")
Pacf(dNile, lwd=2,
    main="Partial Autocorrelation for the River Nile")
  
#AR(p)/ARMA(p, 0) => ACF : 점차 감소하며 0 접근, PACF : 시차 p 이후에 0
#MA(q)/ARMA(0, q) => ACF : 시차 q 이후에 0, PACF : 점차 감소하며 0에 접근
#ARMA(p, q) => ACF: 점차 감소하며 0 접근(시차 q 이후에 0), PACF: 점차 감소하며 0에 접근 (시차 p 이후에 0)

# 자기 상관은 시차 1 이후에 0이 됨
# 편자기상관은 시차 2 이후에 0이 됨
# => AR(2)/ARMA(2, 0) | MA(1)/ARMA(0, 1) | ARMA(2, 1) 모델 선택 가능

Nile.arima <- arima(Nile, order=c(0, 1, 1))
Nile.arima
accuracy(Nile.arima)

# 예측모델 평가와 예측

qqnorm(Nile.arima$residuals, pch=21, col="black",
       bg="gold", main = "Q-Q Plot of Residuals")
qqline(Nile.arima$residuals, col="royalblue", lwd=2)

#귀무가설 검증
Box.test(Nile.arima$residuals, type="Ljung-Box")

Nile.arima.pred <- forecast(Nile.arima, h=5)
Nile.arima.pred

plot(Nile.arima.pred, col="darkgreen", lwd=2,
     fity=1, flwd=3,
     fcol="rpyalblue", shadecols=c("mistyrose", "salmon"),
     xlab="Year", ylab="Flow",
     main="Forecast for flow of the River Nile")

#아리마 자동사냥
gas
gas.arima <- auto.arima(gas)
gas.arima

arima(gas, order=c(2,1,1),
      seasonal=list(order=c(0, 1, 1), period=12))
forecast(gas.arima, h=5 * 12)
plot(forecast(gas.arima, h=5 * 12), col="darkorange", lwd=2,
     fity=1, flwd=3,
     fcol="orangered", shadecols=c("lavender", "skyblue"),
     xlab="Year", ylab="Monthly Production",
     main="Forecast for Austrailian Monthly Gas Production")

