# 자기회귀이동평균
install.packages("WDI", repos = "http://cran.us.r-project.org")
library(WDI)

# 데이터 가져오기
gdp <- WDI(
    country = c("US", "CA", "GB", "DE", "CN", "JP", "SG", "IL"),
    indicator = c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
    start = 1960, end = 2011
)

# 적절한 이름 붙이기
names(gdp) <- c("Country", "iso2c", "iso3c", "Year", "PerCapGDP", "GDP")

library(ggplot2)
library(scales)

# 1인당 GDP
ggplot(gdp, aes(Year, PerCapGDP, color = Country, linetype = Country)) +
    geom_line() +
    scale_y_continuous(label = dollar)
library(useful)

# 절대 GDP
ggplot(gdp, aes(Year, GDP, color = Country, linetype = Country)) +
    geom_line() +
    scale_y_continuous(label = multiple_format(extra = dollar, multiple = "M"))


# 미국 데이터 추출
us <- gdp$PerCapGDP[gdp$Country == "United States"]

# 시계열로 변환
us <- ts(us, start = min(gdp$Year), end = max(gdp$Year))
plot(us, ylab = "Per Capital GDP", xlab = "Year")

acf(us)
pacf(us)

install.packages("forecast", , repos = "http://cran.us.r-project.org")
library(forecast)
ndiffs(x = us)
plot(diff(us, 2))

usBest <- auto.arima(x = us)
usBest

acf(usBest$residuals)
pacf(usBest$residuals)

coef(usBest)

# 표준편차를 비롯한 향후 5년을 예측
predict(usBest, n.ahead = 5, se.fit = TRUE)

# 향후 5년을 예측
theForecast <- forecast(object = usBest, h = 5)
plot(theForecast)
