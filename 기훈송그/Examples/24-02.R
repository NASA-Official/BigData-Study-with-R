# 벡터자기회귀모형
library(reshape2)

# 데이터 프레임을 넓은 포맷으로 만들기
gdpCast <- dcast(Year ~ Country,
    data = gdp[, c("Country", "Year", "PerCapGDP")],
    value.var = "PerCapGDP"
)

# 시계열 변환
gdpTS <- ts(data = gdpCast[, -1], start = min(gdpCast$Year), end = max(gdpCast$Year))

View(gdpTS)

plot(gdpTS, plot.type = "single", col = 1:8)
legend("topleft", legend = colnames(gdpTS), ncol = 2, lty = 1, col = 1:8, cex = 0.9)

gdpTS <- gdpTS[, which(colnames(gdpTS) != "Germany")]
gdpTS <- gdpTS[, which(colnames(gdpTS) != "Israel")]

View(gdpTS)

numDiffs <- ndiffs(gdpTS)
gdpDiffed <- diff(gdpTS, differences = numDiffs)

plot(gdpDiffed, plot.type = "single", col = 1:7)
legend("bottomleft", legend = colnames(gdpDiffed), ncol = 2, lty = 1, col = 1:7, cex = 0.9)

install.packages("vars", repos = "http://cran.us.r-project.org")
library(vars)
# 모형 접합
gdpVar <- VAR(gdpDiffed, lag.max = 12)
# 차원 선택
gdpVar$p
# 모형들의 이름
names(gdpVar$varresult)
# 각 모형들은 실제로는 lm 객체
class(gdpVar$varresult$Canada)
# 각 모형은 자신의 계수를 가짐
head(coef(gdpVar$varresult$Canada))

library(coefplot)
coefplot(gdpVar$varresult$Canada)
coefplot(gdpVar$varresult$Japan)

predict(gdpVar, n.ahead = 5)
