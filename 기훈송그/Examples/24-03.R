# 일반화 자동회귀 조건부 이분산 모형(GARCH)
library(quantmod)
att <- getSymbols("T", auto.assign = FALSE)
library(xts)
head(att)
plot(att)

chartSeries(att)
addBBands()
addMACD(32, 50, 12)

attClose <- att$T.Close
class(attClose)
head(attClose)

install.packages("rugarch", repos = "http://cran.us.r-project.org")
library(rugarch)
attSpec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1)),
    distribution.model = "std"
)

attGarch <- ugarchfit(spec = attSpec, data = attClose)
# attGarch는 S4 객체이기 때문에 @으로 슬롯에 접근한다.
# fit 슬롯은 리스트로, 그 요소들은 $로 접근할 수 있다.
plot(attGarch@fit$residuals, type = "l")
plot(attGarch, which = 10)

# ARMA(1, 1)
attSpec1 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1)),
    distribution.model = "std"
)

# ARMA(0, 0)
attSpec2 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "std"
)

# ARMA(0, 2)
attSpec3 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 2)),
    distribution.model = "std"
)

# ARMA(1, 2)
attSpec4 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 2)),
    distribution.model = "std"
)

attGarch1 <- ugarchfit(spec = attSpec1, data = attClose)
attGarch2 <- ugarchfit(spec = attSpec2, data = attClose)
attGarch3 <- ugarchfit(spec = attSpec3, data = attClose)
attGarch4 <- ugarchfit(spec = attSpec4, data = attClose)

infocriteria(attGarch1)
infocriteria(attGarch2)
infocriteria(attGarch3)
infocriteria(attGarch4)

attPred <- ugarchboot(attGarch, n.ahead = 50, method = c("Partial", "Full")[1])
plot(attPred, which = 2)

# 로그값을 디핑하고, 첫 번째 것은 NA이기 때문에 제외
attLog <- diff(log(attClose))[-1]

# 상세 설정
attLogSpec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1)), distribution.model = "std"
)
attLogGarch <- ugarchfit(spec = attLogSpec, data = attLog)

infocriteria(attLogGarch)
