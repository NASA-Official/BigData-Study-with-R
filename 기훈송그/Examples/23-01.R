# 비선형 최소 제곱
path <- "/Users/song/code/R_Study/data/wifi.rdata"
download.file(
    url = "https://www.jaredlander.com/data/wifi.rdata",
    destfile = path
)

load("data/wifi.rdata")
head(wifi)
library(ggplot2)
ggplot(
    wifi, aes(x = x, y = y, color = Distance)
) +
    geom_point() +
    scale_color_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = mean(wifi$Distance)
    )

# 제급근 모형 설정
# 초깃값은 그리드의 중심값을 사용한다.
wifiMod1 <- nls(Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2), # nolint
    data = wifi, start = list(betaX = 50, betaY = 50)
)
summary(wifiMod1)

ggplot(wifi, aes(x = x, y = y, color = Distance)) +
    geom_point() +
    scale_color_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = mean(wifi$Distance)
    ) +
    geom_point(
        data = as.data.frame(t(coef(wifiMod1))),
        aes(x = betaX, y = betaY), size = 5, color = "green"
    )

# 스플라인(splines)
data(diamonds, package = "ggplot2")
# 몇 개의 자유도를 갖고 적합 실행
# 자유도는 1보다 커야함
# 그렇지만 데이터의 고유한 x값보다는 작아야 함
diaSpline1 <- smooth.spline(x = diamonds$carat, y = diamonds$price)
diaSpline2 <- smooth.spline(x = diamonds$carat, y = diamonds$price, df = 2)
diaSpline3 <- smooth.spline(x = diamonds$carat, y = diamonds$price, df = 10)
diaSpline4 <- smooth.spline(x = diamonds$carat, y = diamonds$price, df = 20)
diaSpline5 <- smooth.spline(x = diamonds$carat, y = diamonds$price, df = 50)
diaSpline6 <- smooth.spline(x = diamonds$carat, y = diamonds$price, df = 100)

get.spline.info <- function(object) {
    data.frame(x = object$x, y = object$y, df = object$df)
}
library(plyr)
library(ggplot2)
# 결과들을 하나의 데이터 프레임으로 묶음
splineDF <- ldply(
    list(diaSpline1, diaSpline2, diaSpline3, diaSpline4, diaSpline5, diaSpline6),
    get.spline.info
)
head(splineDF)

g <- ggplot(diamonds, aes(x = carat, y = price)) +
    geom_point()
g + geom_line(
    data = splineDF,
    aes(x = x, y = y, color = factor(round(df, 0)), group = df)
) + scale_color_discrete("Degrees of \nFreedom")

library(splines)
head(ns(diamonds$carat, df = 1))
head(ns(diamonds$carat, df = 2))
head(ns(diamonds$carat, df = 3))
head(ns(diamonds$carat, df = 4))

# 6개 노트를 가진 경우가 데이터에 좀 더 부드럽게 적합됨
g + stat_smooth(method = "lm", formula = y ~ ns(x, 6), color = "blue")
g + stat_smooth(method = "lm", formula = y ~ ns(x, 3), color = "red")
