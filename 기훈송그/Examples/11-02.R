install.packages('ggplot2')
data(diamonds, package = 'ggplot2')
head(diamonds)

# aggregate (집계)
# cut 타입에 따른 price의 mean(평균)
aggregate(price ~ cut, diamonds, mean)

# ddply
install.packages("plyr")
library(plyr)

# NA 제거
baseball$sf[baseball$year < 1954] <- 0
any(is.na(baseball$sf))
baseball$hbp[is.na(baseball$hbp)] <- 0
any(is.na(baseball$hbp))
# 한 시즌 50타석 미만인 선수 제외
baseball <- baseball[baseball$ab >= 50, ]
# 출루율 계산
baseball$OBP <- with(baseball, (h + bb + hbp) / (ab + bb + hbp + sf))
tail(baseball)
