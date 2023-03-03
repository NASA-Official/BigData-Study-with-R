pbinom(q = 1, size = 2, prob = 0.5)

pois1 <- rpois(n = 10000, lambda = 1)
pois2 <- rpois(n = 10000, lambda = 2)
pois5 <- rpois(n = 10000, lambda = 5)
pois10 <- rpois(n = 10000, lambda = 10)
pois20 <- rpois(n = 10000, lambda = 20)

pois <- data.frame(Lambda.1 = pois1, Lambda.2 = pois2, Lambda.5 = pois5,
                   Lambda.10 = pois10, Lambda.20 = pois20)

library(reshape2)

pois <- melt(data = pois, variable.name = "Lambda", value.name = "x")

# 새로운 열 이름 정돈
library(stringr)
pois$Lambda <- as.factor(as.numeric(str_extract(string = pois$Lambda,
                                                pattern = "\\d+")))
