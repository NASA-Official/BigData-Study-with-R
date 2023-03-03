# 잔차
housing <- read.table("https://www.jaredlander.com/data/housing.csv",
                      sep = ",", header = TRUE,
                      stringsAsFactors = FALSE)

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt",
                    "Income", "IncomePerSqFt", "Expense", "ExpensesPerSqFt",
                    "NetIncome", "Value", "ValuePerSqFt", "Boro")
# 몇 개의 이상점 제거
housing <- housing[housing$Units < 1000, ]
head(housing)

# 모형 적합
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)
# 모형 시각화
install.packages("coefplot")
library(coefplot)
coefplot(house1)
library(ggplot2)
head(fortify(house1))

# 그래프를 객체로 저장
# .fitted, .resid가 새로 생김
h1 <- ggplot(aes(x = .fitted, y = .resid), data = house1) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted Values", y = "Residuals")
h1 + geom_point(aes(color = Boro))

# 기본 플롯
plot(house1, which = 1, col = as.numeric(factor(house1$model$Boro)))
legend("topright", legend = levels(factor(house1$model$Boro)),
       pch = 1, col = as.numeric(factor(levels(factor(house1$model$Boro)))),
       text.col = as.numeric(factor(levels(factor(house1$model$Boro)))),
       title = "Boro"
       )

# 모형의 적합도가 좋은 경우, 표준화된 잔차는 정규분포에 대한 이론적인 분위 수에
# 대한 그래프를 그려보면 직선 형태가 되어야 함
plot(house1, which = 2)
ggplot(house1, aes(sample = .stdresid)) + stat_qq() + geom_abline()

ggplot(house1, aes(x = .resid)) + geom_histogram()
