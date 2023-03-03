install.packages("UsingR")

data(father.son, package = "UsingR")
library(ggplot2)
head(father.son)

ggplot(father.son, aes(x = fheight, y = sheight))
        + geom_point()
        + geom_smooth(method = "lm")
        + labs(x = "Fathers", y = "Sons"
         )

heightsLM <- lm(sheight ~ fheight, data = father.son)

data(tips, package = "reshape2")
tipsLM <- lm(tip ~ day - 1, data = tips)

tipAnova <- aov(tip ~ day - 1, tips)
summary(tipAnova)
summary(tipsLM)

install.packages("dplyr")
library(dplyr)
# 평균과 신뢰 구간을 매뉴얼로 계산
tipsByDay <- tips %>% group_by(day) %>%
  dplyr::summarize(
    tip.mean = mean(tip), tip.sd = sd(tip),
    Length = NROW(tip),
    tfrac = qt(p = 0.9, df = Length - 1),
    Lower = tip.mean - tfrac * tip.sd / sqrt(Length),
    Upper = tip.mean + tfrac * tip.sd / sqrt(Length)
  )
head(tipsByDay)

# tipsLM의 요약에서 값을 추출
tipsInfo <- summary(tipsLM)
tipsCoef <- as.data.frame(tipsInfo$coefficients[, 1:2])
head(tipsCoef)

tipsCoef <- within(tipsCoef, {
  Lower <- Estimate - qt(p = 0.90, df = tipsInfo$df[2]) * `Std. Error`
  Upper <- Estimate + qt(p = 0.90, df = tipsInfo$df[2]) * `Std. Error`
  day <- rownames(tipsCoef)
})

head(tipsCoef)

housing <- read.table("https://www.jaredlander.com/data/housing.csv",
                      sep = ",", header = TRUE,
                      stringsAsFactors = FALSE
                      )
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt",
                    "Income", "IncomePerSqFt", "Expense", "ExpensesPerSqFt",
                    "NetIncome", "Value", "ValuePerSqFt", "Boro")
head(housing)

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)

house1$coefficients
coef(house1)
coefficients(house1)

# 상호작용을 포뮬러에 포함시키라면 *
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
# 상호작용만 보고자 한다면 : 사용
house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data = housing)

house4 <- lm(ValuePerSqFt ~ SqFt*Units*Income, data = housing)
house5 <- lm(ValuePerSqFt ~ Class*Boro, data = housing)

house1.b <- lm(ValuePerSqFt ~ scale(Units) + scale(SqFt) + Boro, data = housing)
# I함수로 감싸면 포뮬러 안에서 수학적인 관계를 유지함
house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, data = housing)

house7 <- lm(ValuePerSqFt ~ (Units + SqFt)^2, data = housing)
house8 <- lm(ValuePerSqFt ~ Units * SqFt, data = housing)
house9 <- lm(ValuePerSqFt ~ I(Units + SqFt)^2, data = housing)

housingNew <- read.table("http://www.jaredlander.com/data/housingNew.csv",
                         sep=",", header=TRUE, stringsAsFactors = FALSE)

housePredict <- predict(house1, newdata = housingNew, se.fit = TRUE,
                        interval = "prediction", level = .95)

head(housePredict$fit)
head(housePredict$se.fit)





