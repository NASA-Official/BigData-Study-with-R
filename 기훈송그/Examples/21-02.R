# 모형 비교
housing <- read.table("https://www.jaredlander.com/data/housing.csv",
                      sep = ",", header = TRUE,
                      stringsAsFactors = FALSE)

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt",
                    "Income", "IncomePerSqFt", "Expense", "ExpensesPerSqFt",
                    "NetIncome", "Value", "ValuePerSqFt", "Boro")
housing <- housing[housing$Units < 1000, ]

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
# 상호작용을 포뮬러에 포함시키라면 *
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
# 상호작용만 보고자 한다면 : 사용
house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data = housing)
house4 <- lm(ValuePerSqFt ~ SqFt*Units*Income, data = housing)
house5 <- lm(ValuePerSqFt ~ Class*Boro, data = housing)

multiplot(house1, house2, house3, house4, house5, pointSize = 2)

# 분산분석(Anova)
anova(house1, house2, house3, house4, house5)

# AIC
AIC(house1, house2, house3, house4, house5)
# BIC
BIC(house1, house2, house3, house4, house5)

# ValuePerSqFt 변수에서 150을 기준으로 이진변수를 만든다.
housing$HighValues <- housing$ValuePerSqFt >= 150

# 로지스틱 회귀모형 적용
high1 <- glm(HighValues ~ Units + SqFt + Boro,
             data = housing, family = binomial(link="logit"))
high2 <- lm(HighValues ~ Units * SqFt + Boro,
            data = housing, family = binomial(link="logit"))
high3 <- lm(HighValues ~ Units + SqFt*Boro + Class,
            data = housing, family = binomial(link="logit"))
high4 <- lm(HighValues ~ Units * SqFt*Boro + SqFt*Class,
            data = housing, family = binomial(link="logit"))
high5 <- lm(HighValues ~ Boro + Class,
            data = housing, family = binomial(link="logit"))

anova(high1, high2, high3, high4, high5)

AIC(high1, high2, high3, high4, high5)

BIC(high1, high2, high3, high4, high5)







