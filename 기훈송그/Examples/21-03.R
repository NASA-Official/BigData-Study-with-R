library(boot)
housing <- read.table("https://www.jaredlander.com/data/housing.csv",
                      sep = ",", header = TRUE,
                      stringsAsFactors = FALSE)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt",
                    "Income", "IncomePerSqFt", "Expense", "ExpensesPerSqFt",
                    "NetIncome", "Value", "ValuePerSqFt", "Boro")
housing <- housing[housing$Units < 1000, ]

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
# lm 대신 glm 사용
houseG1 <- glm(ValuePerSqFt ~ Units + SqFt + Boro,
               data = housing, family = gaussian(link="identity"))

identical(coef(house1), coef(houseG1))
# 5겹 교차 타당성 검증
houseCV1 <- cv.glm(housing, houseG1, K = 5)
# 에러 체크
houseCV1$delta

#glm으로 모형 접합을 다시 시행
houseG2 <- glm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)

houseG3 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + Class, data = housing)

houseG4 <- glm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class, data = housing)

houseG5 <- glm(ValuePerSqFt ~ Boro + Class, data = housing)

# 교차타당도 계산
houseCV2 <- cv.glm(housing, houseG2, K = 5)
houseCV3 <- cv.glm(housing, houseG3, K = 5)
houseCV4 <- cv.glm(housing, houseG4, K = 5)
houseCV5 <- cv.glm(housing, houseG5, K = 5)

# 오차확인
# 결과를 데이터 프레임으로
cvResults <-
  as.data.frame(
    rbind(
      houseCV1$delta,
      houseCV2$delta,
      houseCV3$delta,
      houseCV4$delta,
      houseCV5$delta
    )
  )

# 결과를 보기 좋게 다듬기
names(cvResults) <- c("Error", "Adjusted.Error")
# 모형 이름 추가
cvResults$Model <- sprintf("houseG%s", 1:5)

cvResults
