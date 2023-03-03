library(plyr)
baseball <- baseball[baseball >= 1990,]
head(baseball)

# 평균 타율 계산
bat.avg <-
  function(data,
           indices = 1:NROW(data),
           hits = "h",
           at.bats = "ab") {
    sum(data[indices, hits], na.rm = TRUE) / sum(data[indices, at.bats], na.rm =
                                                   TRUE)
  }

bat.avg(baseball)

avgBoot <-
  boot(
    data = baseball,
    statistic = bat.avg,
    R = 1200,
    stype = "i"
  )
avgBoot

# 신뢰 구간 출력
boot.ci(avgBoot, conf = .95, type = "norm")

# 단계적 변수 선택
# null 모형, 평균 값 직선
nullModel <- lm(ValuePerSqFt ~ 1, data = housing)
# 가장 복잡한 모형
fullModel <-
  lm(ValuePerSqFt ~ Units + SqFt * Boro + Boro * Class, data = housing)

houseStep <-
  step(
    nullModel,
    scope = list(lower = nullModel, upper = fullModel),
    direction = "both"
  )