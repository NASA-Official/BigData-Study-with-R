# 의사결정 나무
library(rpart)
creditTree <- rpart(Credit ~ CreditAmount + Age +
    CreditHistory + Employment, data = credit)
creditTree

install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
library(rpart.plot)
rpart.plot(creditTree, extra = 4)

# 부스팅 의사결정 나무
library(useful)
# 모형을 기술하는 포뮬러
# 나무이기 때문에 절편이 필요 없다.
creditFormula <- Credit ~ CreditHistory + Purpose + Employment + Duration + Age + CreditAmount - 1
# 의사결정 나무이기 때문에 카테고리형 변수에 대해선 모든 레벨을 사용
creditX <- build.x(creditFormula, data = credit, contrasts = FALSE)
creditY <- build.y(creditFormula, data = credit)

# 논리형 벡터를 [0, 1]로 변환한다.
creditY <- as.integer(relevel(creditY, ref = "Bad")) - 1

install.packages("xgboost", repos = "http://cran.us.r-project.org")
library(xgboost)
creditBoost <- xgboost(
    data = creditX, label = creditY, max.depth = 3,
    eta = 0.3, nthread = 4, nrounds = 3, objective = "binary:logistic"
)
creditBoost20 <- xgboost(
    data = creditX, label = creditY, max.depth = 3,
    eta = 0.3, nthread = 4, nrounds = 20, objective = "binary:logistic"
)

xgb.plot.importance(xgb.importance(creditBoost, feature_names = colnames(creditX)))
