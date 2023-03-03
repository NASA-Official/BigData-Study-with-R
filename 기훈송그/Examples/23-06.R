# 랜덤 포레스트
install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
library(xgboost)

creditFormula <- Credit ~ CreditHistory + Purpose + Employment + Duration + Age + CreditAmount - 1

creditX <- build.x(creditFormula, data = credit, contrasts = FALSE)
creditY <- build.y(creditFormula, data = credit)

creditForest <- randomForest(x = creditX, y = creditY)
creditForest

creditY2 <- as.integer(relevel(creditY, ref = "Bad")) - 1

# 랜덤 포레스트 적합
boostedForest <- xgboost(
    data = creditX, label = creditY2, max_depth = 4, num_parallel_tree = 1000,
    subsample = 0.5, colsample_bytree = 0.5,
    nrounds = 3, objective = "binary:logistic"
)

install.packages("DiagrammeR", repos = "http://cran.us.r-project.org")
library(DiagrammeR)
xgb.plot.multi.trees(boostedForest, feature_names = colnames(creditX))
