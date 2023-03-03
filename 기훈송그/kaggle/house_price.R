train <-
    read.csv(
        "./data/house_prices/train.csv",
    )
test <-
    read.csv(
        "./data/house_prices/test.csv",
    )

test$SalePrice <- 0
all <- rbind(train, test)

# 데이터 전처리
library(dplyr)

str(all)

sapply(all, function(x) {
    sum(is.na(x))
})

na.cols <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[na.cols], is.na)), decreasing = TRUE)

## int형부터. 없으면 0을 넣어줌.
all$LotFrontage[is.na(all$LotFrontage)] <- 0
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
all$GarageCars[is.na(all$GarageCars)] <- 0
all$GarageArea[is.na(all$GarageArea)] <- 0

## GarageYrBlt. 집이 지어진 날짜와 동일하다고 추정하자.
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
all$GarageYrBlt[all$GarageYrBlt == 2207] <- 2007

## 값 NA이 아닌 진짜 NA 대체
### MSZoning. RL이 제일 많다.
all[is.na(all$MSZoning), "MSZoning"] <- "RL"

### Exterior1st. VinylSd가 제일 많다.
table(all$Exterior1st)
all[is.na(all$Exterior1st), "Exterior1st"] <- "VinylSd"

### Exterior2nd. VinylSd가 제일 많다.
all[is.na(all$Exterior2nd), "Exterior2nd"] <- "VinylSd"

# MasVnrType. NA를 None으로 대체
all[is.na(all$MasVnrType), "MasVnrType"] <- "None"

### KitchenQual. TA가 제일 많다.
all[is.na(all$KitchenQual), "KitchenQual"] <- "TA"

### Functional. Typ가 제일 많다.
all$Functional[is.na(all$Functional)] <- "Typ"

### SaleType. WD가 제일 많다.
all$SaleType[is.na(all$SaleType)] <- "WD"

# 값 NA를 다른 말로 대체
all$Alley[is.na(all$Alley)] <- "None"
all$BsmtQual[is.na(all$BsmtQual)] <- "None"
all$BsmtCond[is.na(all$BsmtCond)] <- "None"
all$BsmtExposure[is.na(all$BsmtExposure)] <- "None"
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "None"
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "None"
all$Electrical[is.na(all$Electrical)] <- "None"
all$FireplaceQu[is.na(all$FireplaceQu)] <- "None"
all$GarageType[is.na(all$GarageType)] <- "None"
all$GarageFinish[is.na(all$GarageFinish)] <- "None"
all$GarageQual[is.na(all$GarageQual)] <- "None"
all$GarageCond[is.na(all$GarageCond)] <- "None"
all$PoolQC[is.na(all$PoolQC)] <- "None"
all$Fence[is.na(all$Fence)] <- "None"
all$MiscFeature[is.na(all$MiscFeature)] <- "None"

# Id, Utilities 열은 버린다.
all$Utilities <- NULL
all$Id <- NULL

# NA 체크
na.cols <- which(colSums(is.na(all)) > 0)

# string -> factor
all <- as.data.frame(unclass(all), stringsAsFactors = TRUE)

# 정제된 데이터를 다시 train과 test로
train <- all[(all$SalePrice) > 0, ]
test <- all[(all$SalePrice) == 0, ]

# applying models
library(caret)

# 랜덤 포레스트
library(randomForest)
learn_rf <- randomForest(SalePrice ~ .,
    data = train,
    ntree = 500, proximity = TRUE, importance = TRUE
)
cm_rf <- predict(learn_rf, newdata = test)
Id <- test$Id
output_df <- as.data.frame(Id)
output_df$SalePrice <- cm_rf
output_df$SalePrice <- as.factor(output_df$SalePrice)

write.csv(output_df, file = "kaggle_submission.csv", row.names = FALSE)
