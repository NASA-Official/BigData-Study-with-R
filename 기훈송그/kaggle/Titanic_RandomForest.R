# Data Import
titanic_train <-
  read.csv(
    "./R_Study/data/titanic/train.csv",
    stringsAsFactors = FALSE,
    header = TRUE
  )
titanic_test <-
  read.csv(
    "./R_Study/data/titanic/test.csv",
    stringsAsFactors = FALSE,
    header = TRUE
  )

str(titanic_train)
str(titanic_test)

summary(titanic_train)

library(ggplot2)
# 요금에 따른 승객 분포
titanic_fare <-
  ggplot(titanic_train,
    aes(x = Fare, y = ..density..),
    main = "",
    xlab = "Fare Distribution"
  ) +
  geom_density(fill = "blue", alpha = 0.2)
# 나이에 따른 승객 분포
titanic_age <-
  ggplot(titanic_train,
    aes(x = Age, y = ..density..),
    main = "",
    xlab = " Age Distribution"
  ) +
  geom_density(fill = "blue", alpha = 0.2)

install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(gridExtra)
grid.arrange(titanic_fare, titanic_age, nrow = 1, ncol = 2)

# 성별, 좌석 등급에 따른 생존 여부
par(mfrow = c(1, 2))
mosaicplot(table(ifelse(titanic_train$Survived == 1, "Survived", "Dead"), titanic_train$Sex), main = "", cex = 1.2, color = TRUE)
mosaicplot(table(ifelse(titanic_train$Survived == 1, "Survived", "Dead"), titanic_train$Pclass), main = "", cex = 1.2, color = TRUE)

# 연령에 따른 생존 여부
par(mfrow = c(1, 2))
boxplot(Age ~ Survived, titanic_train, xlab = "Survived", ylab = "Age", cex = 1.2)
plot(Age ~ jitter(Survived), titanic_train, xlab = "Survived")

ggplot(titanic_train, aes(Age, log(Fare), color = factor(Survived), shape = factor(Sex))) +
  geom_point() +
  geom_jitter()

# 데이터 전처리

# train, test 구분
titanic_train$IsTrainSet <- TRUE
titanic_test$IsTrainSet <- FALSE

# test에 Survived 추가
titanic_test$Survived <- NA

# train + test
titanic_full <- rbind(titanic_test, titanic_train)
table(titanic_full$IsTrainSet)

# 결측값 처리
## Embarked 결측값을 임의로 "S" 처리
table(titanic_full$Embarked)
titanic_full[titanic_full$Embarked == "", "Embarked"] <- "S"

## Age 처리, NA에 평균값 넣기
table(is.na(titanic_full$Age))
age.median <- median(titanic_full$Age, na.rm = TRUE)
titanic_full[is.na(titanic_full$Age), "Age"] <- age.median

## Fare 처리, NA에 평균값 넣기
fare.median <- median(titanic_full$Fare, na.rm = TRUE)
titanic_full[is.na(titanic_full$Fare), "Fare"] <- fare.median
table(is.na(titanic_full$Fare))

# Factor형으로 변환
str(titanic_full)
titanic_full$Pclass <- as.factor(titanic_full$Pclass)
titanic_full$Sex <- as.factor(titanic_full$Sex)
titanic_full$Embarked <- as.factor(titanic_full$Embarked)

# 정제된 데이터를 다시 train과 test로
titanic_train <- titanic_full[titanic_full$IsTrainSet == TRUE, ]
titanic_test <- titanic_full[titanic_full$IsTrainSet == FALSE, ]

titanic_train$Survived <- as.factor(titanic_train$Survived)

# 랜덤 포레스트
survived_equation <-
  "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived_formula <- as.formula(survived_equation)

library(randomForest)

titanic_model <- randomForest(
  formula = survived_formula, data = titanic_train,
  ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic_test)
)

Survived <- predict(titanic_model, newdata = titanic_test)

PassengerId <- titanic_test$PassengerId
output_df <- as.data.frame(PassengerId)
output_df$Survived <- Survived

write.csv(output_df, file = "kaggle_submission.csv", row.names = FALSE)

# gender_submission과 비교, accuracy: 0.8589
refer_df <- read.csv("./R_Study/data/titanic/gender_submission.csv")
refer_df$Survived <- as.factor(refer_df$Survived)
caret::confusionMatrix(data = output_df$Survived, reference = refer_df$Survived)
