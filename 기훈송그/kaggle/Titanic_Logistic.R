# Data Import
titanic_train <-
    read.csv(
        "./data/titanic/train.csv",
        stringsAsFactors = FALSE,
        header = TRUE
    )
titanic_test <-
    read.csv(
        "./data/titanic/test.csv",
        stringsAsFactors = FALSE,
        header = TRUE
    )

# 데이터 전처리
## train, test 구분
titanic_train$IsTrainSet <- TRUE
titanic_test$IsTrainSet <- FALSE

## test에 Survived 추가
titanic_test$Survived <- NA

## train + test
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
titanic_full$Pclass <- as.factor(titanic_full$Pclass)
titanic_full$Sex <- as.factor(titanic_full$Sex)
titanic_full$Embarked <- as.factor(titanic_full$Embarked)

# 정제된 데이터를 다시 train과 test로
titanic_train <- titanic_full[titanic_full$IsTrainSet == TRUE, ]
titanic_test <- titanic_full[titanic_full$IsTrainSet == FALSE, ]
titanic_train$Survived <- as.factor(titanic_train$Survived)

nonvars <- c("PassengerId", "Name", "Ticket", "Cabin", "Embarked")
titanic_train <- titanic_train[, !(names(titanic_train) %in% nonvars)]

glm1 <- glm(Survived ~ ., data = titanic_train, family = binomial)
summary(glm1)

# Fare와 Parch의 p-value가 0.05를 넘어서므로 제거를 해보자.
glm2 <- glm(Survived ~ . - Parch - Fare,
    data = titanic_train,
    family = binomial
)
summary(glm2)

logistic_pred <- predict(glm2, type = "response")
table(titanic_train$Survived, logistic_pred >= 0.5)

Predict1 <- predict(glm1, type = "response", newdata = titanic_test)
Predict2 <- predict(glm2, type = "response", newdata = titanic_test)

Survived1 <- as.numeric(Predict1 >= 0.5)
Survived2 <- as.numeric(Predict2 >= 0.5)

PassengerId <- titanic_test$PassengerId
output_df1 <- as.data.frame(PassengerId)
output_df1$Survived <- as.factor(Survived1)

output_df2 <- as.data.frame(PassengerId)
output_df2$Survived <- as.factor(Survived2)

# gender_submission과 비교, accuracy1: 0.9378, accuracy2: 0.9402
refer_df <- read.csv("./data/titanic/gender_submission.csv")
refer_df$Survived <- as.factor(refer_df$Survived)
caret::confusionMatrix(data = output_df1$Survived, reference = refer_df$Survived)
caret::confusionMatrix(data = output_df2$Survived, reference = refer_df$Survived)

write.csv(output_df1, file = "kaggle_submit_logi1.csv", row.names = FALSE)
write.csv(output_df2, file = "kaggle_submit_logi2.csv", row.names = FALSE)
