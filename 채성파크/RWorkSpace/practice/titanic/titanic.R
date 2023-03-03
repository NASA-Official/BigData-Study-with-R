#test파일 불러오기
test_df <- read.csv("test.csv")
#학습 파일 불러오기
train_df <- read.csv("train.csv")

str(test_df)
str(train_df)

#char -> factor 형 변환
train_df$Sex <- as.factor(train_df$Sex)
train_df$Embarked <- as.factor(train_df$Embarked)
#int -> factor 형 변환
train_df$Pclass <- as.factor(train_df$Pclass)
#Survived 생존 여부에 따라 No, Yes로 바꾸고 이를 factor로 변환
train_df$Survived <- as.factor(ifelse(train_df$Survived == 0, train_df$Survived <- "No", train_df$Survived <- "Yes"))

#테스트 할 파일도 형변환
test_df$Sex <- as.factor(test_df$Sex)
test_df$Embarked <- as.factor(test_df$Embarked)
test_df$Pclass <- as.factor(test_df$Pclass)

#결측치 확인
sapply(test_df, FUN = function(x) {sum(is.na(x))})
sapply(train_df, FUN = function(x) {sum(is.na(x))})

summary(test_df)
summary(train_df)

#결측치 제거
#na.omit()사용

#결측지 대체, 평균값으로(평균대치법)
test_df$Age <- ifelse(is.na(test_df$Age) == TRUE, mean(test_df$Age, na.rm = TRUE), test_df$Age)
train_df$Age <- ifelse(is.na(train_df$Age) == TRUE, mean(train_df$Age, na.rm = TRUE), train_df$Age)

library(ggplot2)
library(scales)
library(grid)

plot(train_df)
titanic_fare <- ggplot(train_df, aes(x = Fare, y = ..density.. ), main = "", xlab="Fare Distribution") + geom_density(fill="blue", alpha=0.3)
titanic_age <- ggplot(train_df, aes(x = Age, y = ..density.. ), main = "", xlab="Age Distribution") + geom_density(fill="blue", alpha=0.3)

titanic_fare
titanic_age

#성별에 따른 생존여부 막대그래프 그리기
ggplot_sex <- ggplot(train_df, aes(x = Survived,fill = Sex, width = .8)) + geom_bar() + scale_y_continuous(breaks= pretty_breaks())
ggplot_sex

#랜덤포레스트 사용 예측
## 모델 생성
randomfor <- randomForest::randomForest(Survived~Pclass + Age + Sex, data = train_df)
randomfor_info <- randomForest::randomForest(Survived~Sex + Age + Pclass, data = train_df)
## 중요도 확인
randomForest::importance(randomfor_info)
## 예측
randomfor_pre <- predict(randomfor, newdata = test_df, type="response")
Titanic_randomFor <- data.frame(PassengerId = test_df$PassengerId, Survived = randomfor_pre)
## Survived 0, 1로 다시 변경
Titanic_randomFor$Survived <- as.factor(ifelse(Titanic_randomFor$Survived == "No", 0, 1))
head(Titanic_randomFor)
str(Titanic_randomFor)

refer_df <- read.csv("gender_submission.csv")
refer_df$Survived <- as.factor(refer_df$Survived)
str(refer_df)
caret::confusionMatrix(data = Titanic_randomFor$Survived, reference = refer_df[,2])
#정확도 97.61%

# 로지스틱 회귀
train_df <- read.csv("train.csv")
test_df <- read.csv("test.csv")
full_df <- dplyr::bind_rows(train_df, test_df)

sapply(test_df, FUN = function(x){sum(is.na(x))})

#결측지 대체, 평균값으로(평균대치법)
test_df$Age <- ifelse(is.na(test_df$Age) == TRUE, mean(test_df$Age, na.rm = TRUE), test_df$Age)
train_df$Age <- ifelse(is.na(train_df$Age) == TRUE, mean(train_df$Age, na.rm = TRUE), train_df$Age)

nonvars = c("PassengerId", "Name", "Ticket", "Cabin", "Embarked")
train_df <- train_df[, !(names(train_df) %in% nonvars)]

glm1 <- glm(Survived ~ ., data = train_df, family = binomial)
summary(glm1)

#Fare와 Parch의 p값이 0.05를 넘어 제거하는 게 이로움
glm2 <- glm(Survived ~ . - Parch - Fare, data = train_df, family = binomial)
summary(glm2)

logistic_pred <- predict(glm2, type = "response")
table(train_df$Survived, logistic_pred >= 0.5)
predictTest = predict(glm2, type = "response", newdata = test_df)
test_df$Survived = as.numeric(predictTest >= 0.5)
table(test_df$Survived)

predictions = data.frame(test_df[c("PassengerId", "Survived")])

#검증
refer_df <- read.csv("gender_submission.csv")
refer_df$Survived <- as.factor(refer_df$Survived)
levels(refer_df$Survived)
predictions$Survived <- as.factor(predictions$Survived)

caret::confusionMatrix(predictions$Survived, refer_df$Survived )   

# # 랜덤포레스트
# write.csv(Titanic_randomFor, file="Titanic_randomFor.csv", row.names = FALSE)

# 로지스틱 회귀 
write.csv(file = "logistic_pred.csv", predictions, row.names = FALSE)
