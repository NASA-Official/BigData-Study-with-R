library(dplyr)
library(lubridate)
library(tidyverse)

main <- read.csv(
  file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/stroke_/train.csv',
  encoding = 'UTF-8'
)

summary(main)
head(main$gender)


ds1 <- main
ds1$age <- as.numeric(ds1$age)
summary(ds1)

colSums(is.na(ds1))
naFunction <- sapply(ds1, function(x) {
  sum(is.na(x))
})




temp1 <- ds1 %>% filter(gender == 'Male') %>% summarise(avg = mean(age, na.rm = T))
head(temp1)

# bmi컬럼의 결측치를 bmi컬럼의 결측치를 제외한 나머지 값들의 중앙값으로 채웠을 경우 bmi 컬럼의 평균을 소숫점 이하 3자리 까지 구하여라
ds2 <- main

colSums(is.na(ds2))

nonNaBmiMedian <- median(ds2$bmi, na.rm = T)

ds2$bmi[is.na(ds2$bmi) == T] <- nonNaBmiMedian
options(digits = 5)
mean(ds2$bmi)


# bmi컬럼의 각 결측치들을 결측치를 가진 환자 나이대(10단위)의 평균 bmi 값으로 대체한 후 대체된 bmi 컬럼의 평균을 소숫점 이하 3자리 까지 구하여라

# 실기 2유형 ==========================================================================================
library(rpart)
library(caret)
library(readr)
library(dplyr)
library(tidyr)
library(ModelMetrics)
library(randomForest)

train <- read.csv(
  file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/churn/train.csv',
  encoding = 'UTF-8',
  na.strings = c('', ' ', 'NA', NA)
)

test <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/churn/test.csv',
  encoding = 'UTF-8',
  na.strings = c('', ' ', 'NA', NA)
)

sub <- read.csv(
    file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/churn/submission.csv',
  encoding = 'UTF-8',
  na.strings = c('', ' ', 'NA', NA)
)

summary(train)

# 결측값 없음
colSums(is.na(train))
colSums(is.na(test))

summary(train)
head(train)


# 필요없는 컬럼은 제거하기
train <- train[, -c(1, 2, 3)]
test <- test[, -c(1, 2, 3)]
head(test)


summary(train)
head(train)


# 타입 변환
train$Gender <- as.factor(train$Gender)
test$Gender <- as.factor(test$Gender)

# train$HasCrCard
train$HasCrCard <- ifelse(train$HasCrCard == 1, 'Yes', 'No')
train$HasCrCard <- as.factor(train$HasCrCard)

test$HasCrCard <- ifelse(test$HasCrCard == 1, 'Yes', 'No')
test$HasCrCard <- as.factor(test$HasCrCard)

# train$Exited
train$Exited <- ifelse(train$Exited == 1, 'Yes', 'No')
train$Exited <- as.factor(train$Exited)

# NumOfProducts
train$NumOfProducts <- as.factor(train$NumOfProducts)
test$NumOfProducts <- as.factor(test$NumOfProducts)

# Geography
train$Geography <- as.factor(train$Geography)
test$Geography <- as.factor(test$Geography)


# Tenure
train$Tenure <- as.factor(train$Tenure)
test$Tenure <- as.factor(test$Tenure)


head(train, 10)
summary(train)


# AIC체크

head(train$Exited, 12)

fm <- glm(
  Exited ~ ., 
  family = binomial,
  data = train
)

summary(train)
fm


step(fm, direction="both")


fm <- formula(Exited ~ Geography + Gender + Age + Balance + NumOfProducts + HasCrCard + IsActiveMember)
fm


# 데이터 스케일링
model <- preProcess(
  train,
  method = c('range'))


train <- predict(model, train)

rf <- randomForest(
  Exited ~ .,
  train,
  ntree = 400,
  do.trace = TRUE
)

pred <- predict(
  object = rf,
  newdata = test
)

list <- sub
list <- as.factor(list)


