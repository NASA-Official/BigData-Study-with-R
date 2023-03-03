#테스트할 파일 읽기
test <- read.csv("test.csv", na.strings = c("", "na", "NA"))
#학습시킬 데이터 읽기
train <- read.csv("train.csv", na.strings = c("", "na", "NA"))

#데이터 꼬라지 확인
str(test)
str(train)

summary(train)

#목표 : test의 Transported 예측

#factor로 변환
test$CryoSleep <- as.factor(test$CryoSleep)
train$CryoSleep <- as.factor(train$CryoSleep)
test$VIP <- as.factor(test$VIP)
train$VIP <- as.factor(train$VIP)
train$Transported <- as.factor(train$Transported)

#결측치 확인
sapply(train, FUN = function(x){sum(is.na(x))})
sapply(test, FUN = function(x){sum(is.na(x))})

#나이 결측치 -> 평균 대치
averageAge <- mean(train$Age, na.rm = TRUE)
testAverageAge <- mean(test$Age, na.rm = TRUE)
train$Age <- ifelse(is.na(train$Age), averageAge, train$Age)
test$Age <- ifelse(is.na(test$Age), testAverageAge, test$Age)

#이름은 의미 없는 데이터이므로 제거
test2 <- subset(test, select=-c(Name))
train2 <- subset(train, select=-c(Name))

str(test2)
str(train2)

##서비스에 지출한 금액이 의미있는 데이터일까? 한 번 제거한 뒤 예측해보자
test_no_service <- test[, 1:7]
train_no_service <- train[, 1:7]
train_no_service$Transported <- train$Transported

str(test_no_service)
str(train_no_service)

#막대 그래프 그리기 위한 라이브러리들
library(ggplot2)
library(scales)

#냉동수면 상태에 따른 생존여부 막대 그래프 출력
ggplot_cryosleep <- ggplot(train_no_service, aes(x = Transported, fill = CryoSleep, width = .8)) + geom_bar() + scale_y_continuous(breaks= pretty_breaks())
ggplot_cryosleep

#랜덤 포레스트를 이용하여 모델 생성
randomfor <- randomForest::randomForest(Transported~CryoSleep + VIP + Cabin + Destination + Age, data = train_no_service)
randomfor_info <- randomForest::randomForest(Transported~VIP + Cabin + Destination + Age + CryoSleep, data = train_no_service)
## 중요도 확인
randomForest::importance(randomfor_info)
randomForest::importance(randomfor)

## 예측
randomfor_pre <- predict(randomfor, newdata = test_no_service, type="response")

head(randomfor_pre)

predict_result_without_service <- data.frame(PassengerId = test_no_service$PassengerId, Transported = randomfor_pre)
head(predict_result_without_service)
str(predict_result_without_service)

# 서비스 제외 랜덤포레스트 예측 결과 추출
write.csv(predict_result_without_service, file="SpaceTitanic_predict_result_without_service.csv", row.names = FALSE)

#서비스를 고려해볼까? 서비스를 많이 사용한 경우 냉동 수면에 들지 않았을 확률이 높을 것이다!
## 결측치 확인
sapply(train2, FUN = function(x){sum(is.na(x))})
sapply(test2, FUN = function(x){sum(is.na(x))})

#서비스 종류에 상관없이 서비스를 이용했다면 수면에 들지 않았을 것! -> 데이터 압축 가능
train2$totalService <- train2$RoomService + train2$FoodCourt + train2$ShoppingMall + train2$Spa + train2$VRDeck
test2$totalService <- test2$RoomService + test2$FoodCourt + test2$ShoppingMall + test2$Spa + test2$VRDeck

#모든 서비스 사용내역 통합 했으니 각 서비스 별 데이터 삭제
train2 <- subset(train2, select = -c(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck))
test2 <- subset(test2, select = -c(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck))

#총 서비스 이용시간 결측치 치환, 냉동수면 상태라면 0, 냉동수면 아니라면 중앙값 이용. (평균은 왜곡이 너무 심할 수 있다.)
totalServiceMedian <- median(train2$totalService, na.rm=TRUE)
train2$totalService <- ifelse(is.na(train2$totalService & !test2$CryoSleep), totalServiceMedian, train2$totalService)
totalServiceTestMedian <- median(test2$totalService, na.rm=TRUE)
test2$totalService <- ifelse(is.na(test2$totalService) & !test2$CryoSleep, totalServiceTestMedian, test2$totalService)

str(train2)
str(test2)

#랜덤 포레스트를 이용하여 모델 생성
randomfor2 <- randomForest::randomForest(Transported~CryoSleep + totalService + VIP + Cabin + Destination + Age, data = train2)
randomfor_info2 <- randomForest::randomForest(Transported~totalService + VIP + Cabin + Destination + Age + CryoSleep, data = train2)
## 중요도 확인
randomForest::importance(randomfor_info2)
randomForest::importance(randomfor2)

## 예측
randomfor_pre2 <- predict(randomfor2, newdata = test2, type="response")

head(randomfor_pre2)

summary(train)
predict_result_with_service <- data.frame(PassengerId = test2$PassengerId, Transported = randomfor_pre2)
head(predict_result_with_service)
str(predict_result_with_service)

# 서비스 제외 랜덤포레스트 예측 결과 추출
write.csv(predict_result_with_service, file="SpaceTitanic_predict_result_with_service.csv", row.names = FALSE)

##데이터 시각화

str(train)
summary(train)

#나이별 분포
space_titanic_Age <- ggplot(train, aes(x = Age, y = ..density..), main = "", xlab = "Age Distribution") + geom_density(fill="blue", alpha=0.2)
space_titanic_Age

#모자이크

par(mfrow=c(1, 1))
mosaicplot(table(ifelse(train$Transported=="True", "Survived", "Dead"), train$CryoSleep), main = "", cex=1.2, color=TRUE, ylab="CryoSleep")
mosaicplot(table(ifelse(train$Transported=="True", "Survived", "Dead"), train$VIP), main = "", cex=1.2, color=TRUE, ylab="VIP")
mosaicplot(table(ifelse(train$Transported=="True", "Survived", "Dead"), train$HomePlanet), main = "", cex=1.2, color=TRUE, ylab="HomePlanet")
mosaicplot(table(ifelse(train$Transported=="True", "Survived", "Dead"), train$Destination), main = "", cex=1.2, color=TRUE, ylab="Destination")

ggplot(train, aes(Age, VIP, color = factor(Transported), shape=factor(CryoSleep))) + geom_point() + geom_jitter()
