#데이터 불러오기
train_df <- read.csv("train.csv", na.strings=c('', ' ', 'NA', NA))
test_df <- read.csv("test.csv")

library(dplyr)

#데이터 꼬라지 확인
train_df %>% str()
train_df %>% summary()
test_df %>% str()
test_df %>% summary()

#factor 형변환
train_df$요일 <- factor(x=train_df$요일, levels=c("월","화", "수", "목", "금"))
test_df$요일 <- factor(x=test_df$요일, levels=c("월","화", "수", "목", "금"))
train_df$일자 <- as.Date(train_df$일자)
test_df$일자 <- as.Date(test_df$일자)

#시각화 라이브러리 설치 및 임포트
install.packages("inspectdf")
library(inspectdf)

#변수간 상관관계 파악
inspect_cor(train_df) %>% show_plot()
inspect_cor(test_df) %>% show_plot()
#NA 값 존재하는 지 시각화
inspect_na(train_df) %>% show_plot()
inspect_na(test_df) %>% show_plot()
#히스토그램
inspect_num(train_df) %>% show_plot()
inspect_num(test_df) %>% show_plot()
#변수 타입 시각화
inspect_types(train_df) %>% show_plot()
inspect_types(test_df) %>% show_plot()

#중식계 석식계 평균
train_df %>% select(중식계, 석식계) %>% colMeans()

library(ggplot2)

#일자에 따른 현본사소속재택근무자 변화 그래프로 그려봄
ggplot(train_df) + aes(x = 일자, y = 현본사소속재택근무자수) + 
  geom_line(size = 0.5, colour = "#112246") + theme_minimal()

train_df %>%
  mutate(corona=factor(ifelse(현본사소속재택근무자수 > 0, 1, 0))) %>%
  group_by(corona, 요일) %>% 
  summarise(중=mean(중식계), 석=mean(석식계))

##일자에 따른 본사 정원 수 변화
ggplot(train_df) + aes(x=일자, y=본사정원수) +
  geom_line(size=0.5, color="#112245") + theme_minimal()

##
train_df %>%
  mutate(최대가능인원=본사정원수 -본사휴가자수 -본사출장자수 -현본사소속재택근무자수,
         휴가비율= 본사휴가자수/본사정원수,
         출장비율 = 본사출장자수/본사정원수,
         시간외근무명령서승인건수비율=본사시간외근무명령서승인건수/본사정원수,
         본사재택근무비율=현본사소속재택근무자수/본사정원수
  ) %>%
  select(-c(본사휴가자수,본사출장자수,본사시간외근무명령서승인건수,현본사소속재택근무자수)) -> train_df_nomalization
test_df %>%
  mutate(최대가능인원=본사정원수 -본사휴가자수 -본사출장자수 -현본사소속재택근무자수,
         휴가비율= 본사휴가자수/본사정원수,
         출장비율 = 본사출장자수/본사정원수,
         시간외근무명령서승인건수비율=본사시간외근무명령서승인건수/본사정원수,
         본사재택근무비율=현본사소속재택근무자수/본사정원수) %>%
  select(-c(본사휴가자수,본사출장자수,본사시간외근무명령서승인건수,현본사소속재택근무자수)) -> test_df_nomalization

summary(train_df_nomalization)
summary(test_df_nomalization)

library(scales)

randomFor_launch <- randomForest::randomForest(중식계~최대가능인원+휴가비율+출장비율+시간외근무명령서승인건수비율 + 본사재택근무비율
                                               + 요일 + 일자, data=train_df_nomalization)
randomFor_dinner <- randomForest::randomForest(석식계~최대가능인원+휴가비율+출장비율+시간외근무명령서승인건수비율 + 본사재택근무비율
                                               + 요일 + 일자, data=train_df_nomalization)

randomForest::importance(randomFor_dinner)

randomfor_predic_launch<- predict(randomFor_launch, newdata = test_df_nomalization, type="response")
randomfor_predic_dinner<- predict(randomFor_dinner, newdata = test_df_nomalization, type="response")

head(randomfor_pre)
summary(randomfor_pre)

predict_result_with_service <- data.frame(일자 = test_df$일자, 중식계 = randomfor_predic_launch, 석식계 = randomfor_predic_dinner)
head(predict_result_with_service)

# # 랜덤포레스트
write.csv(predict_result_with_service, file="predict_meal_count.csv", fileEncoding = 'UTF-8', row.names = FALSE)

read.csv("predict_meal_count.csv") %>% head() 
