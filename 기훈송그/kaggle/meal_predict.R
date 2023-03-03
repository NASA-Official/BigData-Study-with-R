# 구내식당 식수 인원 예측 AI 경진대회
## https://youtu.be/dtmXfrzTGOM
train <-
    read.csv(
        "./data/meal_predict/train.csv",
        stringsAsFactors = FALSE,
        header = TRUE
    )
test <-
    read.csv(
        "./data/meal_predict/test.csv",
        stringsAsFactors = FALSE,
        header = TRUE
    )

View(test)

train$요일 <- ordered(train$요일, levels = c("월", "화", "수", "목", "금"))
test$요일 <- ordered(test$요일, levels = c("월", "화", "수", "목", "금"))

install.packages("skimr", repos = "http://cran.us.r-project.org")
library(skimr)

skim(train)

install.packages("inspectdf", repos = "http://cran.us.r-project.org")
library(inspectdf)
inspect_cor(train) %>% show_plot()
inspect_na(train) %>% show_plot()
inspect_num(train) %>% show_plot()
inspect_types(train) %>% show_plot()

# 결측치, *자기개발의 날* 대체
train$석식메뉴 <- replace(train$석식메뉴, nchar(train$석식메뉴) < 10, "*")

# 전체 날짜의 평균 식수인원, 중식계: 890.3344, 석식계: 461.7726
train %>%
    select(중식계, 석식계) %>%
    colMeans()

train$일자 <- as.POSIXct(train$일자)
test$일자 <- as.POSIXct(test$일자)

library(ggplot2)
ggplot(data = train) +
    aes(x = 일자, y = 현본사소속재택근무자수) +
    geom_line(size = 0.5, color = "blue")

train$현본사소속재택근무자수

# 재택근무라는 개념이 코로나 이후에 생김
# 코로나 이후의 평균 식수인원, 중식계: 890.3344, 석식계: 461.7726
train %>%
    mutate(corona = factor(ifelse(현본사소속재택근무자수 > 0, 1, 0))) %>%
    group_by(corona, 요일) %>%
    summarize(중 = mean(중식계), 석 = mean(석식계))

# 본사 정원수는 꾸준히 증가하기 때문에 식수인원 비율로 변환해서 확인해야 함
ggplot(data = train) +
    aes(x = 일자, y = 본사정원수) +
    geom_line(size = 0.5, color = "blue")

ratio1 <- train %>%
    mutate(
        최대가능인원 = 본사정원수 - 본사휴가자수 - 본사출장자수 - 현본사소속재택근무자수,
        휴가비율 = 본사휴가자수 / 본사정원수,
        출장비율 = 본사출장자수 / 본사정원수,
        시간외근무명령서승인건수비율 = 본사시간외근무명령서승인건수 / 본사정원수,
        본사재택근무비율 = 현본사소속재택근무자수 / 본사정원수,
        중식계비율 = 중식계 / 최대가능인원,
        석식계비율 = 석식계 / 최대가능인원,
        점심저녁차이 = 중식계비율 - 석식계비율
    ) %>%
    select(-c(본사휴가자수, 본사출장자수, 본사시간외근무명령서승인건수, 현본사소속재택근무자수))

ratio1

ratio2 <- ratio1 %>%
    mutate(corona = factor(ifelse(본사재택근무비율 > 0, 1, 0))) %>%
    group_by(corona, 요일)

# 요일별로 점심을 먹는 비율
ggplot(ratio1) +
    aes(x = 요일, y = 중식계비율) +
    geom_boxplot(shape = "circle", fill = "#112446")

# 언제 야근을 제일 많이 할까?
ggplot(ratio1) +
    aes(x = 일자, y = 시간외근무명령서승인건수비율) +
    geom_line(size = 0.5, color = "#112446")

# 비율이 0.2 이상 넘어가는 날들을 확인해보자
ratio2 <- ratio1 %>%
    filter(시간외근무명령서승인건수비율 > 0.2) %>%
    arrange(-시간외근무명령서승인건수비율) %>%
    select(일자, 요일, 시간외근무명령서승인건수비율)

library(lubridate)

# 12월, 3월, 2월에 야근을 많이 한다.
table(month(ratio2$일자)) %>% plot()

# 수요일과 금요일에 야근을 안한다.
table(ratio2$요일, month(ratio2$일자))

# 연도별 월별 야근 비율
table(year(ratio2$일자), month(ratio2$일자))

# test 데이터 셋에 대해서도 확인
test_ratio1 <- test %>%
    mutate(
        최대가능인원 = 본사정원수 - 본사휴가자수 - 본사출장자수 - 현본사소속재택근무자수,
        휴가비율 = 본사휴가자수 / 본사정원수,
        출장비율 = 본사출장자수 / 본사정원수,
        시간외근무명령서승인건수비율 = 본사시간외근무명령서승인건수 / 본사정원수,
        본사재택근무비율 = 현본사소속재택근무자수 / 본사정원수
    ) %>%
    select(-c(본사휴가자수, 본사출장자수, 본사시간외근무명령서승인건수, 현본사소속재택근무자수))

test_ratio2 <- test_ratio1 %>%
    filter(시간외근무명령서승인건수비율 > 0.2) %>%
    arrange(-시간외근무명령서승인건수비율) %>%
    select(일자, 요일, 시간외근무명령서승인건수비율)

table(month(test_ratio2$일자))
table(test_ratio2$요일, month(test_ratio2$일자))
table(year(test_ratio2$일자), month(test_ratio2$일자))

# 연휴 전후에 대한 변수 추가
holiday <- rep(0, 1205)
weekday <- as.numeric(train$요일)
for (i in 1:(length(weekday) - 1)) {
    # 월화수목은 다음날이 화수목금이 아니면 휴가 전날
    if (weekday[i] >= 1 & weekday[i] <= 4) {
        if (weekday[i] != weekday[i + 1] - 1) {
            holiday[i] <- 1 # 연휴전날
            holiday[i + 1] <- 2 # 연휴다음날
            i = i + 1 # 하루스킵
        }
    } else {
        if (weekday[i + 1] != 1) {
            holiday[i] <- 1 # 연휴전날
            holiday[i + 1] <- 2 # 연휴다음날
            i = i + 1
        }
    }
}
table(holiday)

# test 데이터 셋에 대해서도 동일하게
test_holiday <- rep(0, 50)
weekday <- as.numeric(test$요일)
for (i in 1:(length(weekday) - 1)) {
    # 월화수목은 다음날이 화수목금이 아니면 휴가 전날
    if (weekday[i] >= 1 & weekday[i] <= 4) {
        if (weekday[i] != weekday[i + 1] - 1) {
            test_holiday[i] <- 1 # 연휴전날
            test_holiday[i + 1] <- 2 # 연휴다음날
            i <- i + 1 # 하루스킵
        }
    } else {
        if (weekday[i + 1] != 1) {
            test_holiday[i] <- 1 # 연휴전날
            test_holiday[i + 1] <- 2 # 연휴다음날
            i <- i + 1
        }
    }
}
table(test_holiday)

temp1 <- ratio1 %>%
    group_by(month(일자)) %>%
    summarise(mean(점심저녁차이))

# 모델링
# 중식계 모델

library(recipes)
base_rec_lunch <- train %>%
    recipe(중식계 ~ .) %>%
    step_mutate(
        최대가능인원 = 본사정원수 - 본사휴가자수 - 본사출장자수 - 현본사소속재택근무자수,
        최대가능인원비율 = 최대가능인원 / 본사정원수,
        휴가비율 = 본사휴가자수 / 본사정원수,
        출장비율 = 본사출장자수 / 본사정원수,
        시간외근무명령서승인건수비율 = 본사시간외근무명령서승인건수 / 본사정원수,
        본사재택근무비율 = 현본사소속재택근무자수 / 본사정원수,
        연 = year(일자),
        월 = month(일자),
        일 = day(일자)
    ) %>%
    step_rm(
        일자, 본사정원수, 본사휴가자수,
        본사출장자수, 본사시간외근무명령서승인건수,
        현본사소속재택근무자수
    ) %>%
    prep()

test$중식계 <- train$중식계[1:50]
test$석식계 <- train$석식계[1:50]

train_base_lunch <- base_rec_lunch %>% juice()
test_base_lunch <- bake(base_rec_lunch, test)

train_base_lunch <- train_base_lunch %>% mutate(holiday = factor(holiday))
test_base_lunch <- test_base_lunch %>% mutate(holiday = factor(test_holiday))

train_base_lunch <- train_base_lunch %>%
    select(-c(조식메뉴, 중식메뉴, 석식메뉴))

test_base_lunch <- test_base_lunch %>%
    select(-c(조식메뉴, 중식메뉴, 석식메뉴))

# 석식계
base_rec_dinner <- train %>%
    recipe(석식계 ~ .) %>%
    step_mutate(
        최대가능인원 = 본사정원수 - 본사휴가자수 - 본사출장자수 - 현본사소속재택근무자수,
        최대가능인원비율 = 최대가능인원 / 본사정원수,
        휴가비율 = 본사휴가자수 / 본사정원수,
        출장비율 = 본사출장자수 / 본사정원수,
        시간외근무명령서승인건수비율 = 본사시간외근무명령서승인건수 / 본사정원수,
        본사재택근무비율 = 현본사소속재택근무자수 / 본사정원수,
        연 = year(일자),
        월 = month(일자),
        일 = day(일자)
    ) %>%
    step_rm(
        일자, 본사정원수, 본사휴가자수,
        본사출장자수, 본사시간외근무명령서승인건수,
        현본사소속재택근무자수
    ) %>%
    prep()

train_base_dinner <- base_rec_dinner %>% juice()
test_base_dinner <- bake(base_rec_dinner, test)

train_base_dinner <- train_base_dinner %>%
    select(-c(조식메뉴, 중식메뉴, 석식메뉴))

test_base_dinner <- test_base_dinner %>%
    select(-c(조식메뉴, 중식메뉴, 석식메뉴))

# 랜덤 포레스트
set.seed(125)

install.packages("parsnip", repos = "http://cran.us.r-project.org")
install.packages("workflows", repos = "http://cran.us.r-project.org")
install.packages("ranger", repos = "http://cran.us.r-project.org")
install.packages("vip", repos = "http://cran.us.r-project.org")

library(parsnip)
library(workflows)
library(vip)

rf_mod <-
    rand_forest() %>%
    set_mode("regression") %>%
    set_engine("ranger", importance = "impurity")

rf_wf <-
    workflow() %>%
    add_model(rf_mod) %>%
    add_formula(중식계 ~ .)

rf_fit <- rf_wf %>% fit(train_base_lunch)

rf_wf2 <-
    workflow() %>%
    add_model(rf_mod) %>%
    add_formula(석식계 ~ .)

rf_fit2 <- rf_wf %>% fit(train_base_dinner)

rf_fit %>%
    pull_workflow_fit() %>%
    vip(num_features = 30)

rf_fit2 %>%
    pull_workflow_fit() %>%
    vip(num_features = 30)

rf_predict1 <- predict(rf_fit, new_data = test_base_lunch)
rf_predict2 <- predict(rf_fit2, new_data = test_base_dinner)

class(rf_predict1$.pred)

output_df <- as.data.frame(rf_predict1$.pred)
output_df$석식계 <- rf_predict2$.pred

names(output_df) <- c("중식계", "석식계")

write.csv(output_df,
    file = "meal_predict.csv", row.names = FALSE,
    fileEncoding = "UTF-8"
)
