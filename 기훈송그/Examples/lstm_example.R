# https://blog.naver.com/PostView.naver?blogId=joseb1234&logNo=222260284480&redirect=Dlog&widgetTypeCall=true&directAccess=false

install.packages("timetk", repos = "http://cran.us.r-project.org")

library(tensorflow)
library(keras)
library(dplyr)
library(tidyr)
library(ggplot2)
library(timetk)

head(economics)

# 실업 평균, 표준편차 구하기
scale_factors <- c(mean(economics$unemploy), sd(economics$unemploy))

scaled_train <- economics$unemploy

# 표준화
scaled_train <- economics %>%
    dplyr::select(unemploy) %>%
    dplyr::mutate(
        unemploy = (unemploy - scale_factors[1]) / scale_factors[2]
    )
str(scaled_train)

# keras LSTM 모델은 샘플수, 시계열수, 변수의 수를 원한다
prediction <- 12
lag <- prediction

# 학습 데이터를 행렬 형태로 바꿔주기
scaled_train <- as.matrix(scaled_train)

# 574 - 12 - 12 + 1
length(scaled_train) - lag - prediction + 1

# 데이터 11회 시간차를 구하기((nrow(data)-lag-prediction+1, 12, 1))
x_train_data <- t(sapply(
    1:(length(scaled_train) - lag - prediction + 1),
    function(x) scaled_train[x:(x + lag - 1), 1]
))

# 학습용 데이터를 3차원 배열 형태로 바꿔주기
x_train_arr <- array(
    data = as.numeric(unlist(x_train_data)),
    dim = c(
        nrow(x_train_data),
        lag,
        1
    )
)

# Y값 구하기
y_train_data <- t(sapply(
    (1 + lag):(length(scaled_train) - prediction + 1),
    function(x) scaled_train[x:(x + prediction - 1)]
))

y_train_arr <- array(
    data = as.numeric(unlist(y_train_data)),
    dim = c(
        nrow(y_train_data),
        prediction,
        1
    )
)

# 마지막 12개 관측치를 학습 데이터에서 뽑아내기
x_test <- economics$unemploy[(nrow(scaled_train) - prediction + 1):nrow(scaled_train)]

# 학습을 위해 x_test 데이터를 표준화하기
x_test_scaled <- (x_test - scale_factors[1]) / scale_factors[2]

# 예측할 12개월 데이터를 배열(array)형태로 바꿔주기
x_pred_arr <- array(
    data = x_test_scaled,
    dim = c(1, lag, 1)
)

## LSTM 예측
lstm_model <- keras_model_sequential()

# batch size는 소그룹의 규모
lstm_model %>%
    layer_lstm(
        units = 50, # size of the Layer
        batch_input_shape = c(1, 12, 1), # batch size, timesteps, features
        return_sequences = TRUE,
        stateful = TRUE
    ) %>%
    # fraction of the units to drop for the linear transformation of the inputs
    layer_dropout(rate = 0.5) %>%
    time_distributed(keras::layer_dense(units = 1))

# loss함수는 mae(mean absolute error), 최적화 함수는 adam, 측정기준은 정확도
lstm_model %>%
    compile(
        loss = "mae",
        optimizer = "adam",
        metrics = "accuracy"
    )

# shuffle = FALSE로 시계열 연속성 유지
lstm_model %>% fit(
    x = x_train_arr,
    y = y_train_arr,
    batch_size = 1,
    epochs = 20,
    verbose = 0,
    shuffle = FALSE
)

# lstm 모형으로 예측
lstm_forecast <- lstm_model %>%
    predict(x_pred_arr, batch_size = 1) %>%
    .[, , 1]

# 예측값을 원래 값으로 되돌리기
lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]

fitted <- predict(lstm_model, x_train_arr, batch_size = 1) %>%
    .[, , 1]

asdf <- fitted

# 551 12
dim(fitted)

str(asdf)

if (dim(fitted)[2] > 1) {
    print("case 1")
    fit <- c(fitted[, 1], fitted[dim(fitted)[1], 2:dim(fitted)[2]])
} else {
    print("case 2")
    fit <- fitted[, 1]
}
str(fitted)
str(fit)

# make fit to be original scale
fitted <- fit * scale_factors[2] + scale_factors[1]

# 첫 번째 예측 값 지워주기
fitted <- c(rep(NA, lag), fitted)

lstm_forecast <- timetk::tk_ts(lstm_forecast,
    start = c(2015, 5),
    end = c(2016, 4),
    frequency = 12
)

# 입력 시계열
input_ts <- timetk::tk_ts(
    economics$unemploy,
    start = c(1967, 7),
    end = c(2015, 4),
    frequency = 12
)

# 예측값 객체 정의
forecast_list <- list(
    model = NULL,
    method = "LSTM",
    mean = lstm_forecast,
    x = input_ts,
    fitted = fitted,
    residuals = as.numeric(input_ts) - as.numeric(fitted)
)

class(forecast_list) <- "forecast"

forecast::autoplot(forecast_list)

forecast_list
