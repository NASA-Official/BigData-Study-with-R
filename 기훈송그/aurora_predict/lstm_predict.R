install.packages("keras", repos = "http://cran.us.r-project.org")

library(tensorflow)
library(keras)
library(dplyr)
library(tidyr)
library(ggplot2)
library(timetk)
library(reticulate)
conda_list()
use_condaenv("r-reticulate")


all <- readRDS(
    file = "all_orderby_time.rds"
)

all$DATE <- as.numeric(all$DATE)

head(all, 2)


# KP의 평균 1.72, 표준 편자 1.32
scale_factors <- c(mean(all$KP), sd(all$KP))

scaled_train <- all$KP

# 표준화
scaled_train <- all %>%
    dplyr::select(KP) %>%
    dplyr::mutate(
        KP = (KP - scale_factors[1]) / scale_factors[2]
    )
str(scaled_train)

# 24h * 3
prediction <- 72
lag <- prediction

scaled_train <- as.matrix(scaled_train)

# 188353
length(scaled_train) - lag - prediction + 1

# 데이터 72회의 시간차 구하기
x_train_data <- t(sapply(
    1:(length(scaled_train) - lag - prediction + 1),
    function(x) scaled_train[x:(x + lag - 1), 1]
))

# 학습용 데이터를 3차원 배열 형태로 바꾸기
x_train_arr <- array(
    data = as.numeric(unlist(x_train_data)),
    dim = c(
        nrow(x_train_data),
        lag,
        1
    )
)

# y값
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

# 마지막 72개 관측치를 학습 데이터에서 뽑아내기
x_test <- all$KP[(nrow(scaled_train) - prediction + 1):nrow(scaled_train)]

# 표준화 진행
x_test_scaled <- (x_test - scale_factors[1]) / scale_factors[2]

# 예측할 72시간 데이터를 배열 형태로
x_pred_arr <- array(
    data = x_test_scaled,
    dim = c(1, lag, 1)
)

lstm_model <- keras_model_sequential()

lstm_model %>%
    layer_lstm(
        units = 50,
        batch_input_shape = c(1, 72, 1),
        return_sequences = TRUE,
        stateful = TRUE
    ) %>%
    layer_dropout(rate = 0.5) %>%
    time_distributed(keras::layer_dense(units = 1))

lstm_model %>%
    compile(
        loss = "mae",
        optimizer = "adam",
        metrics = "accuracy"
    )

lstm_model %>% fit(
    x = x_train_arr,
    y = y_train_arr,
    batch_size = 1,
    epochs = 20,
    verbose = 1,
    shuffle = FALSE
)

backup_lstm_model <- lstm_model

save_model_tf(lstm_model, "lstm_model/")

############### 여기 부터 돌려야됨 ############################

lstm_forecast <- lstm_model %>%
    predict(x_pred_arr, batch_size = 1) %>%
    .[, , 1]

lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]

fitted <- predict(lstm_model, x_train_arr, batch_size = 1) %>%
    .[, , 1]
backup_fitted <- fitted

# 188353 72
dim(fitted)

if (dim(fitted)[2] > 1) {
    print("case 1")
    fit <- c(fitted[, 1], fitted[dim(fitted)[1], 2:dim(fitted)[2]])
} else {
    print("case 2")
    fit <- fitted[, 1]
}

# make fit to be original scale
fitted <- fit * scale_factors[2] + scale_factors[1]

View(fitted)

str(fitted)

# 첫 번째 예측 값 지워주기
fitted <- c(rep(NA, lag), fitted)

View(fitted)

str(fitted)

lstm_forecast <- timetk::tk_ts(lstm_forecast,
    start = c(2001, 5091),
    end = c(2023, 673),
    frequency = 24 * 365.25
)

# 입력 시계열
input_ts <- timetk::tk_ts(
    all$KP,
    start = c(2023, 674),
    end = c(2023, 745),
    frequency = 24 * 365.25
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

str(forecast_list$x)

tail(forecast_list$x, 72)
