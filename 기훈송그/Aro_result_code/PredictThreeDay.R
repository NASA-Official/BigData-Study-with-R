predictThreeDayKP <- function(KP_data) {
    # 72개의 이상의 regulate를 넣으면 72개만
    # 장기 예측에 유리한 LSTM
    # KP column이 있어야 사용할 수 있음
    list.of.packages <- c("tensorflow", "keras")
    new.packages <- list.of.packages[
        !(list.of.packages %in% installed.packages()[, "Package"])
    ]
    if (length(new.packages)) {
        install.packages(new.packages, repos = "http://cran.us.r-project.org")
    }
    library(tensorflow)
    library(keras)
    KP_model <- load_model_tf("lstm_model")
    some_days <- KP_data[((nrow(KP_data) - 71):nrow(KP_data)), ]
    # 24h * number
    lag <- 72
    scale_factors <- c(mean(KP_data$KP), sd(KP_data$KP))
    KP <- some_days$KP
    KP_scaled <- (KP - scale_factors[1]) / scale_factors[2]
    KP_pred_arr <- array(
        data = KP_scaled,
        dim = c(1, lag, 1)
    )
    KP_forecast <- KP_model %>%
        predict(KP_pred_arr, batch_size = 1) %>%
        .[, , 1]
    KP.forecast <- KP_forecast * scale_factors[2] + scale_factors[1]
    return(KP.forecast)
}
