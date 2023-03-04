library(tensorflow)
library(keras)
library(dplyr)
library(tidyr)
library(ggplot2)
library(timetk)
library(reticulate)
conda_list()
use_condaenv("r-reticulate")

KP.model <- load_model_tf("lstm_model")

all <- readRDS(
    file = "all_orderby_time.rds"
)

three_days <- all[nrow(all) - 71:nrow(all), ]

# 24h * 3
lag <- 72
scale_factors <- c(mean(all$KP), sd(all$KP))

KP <- three_days$KP

KP_scaled <- (KP - scale_factors[1]) / scale_factors[2]

KP_pred_arr <- array(
    data = KP_scaled,
    dim = c(1, lag, 1)
)

KP.forecast <- KP.model %>%
    predict(KP_pred_arr, batch_size = 1) %>%
    .[, , 1]

KP.forecast <- KP.forecast * scale_factors[2] + scale_factors[1]

str(KP.forecast)
