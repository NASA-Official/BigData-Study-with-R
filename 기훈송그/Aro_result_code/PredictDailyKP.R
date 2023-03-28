predictDailyKP <- function(regulate, number) {
    # regulate data number개만큼 가져와서 미래의 KP number개를 예측한다
    # 단기 예측에 유리한 random forest
    list.of.packages <- c("lubridate", "dplyr", "zoo", "caret", "randomForest")
    new.packages <- list.of.packages[
        !(list.of.packages %in% installed.packages()[, "Package"])
    ]
    if (length(new.packages)) {
        install.packages(new.packages, repos = "http://cran.us.r-project.org")
    }
    library(lubridate)
    library(dplyr)
    library(zoo)
    library(caret)
    library(randomForest)
    aurora_model <- readRDS(
        "randomForest2023_Mark23.rda"
    )
    temp <- number - 1
    some_days <- regulate[(nrow(regulate) - temp):nrow(regulate), ]
    result <- predict(
        aurora_model,
        some_days
    )
    return(result)
}
