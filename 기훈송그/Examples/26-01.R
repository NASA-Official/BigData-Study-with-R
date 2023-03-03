# caret
install.packages("caret", repos = "http://cran.us.r-project.org")

library(caret)
ctrl <- trainControl(
    method = "repeatedcv", repeats = 3, number = 5,
    summaryFunction = defaultSummary, allowParallel = TRUE
)

gamGrid <- data.frame(
    select = c(TRUE, TRUE, FALSE, FALSE),
    method = c("GCV.Cp", "REML", "GCV.Cp", "REML"),
    stringsAsFactors = FALSE
)
gamGrid

df <- read.table(
    "http://jaredlander.com/data/acs_ny.csv",
    sep = ",", header = TRUE, stringsAsFactors = FALSE
)
acs <- tibble::as_tibble(
    read.table(
        "http://jaredlander.com/data/acs_ny.csv",
        sep = ",", header = TRUE, stringsAsFactors = FALSE
    )
)

library(plyr)
library(dplyr)

acs <- acs %>% mutate(
    Income = factor(FamilyIncome >= 150000,
        levels = c(FALSE, TRUE), labels = c("Below", "Above")
    )
)

acsFormula <- Income ~ NumChildren + NumRooms + NumVehicles +
    NumWorkers + OwnRent + ElectricBill +
    FoodStamp + HeatingFuel

ctrl <- trainControl(
    method = "repeatedcv", repeats = 2, number = 5,
    summaryFunction = twoClassSummary,
    classProbs = TRUE, allowParallel = FALSE
)

boostGrid <- expand.grid(
    nrounds = 100, # 최대 반복 횟수
    max_depth = c(2, 6, 10),
    eta = c(0.01, 0.1), # 축소량
    gamma = c(0),
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 0.7
)

set.seed(73615)

install.packages("xgboost", repos = "http://cran.us.r-project.org")
install.packages("DiagrammeR", repos = "http://cran.us.r-project.org")

library(xgboost)
library(DiagrammeR)
boostTuned <- train(acsFormula,
    data = acs,
    method = "xgbTree",
    metric = "ROC",
    trControl = ctrl,
    tuneGrid = boostGrid, nthread = 4
)

boostTuned$results %>% arrange(ROC)

plot(boostTuned)
xgb.plot.multi.trees(boostTuned$finalModel,
    feature_names = boostTuned$coefnames
)

acsNew <- read.table("http://www.jaredlander.com/data/acsNew.csv",
    header = TRUE,
    sep = ",", stringsAsFactors = FALSE
)

predict(boostTuned, newdata = acsNew, type = "raw") %>% head()
predict(boostTuned, newdata = acsNew, type = "prob") %>% head()
