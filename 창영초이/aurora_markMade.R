

install.packages("caret", repos = "http://cran.us.r-project.org")
install.packages("GGally", repos = "http://cran.us.r-project.org")
install.packages("inspectdf", repos = "http://cran.us.r-project.org")
install.packages("skimr", repos = "http://cran.us.r-project.org")
install.packages("usethis", repos = "http://cran.us.r-project.org")
install.packages("bigmemory-package", repos = "http://cran.us.r-project.org")


library(caret)
library(tidyverse)
library(zoo)
library(ggplot2)
library(GGally)
library(skimr)
library(inspectdf)
library(randomForest)
library(usethis)
library(rstudioapi)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()


all <- read.csv(
    file = "./DataSet/aurora_regulated.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA),
    stringsAsFactor = TRUE
)

all$YEAR <- as.factor(all$YEAR)
all$MONTH <- as.factor(all$MONTH)
all$DAY <- as.factor(all$DAY)
all$TIME <- as.factor(all$TIME)
all$ELECTRON_DATA_STATE <- as.factor(all$ELECTRON_DATA_STATE)
all$PROTONS_DATA_STATE <- as.factor(all$PROTONS_DATA_STATE)
all$FLUX_PROTON_STATE1 <- as.factor(all$FLUX_PROTON_STATE1)
all$FLUX_PROTON_STATE2 <- as.factor(all$FLUX_PROTON_STATE2)
all$SW_STATE <- as.factor(all$SW_STATE)



write.csv(  
    all,
    'aurora_regulated.csv',
    row.names = FALSE
)


saveRDS(
    all,
    file = "./DataSet/allData.rds"
)

allRdsData <- readRDS(
    "./DataSet/allData.rds"
)

str(allRdsData)


rf_model_2005 <- readRDS(
    "./Model/randomForest2005_Mark5.rda"
)

rf_model_2005



# ========================================  N년도 데이터 만 뽑기 ===========================================


selected_regulate_2006 <- selected_regulate %>% filter(selected_regulate$YEAR == 2006)

# train:test = 7:3으로 랜덤 추출
nrows <- NROW(selected_regulate_2006)
set.seed(2023)
# half <- sample(1:nrows, 0.3 * nrows)
# NROW(half) # 56548
# str(half)
# half_seventy <- half[1:39583]
# half_thirty <- half[39584:56548]

index <- sample(1:nrows, 0.7 * nrows)
train_2006 <- selected_regulate_2006[index, ]
test_2006 <- selected_regulate_2006[-index, ]

temp <- selected_regulate_2006[-index, ]
temp <- test_2006$KP

nrow(train_2006)
nrow(test_2006)



refit <- update(
  model,
  KP ~ .,
    data = train_2006,
    do.trace = 10,
    keep.forest = TRUE,
    ntree = 1000,
    proximity = TRUE, 
    importance = TRUE
)


refit


# ======================================== predict RMSE 계산하기 ========================================


temp <- test_2006$KP
temp

result <- predict(
    refit,
    newdata = test_2006
)


RMSE(
  result,
  temp
)



# ======================================== 최종 모델 저장 ===========================================

saveRDS(
    refit,
    file = "./Model/randomForest2006_Mark6.rda"
)

model_read <- readRDS(
    "./Model/randomForest2006_Mark6.rda"
)

model_read
