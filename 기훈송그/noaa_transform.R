install.packages("caret", repos = "http://cran.us.r-project.org")
install.packages("GGally", repos = "http://cran.us.r-project.org")
install.packages("inspectdf", repos = "http://cran.us.r-project.org")
install.packages("skimr", repos = "http://cran.us.r-project.org")
install.packages("usethis", repos = "http://cran.us.r-project.org")

library(caret)
library(tidyverse)
library(zoo)
library(ggplot2)
library(GGally)
library(skimr)
library(inspectdf)
library(randomForest)
library(usethis)

all <- read.csv(
    # file = "~/Desktop/AuroraData/aurora_complete.csv",
    # file = "C:/Users/SONG/Documents/R_Study/aurora_complete.csv",
    file = "aurora_complete.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)

all$ELECTRON_DATA_STATE <- as.factor(all$ELECTRON_DATA_STATE)
all$PROTONS_DATA_STATE <- as.factor(all$PROTONS_DATA_STATE)
all$FLUX_PROTON_STATE1 <- as.factor(all$FLUX_PROTON_STATE1)
all$FLUX_PROTON_STATE2 <- as.factor(all$FLUX_PROTON_STATE2)
all$SOLAR_WIND_DATA_STATE <- as.factor(all$SOLAR_WIND_DATA_STATE)
all$YEAR <- as.factor(all$YEAR)
all$MONTH <- as.factor(all$MONTH)
all$DAY <- as.factor(all$DAY)
all$TIME <- as.factor(all$TIME)

all_without <- all %>% select(
    -KP, -GSE_X, -GSE_Y, -GSE_Z,
    -GSM_BX, -GSM_BY, -GSM_BZ, -GSM_LAT, -GSM_LONG
)

scale_model <- preProcess(all_without, method = "range")
regulate <- predict(scale_model, all_without)

# KP
regulate$KP <- all$KP
regulate$GSE_X <- all$GSE_X
regulate$GSE_Y <- all$GSE_Y
regulate$GSE_Z <- all$GSE_Z
regulate$GSM_BX <- all$GSM_BX
regulate$GSM_BY <- all$GSM_BY
regulate$GSM_BZ <- all$GSM_BZ
regulate$GSM_LAT <- all$GSM_LAT
regulate$GSM_LONG <- all$GSM_LONG

regulate <- regulate %>% select(
    "YEAR",
    "MONTH",
    "DAY",
    "TIME",
    "ELECTRON_DATA_STATE",
    "E_38_53",
    "E_175_315",
    "PROTONS_DATA_STATE",
    "P_47_65",
    "P_112_187",
    "P_310_580",
    "P_761_1220",
    "P_1060_1910",
    "GSE_X",
    "GSE_Y",
    "GSE_Z",
    "GSM_BX",
    "GSM_BY",
    "GSM_BZ",
    "GSM_BT",
    "GSM_LAT",
    "GSM_LONG",
    "FLUX_PROTON_STATE1",
    "X_10MeV",
    "FLUX_PROTON_STATE2",
    "X_30MeV",
    "SOLAR_WIND_DATA_STATE",
    "SOLAR_WIND_PROTON_DENSITY",
    "SOLAR_WIND_BULK_SPEED",
    "SOLAR_WIND_ION_TEMPERATURE",
    "KP"
)

regulate <- regulate %>% rename("10MeV" = "X_10MeV")
regulate <- regulate %>% rename("30MeV" = "X_30MeV")

regulate <- regulate %>% rename("tenMeV" = "10MeV")
regulate <- regulate %>% rename("thirtyMeV" = "30MeV")

regulate <- regulate %>% rename("SW_STATE" = "SOLAR_WIND_DATA_STATE")
regulate <- regulate %>% rename("SW_PD" = "SOLAR_WIND_PROTON_DENSITY")
regulate <- regulate %>% rename("SW_BS" = "SOLAR_WIND_BULK_SPEED")
regulate <- regulate %>% rename("SW_IT" = "SOLAR_WIND_ION_TEMPERATURE")
str(regulate)

all <- all %>% rename("SW_STATE" = "SOLAR_WIND_DATA_STATE")
all <- all %>% rename("SW_PD" = "SOLAR_WIND_PROTON_DENSITY")
all <- all %>% rename("SW_BS" = "SOLAR_WIND_BULK_SPEED")
all <- all %>% rename("SW_IT" = "SOLAR_WIND_ION_TEMPERATURE")

ggcorr(regulate, name = "aurora", label = TRUE) +
    theme(legend.position = "none") +
    labs(title = "aurora") +
    theme(plot.title = element_text(
        face = "bold", color = "black", hjust = 0.5, size = 12
    ))

ggcorr(all, name = "aurora_all", label = TRUE) +
    theme(legend.position = "none") +
    labs(title = "aurora_all") +
    theme(plot.title = element_text(
        face = "bold", color = "black", hjust = 0.5, size = 12
    ))

ggpairs(all)
skim(regulate)
skim(all)

inspect_num(regulate) %>% show_plot()

# select regulate
selected_regulate <- regulate %>% select(
    "YEAR",
    "MONTH",
    "DAY",
    "TIME",
    "ELECTRON_DATA_STATE",
    "E_38_53",
    "E_175_315",
    "PROTONS_DATA_STATE",
    "P_47_65",
    "P_112_187",
    "P_310_580",
    "P_761_1220",
    "P_1060_1910",
    "GSM_BZ",
    "GSM_BT",
    "GSM_LAT",
    "FLUX_PROTON_STATE1",
    "tenMeV",
    "FLUX_PROTON_STATE2",
    "thirtyMeV",
    "SW_STATE",
    "SW_PD",
    "SW_BS",
    "SW_IT",
    "KP"
)

table(selected_regulate$SW_STATE)
temp <- selected_regulate %>% filter(selected_regulate$ELECTRON_DATA_STATE == -1)
head(temp, 10)

# 2001년도 데이터
selected_regulate_2001 <- selected_regulate %>% filter(selected_regulate$YEAR == 2001)

# train:test = 7:3으로 랜덤 추출
nrows <- NROW(selected_regulate_2001)
set.seed(2023)
# half <- sample(1:nrows, 0.3 * nrows)
# NROW(half) # 56548
# str(half)
# half_seventy <- half[1:39583]
# half_thirty <- half[39584:56548]

index <- sample(1:nrows, 0.7 * nrows)
# train_2001 <- selected_regulate_2001[index, ]
# test_2001 <- selected_regulate_2001[-index, ]

train <- selected_regulate[index, ]
test <- selected_regulate[-index, ]

usethis::edit_r_environ()

str(test_2001)

aurora_model <- randomForest(KP ~ .,
    data = train_2001,
    do.tracer = 10,
    ntree = 500, proximity = TRUE, importance = TRUE
)

predict_2001 <- predict(aurora_model, test_2001 %>% select(-KP))


View(predict_2001)
str(test_2001$KP)

diff_2001 <- ((test_2001$KP - predict_2001$pred) / test_2001) * 100

model_read <- readRDS(
    "~/Desktop/AuroraData/randomForest2001_Mark1.rda",
)

model_read

summary(model_read)
