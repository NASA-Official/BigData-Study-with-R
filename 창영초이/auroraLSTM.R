install.packages("tseries", repos = "http://cran.us.r-project.org")
install.packages("forecast", repos = "http://cran.us.r-project.org")
install.packages("xts", repos = "http://cran.us.r-project.org")
install.packages("fpp3", repos = "http://cran.us.r-project.org")


# only KP
library(rstudioapi)
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(xts)
library(fpp3)
library(fable)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

all <- readRDS(
  "./dataSet/all_orderby_time.rds"
)


all$FLUX_PROTON_STATE1 <- as.factor(all$FLUX_PROTON_STATE1)
all$FLUX_PROTON_STATE2 <- as.factor(all$FLUX_PROTON_STATE2)
all$ELECTRON_DATA_STATE <- as.factor(all$ELECTRON_DATA_STATE)
all$PROTONS_DATA_STATE <- as.factor(all$PROTONS_DATA_STATE)

summary(all)
head(all)

temp <- as_datetime(
  all$DATE,
  tz = 'UTC'
)


all$KP <- as.double(all$KP)
head(all$KP)


temp <- all %>% as_tsibble(
  index = DATE
)



tslm_model <- temp %>%
  model(tslm = 
    TSLM(
      KP ~ ELECTRON_DATA_STATE + E_38_53 + E_175_315 + PROTONS_DATA_STATE + P_47_65 + P_112_187 + P_310_580 + P_761_1220 + P_1060_1910 + GSM_BT + GSM_LAT + FLUX_PROTON_STATE1  + FLUX_PROTON_STATE2 +  SW_STATE + SW_PD + SW_PD + SW_IT 
    )
)



tslm_model %>% augment()

library(scales)



test <- temp %>% model(
  TSLM(formula(KP ~ trend() + season()))
)

test %>% report()
test %>% glance()
pred <- test %>% forecast(
    h = 100
)

pred

pred %>% autoplot(temp)

print(n = 340 , pred)




