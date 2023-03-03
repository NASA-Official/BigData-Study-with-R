install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("zoo", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(zoo)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# getwd()
fi_fi <- read.csv(
    file = "~/Desktop/AuroraData/fi_fi_final.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)

colnames(fi_fi)

ace_epam_1h <- read.csv(
    # file = "C:/Users/multicampus/Desktop/AuroraData/ace_epam_1h.csv",
    file = "~/Desktop/AuroraData/ace_epam_1h.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)
ace_epam_1h <- ace_epam_1h %>% rename("P_1060_1910" = "P_060_1910")

# -1.00e+05 값 NA로, 선형보간
ace_epam_1h[ace_epam_1h == -1.00e+05] <- NA
ace_epam_1h <- ace_epam_1h %>% mutate_all(., .funs = funs(na.approx(., method = "linear", rule = 2)))
head(ace_epam_1h)

ace_loc_1h <- read.csv(
    # file = "C:/Users/multicampus/Desktop/AuroraData/ace_loc_1h.csv",
    file = "~/Desktop/AuroraData/ace_loc_1h.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)

# -999.9 값 NA로, 선형보간
ace_loc_1h[ace_loc_1h == -999.9] <- NA
ace_loc_1h <- ace_loc_1h %>% mutate_all(., .funs = funs(na.approx(., method = "linear", rule = 2)))

ace_loc_1h <- ace_loc_1h %>% rename("GSE_X" = "GES_X")
ace_loc_1h <- ace_loc_1h %>% rename("GSE_Y" = "GES_Y")
ace_loc_1h <- ace_loc_1h %>% rename("GSE_Z" = "GES_Z")

ace_mag_1h <- read.csv(
    # file = "C:/Users/multicampus/Desktop/AuroraData/ace_mag_1h.csv",
    file = "~/Desktop/AuroraData/ace_mag_1h.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)

# -999.9 값 NA로, 선형보간
ace_mag_1h[ace_mag_1h == -999.9] <- NA
ace_mag_1h <- ace_mag_1h %>% mutate_all(., .funs = funs(na.approx(., method = "linear", rule = 2)))

ace_sis_1h <- read.csv(
    # file = "C:/Users/multicampus/Desktop/AuroraData/ace_sis_1h.csv",
    file = "~/Desktop/AuroraData/ace_sis_1h.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)

# -1.00e+05 값 NA로, 선형보간
ace_sis_1h[ace_sis_1h == -1.00e+05] <- NA
ace_sis_1h <- ace_sis_1h %>% mutate_all(., .funs = funs(na.approx(., method = "linear", rule = 2)))

ace_swepam_1h <- read.csv(
    # file = "C:/Users/multicampus/Desktop/AuroraData/ace_swepam_1h.csv",
    file = "~/Desktop/AuroraData/ace_swepam_1h.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)

# -9999.9를 NA로 대체
ace_swepam_1h$SOLAR_WIND_PROTON_DENSITY <- replace(
    ace_swepam_1h$SOLAR_WIND_PROTON_DENSITY,
    ace_swepam_1h$SOLAR_WIND_PROTON_DENSITY == -9999.9,
    NA
)
ace_swepam_1h$SOLAR_WIND_BULK_SPEED <- replace(
    ace_swepam_1h$SOLAR_WIND_BULK_SPEED,
    ace_swepam_1h$SOLAR_WIND_BULK_SPEED == -9999.9,
    NA
)

# -1.00e+05를 NA로 대체
ace_swepam_1h$SOLAR_WIND_ION_TEMPERATURE <- replace(
    ace_swepam_1h$SOLAR_WIND_ION_TEMPERATURE,
    ace_swepam_1h$SOLAR_WIND_ION_TEMPERATURE == -1.00e+05,
    NA
)
# 선형 보간
ace_swepam_1h <- ace_swepam_1h %>% mutate_all(., .funs = funs(na.approx(., method = "linear", rule = 2)))

kp_ap_data <- read.csv(
    # file = "C:/Users/multicampus/Desktop/AuroraData/kp_ap_data.csv",
    file = "~/Desktop/AuroraData/kp_ap_data.csv",
    fileEncoding = "UTF-8",
    na.strings = c("", " ", "NA", NA)
)
kp_ap_data <- kp_ap_data %>% rename("TIME" = "START_TIME")
kp_ap_data$TIME <- kp_ap_data$TIME * 100

all <- left_join(
    ace_epam_1h,
    ace_loc_1h,
    by = c("YEAR", "MONTH", "DAY", "TIME")
)

all <- left_join(
    all,
    ace_mag_1h,
    by = c("YEAR", "MONTH", "DAY", "TIME")
)

all <- left_join(
    all,
    ace_sis_1h,
    by = c("YEAR", "MONTH", "DAY", "TIME")
)

all <- left_join(
    all,
    ace_swepam_1h,
    by = c("YEAR", "MONTH", "DAY", "TIME")
)

all <- left_join(
    all,
    kp_ap_data,
    by = c("YEAR", "MONTH", "DAY", "TIME")
)

# KP, AP 결측치 down 보간
temp <- as.double(all$KP)
temp <- na.locf(temp)
all$KP <- temp

# 필요한 데이터만 추출
temp_all <- all %>% select(c(
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
))

all <- temp_all

write.csv(all, file = "aurora_complete.csv", row.names = FALSE)
