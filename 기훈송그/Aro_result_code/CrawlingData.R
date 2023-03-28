crawlingData <- function(input_date) {
    # ex) input_date = "2023-02-01"
    # 결과값은 정규화 되어 바로 랜덤포레스트에 넣을 수 있음
    # Installing Packages that are not already available in the system
    list.of.packages <- c("lubridate", "dplyr", "zoo", "caret")
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
    startDate <- as.Date(input_date)
    tempDate <- startDate
    endDate <- Sys.Date()
    # ace_1h 데이터 가져오기
    baseURL <- "https://sohoftp.nascom.nasa.gov/sdb/goes/ace/monthly/"
    targetURL_1 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_epam_1h.txt", sep = "")
    targetURL_2 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_mag_1h.txt", sep = "")
    targetURL_3 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_sis_1h.txt", sep = "")
    targetURL_4 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_swepam_1h.txt", sep = "")
    ## read.table
    ace_epam_df <- read.table(file = targetURL_1, header = FALSE, skip = 16, sep = "")
    ace_mag_df <- read.table(file = targetURL_2, header = FALSE, skip = 16, sep = "")
    ace_sis_df <- read.table(file = targetURL_3, header = FALSE, skip = 16, sep = "")
    ace_swepam_df <- read.table(file = targetURL_4, header = FALSE, skip = 17, sep = "")
    tempDate <- tempDate + months(1)
    # get next month
    while (TRUE) {
        ## url
        targetURL_1 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_epam_1h.txt", sep = "")
        targetURL_2 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_mag_1h.txt", sep = "")
        targetURL_3 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_sis_1h.txt", sep = "")
        targetURL_4 <- paste(baseURL, format(tempDate, "%Y%m"), "_ace_swepam_1h.txt", sep = "")
        # read table
        temp_epam <- read.table(file = targetURL_1, header = FALSE, skip = 16, sep = "")
        ace_epam_df <- rbind(ace_epam_df, temp_epam)
        temp_mag <- read.table(file = targetURL_2, header = FALSE, skip = 16, sep = "")
        ace_mag_df <- rbind(ace_mag_df, temp_mag)
        temp_sis <- read.table(file = targetURL_3, header = FALSE, skip = 16, sep = "")
        ace_sis_df <- rbind(ace_sis_df, temp_sis)
        temp_swepam <- read.table(file = targetURL_4, header = FALSE, skip = 17, sep = "")
        ace_swepam_df <- rbind(ace_swepam_df, temp_swepam)
        if (format(tempDate, "%Y%m") == format(endDate, "%Y%m")) break
    }
    # modify colnames
    colnames(ace_epam_df) <- c(
        "YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDS_OF_THE_DAY",
        "ELECTRON_DATA_STATE", "E_38_53", "E_175_315",
        "PROTONS_DATA_STATE", "P_47_65", "P_112_187", "P_310_580", "P_761_1220", "P_1060_1910", "RATIO"
    )
    colnames(ace_mag_df) <- c(
        "YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDS_OF_THE_DAY",
        "GSM_DATA_STATE", "GSM_BX", "GSM_BY", "GSM_BZ", "GSM_BT", "GSM_LAT", "GSM_LONG"
    )
    colnames(ace_sis_df) <- c(
        "YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDS_OF_THE_DAY",
        "FLUX_PROTON_STATE1", "tenMeV", "FLUX_PROTON_STATE2", "thirtyMeV"
    )
    colnames(ace_swepam_df) <- c(
        "YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDS_OF_THE_DAY",
        "SW_STATE", "SW_PD", "SW_BS", "SW_IT"
    )
    # Data Preprocessing
    ace_epam_df[ace_epam_df == -1.00e+05] <- NA
    ace_epam_df <-
        ace_epam_df %>% mutate_all(., list(~ na.approx(., method = "linear", rule = 2)))
    ace_mag_df[ace_mag_df == -999.9] <- NA
    ace_mag_df <-
        ace_mag_df %>% mutate_all(., list(~ na.approx(., method = "linear", rule = 2)))
    ace_sis_df[ace_sis_df == -1.00e+05] <- NA
    ace_sis_df <-
        ace_sis_df %>% mutate_all(., list(~ na.approx(., method = "linear", rule = 2)))
    ace_swepam_df$SW_PD <- replace(
        ace_swepam_df$SW_PD,
        ace_swepam_df$SW_PD == -9999.9,
        NA
    )
    ace_swepam_df$SW_BS <- replace(
        ace_swepam_df$SW_BS,
        ace_swepam_df$SW_BS == -9999.9,
        NA
    )
    ace_swepam_df$SW_IT <- replace(
        ace_swepam_df$SW_IT,
        ace_swepam_df$SW_IT == -1.00e+05,
        NA
    )
    ## linear interpolation
    ace_swepam_df <-
        ace_swepam_df %>% mutate_all(., list(~ na.approx(., method = "linear", rule = 2)))
    # Join All
    all <- left_join(
        ace_epam_df,
        ace_mag_df,
        by = c("YEAR", "MONTH", "DAY", "TIME")
    )
    all <- left_join(
        all,
        ace_sis_df,
        by = c("YEAR", "MONTH", "DAY", "TIME")
    )
    all <- left_join(
        all,
        ace_swepam_df,
        by = c("YEAR", "MONTH", "DAY", "TIME")
    )
    all <- all %>% select(
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
    )
    # factor형으로 변환
    all$ELECTRON_DATA_STATE <- as.factor(all$ELECTRON_DATA_STATE)
    all$PROTONS_DATA_STATE <- as.factor(all$PROTONS_DATA_STATE)
    all$FLUX_PROTON_STATE1 <- as.factor(all$FLUX_PROTON_STATE1)
    all$FLUX_PROTON_STATE2 <- as.factor(all$FLUX_PROTON_STATE2)
    all$SW_STATE <- as.factor(all$SW_STATE)
    all$YEAR <- as.factor(all$YEAR)
    all$MONTH <- as.factor(all$MONTH)
    all$DAY <- as.factor(all$DAY)
    all$TIME <- as.factor(all$TIME)
    # 정규화에서 제외할 데이터
    all_without <- all %>% select(
        -GSM_BZ, -GSM_LAT
    )
    # 정규화
    scale_model <- preProcess(all_without, method = "range")
    regulate <- predict(scale_model, all_without)
    # regulate$KP <- all$KP
    regulate$GSM_BZ <- all$GSM_BZ
    regulate$GSM_LAT <- all$GSM_LAT
    # factor level setting
    year_factor <- factor(c(2001:2023))
    year_chr <- as.character(year_factor)
    regulate$YEAR <- factor(regulate$YEAR, levels = c(year_chr))
    month_factor <- factor(c(1:12))
    month_chr <- as.character(month_factor)
    regulate$MONTH <- factor(regulate$MONTH, levels = c(month_chr))
    day_factor <- factor(c(1:31))
    day_chr <- as.character(day_factor)
    regulate$DAY <- factor(regulate$DAY, levels = c(day_chr))
    time_factor <- as.factor(seq(0, 2300, 100))
    time_chr <- as.character(time_factor)
    regulate$TIME <- factor(regulate$TIME, levels = c(time_chr))
    eds_factor <- factor(c("-1", "0", "9"))
    eds_chr <- as.character(eds_factor)
    regulate$ELECTRON_DATA_STATE <- factor(regulate$ELECTRON_DATA_STATE, levels = c(eds_chr))
    regulate$PROTONS_DATA_STATE <- factor(regulate$PROTONS_DATA_STATE, levels = c(eds_chr))
    fps_factor <- factor(c("0", "9"))
    fps_chr <- as.character(fps_factor)
    regulate$FLUX_PROTON_STATE1 <- factor(regulate$FLUX_PROTON_STATE1, levels = c(fps_chr))
    regulate$FLUX_PROTON_STATE2 <- factor(regulate$FLUX_PROTON_STATE2, levels = c(fps_chr))
    sw_factor <- factor(c("0", "1", "3", "4", "6", "7", "9"))
    sw_chr <- as.character(sw_factor)
    regulate$SW_STATE <- factor(regulate$SW_STATE, levels = c(sw_chr))
    return(regulate)
}
