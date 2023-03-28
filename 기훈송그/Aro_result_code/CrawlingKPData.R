crawlingKPData <- function(input_date) {
    # ex) input_date = "2023-02-01"
    # 관측된 KP 데이터를 input_date부터 지금 순간까지 가져온다
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
    endDate <- Sys.Date()
    # Get KP data
    kp_url <- paste("https://kp.gfz-potsdam.de/kpdata?startdate=", startDate,
        "&enddate=", endDate, "&format=kp2#kpdatadownload-143",
        sep = ""
    )
    kp_ap_df <- read.table(
        file = kp_url,
        header = FALSE, sep = "",
        col.names = c(
            "YEAR", "MONTH", "DAY", "TIME",
            "MID_TIME", "DAYS", "MID_OF_DAYS", "KP", "AP", "VERACITY"
        ),
    )
    kp_ap_df$TIME <- kp_ap_df$TIME * 100
    kp_ap_df <- kp_ap_df %>% select("YEAR", "MONTH", "DAY", "TIME", "KP")
    return(kp_ap_df)
}
