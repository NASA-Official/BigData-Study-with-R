library(dplyr)

# aurora_regulated를 시간별로 sort
# all <- read.csv(
#     # file = "~/Desktop/AuroraData/aurora_complete.csv",
#     # file = "C:/Users/SONG/Documents/R_Study/aurora_regulated.csv",
#     # file = "C:/Users/multicampus/Documents/R_Study/aurora_regulated.csv",
#     fileEncoding = "UTF-8",
#     na.strings = c("", " ", "NA", NA)
# )
all <- readRDS(
    file = "allData.rds"
)

str(all)

# YEAR, MONTH, DAY, TIME을 POSIX형으로
all_orderby_year <- all %>% arrange(YEAR, MONTH)

head(all_orderby_year, 26)

temp <- all_orderby_year
temp$dateTime <- paste(temp$YEAR,
    paste(ifelse(
        as.integer(temp$MONTH) < 10,
        paste("0", as.integer(temp$MONTH), sep = ""),
        temp$MONTH
    )),
    paste(ifelse(
        as.integer(temp$DAY) < 10,
        paste("0", as.integer(temp$DAY), sep = ""),
        temp$DAY
    )),
    sep = "-"
)

head(temp$dateTime, 26)
head(temp$TIME, 26)
head(as.integer(temp$TIME), 26)

temp$mTIME <- paste(ifelse(
    as.numeric(temp$TIME) - 1 < 10,
    paste("0", (as.numeric(temp$TIME) - 1), sep = ""),
    (as.numeric(temp$TIME) - 1)
), "00", sep = ":")


head(temp$mTIME, 26)

temp$REAL_TIME <- paste(temp$dateTime, temp$mTIME, sep = " ")
temp$DATE <- strptime(temp$REAL_TIME, "%Y-%m-%d %H:%M", tz = "UTC")

head(temp$DATE, 26)

all_orderby_year$DATE <- temp$DATE

all_orderby_year <- all_orderby_year %>% relocate(c("DATE"))

all_orderby_year <- all_orderby_year[, -which(names(all_orderby_year) %in% c(
    "YEAR", "MONTH", "DAY", "TIME"
))]

str(all_orderby_year)

saveRDS(
    all_orderby_year,
    file = "all_orderby_time.rds"
)
