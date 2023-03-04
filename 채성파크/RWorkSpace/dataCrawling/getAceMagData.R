startDate <- as.Date("2001-08-01")
tempDate <- startDate
endDate <- Sys.Date()

#시작날 데이터 가져오기
baseURL <- "https://sohoftp.nascom.nasa.gov/sdb/goes/ace/monthly/"
targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_mag_1h.txt", sep="")

ace_mag_fd <- read.table(file=targetURL, header= FALSE, skip=17, sep="")
tempDate <- tempDate + months(1)

head(ace_mag_fd)

# install.packages("lubridate")
library(lubridate)
library(dplyr)

while(TRUE) {
  if (format(tempDate, "%Y%m") == format(endDate, "%Y%m")) break
  # Sys.sleep(runif(1, min=0.1, max=1))
  targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_mag_1h.txt", sep="")
  #2011년 11월
  read.table(file=targetURL, header= FALSE, skip=17, sep="") %>% rbind(ace_mag_fd) -> ace_mag_fd
  print(paste("Success", tempDate))
  tempDate <- tempDate + months(1)
}

summary(ace_mag_fd)
str(ace_mag_fd)

colnames(ace_mag_fd) <- c("YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDs_OF_THE_DAY", "GSM_DATA_STATE", "GSM_BX", "GSM_BY", "GSM_BZ", "GSM_BT", "GSM_LAT", "GSM_LONG")
write.csv(ace_mag_fd, file="ace_mag_1h.csv", row.names = FALSE)
