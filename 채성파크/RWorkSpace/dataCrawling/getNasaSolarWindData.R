library(lubridate)
library(dplyr)

startDate <- as.Date("2001-08-01")
tempDate <- startDate
endDate <- Sys.Date()

#시작날 데이터 가져오기
baseURL <- "https://sohoftp.nascom.nasa.gov/sdb/goes/ace/monthly/"
targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_swepam_1h.txt", sep="")

ace_swepam_fd <- read.table(file=targetURL, header= FALSE, skip=18, sep="")
tempDate <- tempDate + months(1)

#head(ace_swepam_fd)

while(TRUE) {
  if (format(tempDate, "%Y%m") == format(endDate, "%Y%m")) break
  #Sys.sleep(runif(1, min=0.1, max=1))
  targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_swepam_1h.txt", sep="")
  #2011년 11월
  read.table(file=targetURL, header= FALSE, skip=18, sep="", nrows = 744) %>% rbind(ace_swepam_fd) -> ace_swepam_fd
  print(paste("Success", tempDate))
  tempDate <- tempDate + months(1)
}

#summary(ace_swepam_fd)
#str(ace_swepam_fd)

colnames(ace_swepam_fd) <- c("YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDs_OF_THE_DAY", "SOLAR_WIND_DATA_STATE", "SOLAR_WIND_PROTON_DENSITY", "SOLAR_WIND_BULK_SPEED", "SOLAR_WIND_ION_TEMPERATURE")
write.csv(ace_swepam_fd, file="ace_swepam_1h.csv", row.names = FALSE)
