library(lubridate)
library(dplyr)

startDate <- as.Date("2001-08-01")
tempDate <- startDate
endDate <- Sys.Date()

#시작날 데이터 가져오기
baseURL <- "https://sohoftp.nascom.nasa.gov/sdb/goes/ace/monthly/"
targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_sis_1h.txt", sep="")

ace_sis_fd <- read.table(file=targetURL, header= FALSE, skip=16, sep="")
tempDate <- tempDate + months(1)

#head(ace_sis_fd)

while(TRUE) {
  if (format(tempDate, "%Y%m") == format(endDate, "%Y%m")) break
  Sys.sleep(runif(1, min=0.1, max=1))
  targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_sis_1h.txt", sep="")
  #2011년 11월
  read.table(file=targetURL, header= FALSE, skip=16, sep="", nrows = 744) %>% rbind(ace_sis_fd) -> ace_sis_fd
  print(paste("Success", tempDate))
  tempDate <- tempDate + months(1)
}

#summary(ace_sis_fd)
#str(ace_sis_fd)

colnames(ace_sis_fd) <- c("YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDs_OF_THE_DAY", "FLUX_PROTON_STATE1", "_10MeV", "FLUX_PROTON_STATE2", "_30MeV")
write.csv(ace_sis_fd, file="ace_sis_1h.csv", row.names = FALSE)
