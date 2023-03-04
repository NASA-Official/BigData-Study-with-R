library(lubridate)
library(dplyr)

startDate <- as.Date("2001-08-01")
tempDate <- startDate
endDate <- Sys.Date()

#시작날 데이터 가져오기
baseURL <- "https://sohoftp.nascom.nasa.gov/sdb/goes/ace/monthly/"
targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_loc_1h.txt", sep="")

ace_loc_fd <- read.table(file=targetURL, header= FALSE, skip=17, sep="")
tempDate <- tempDate + months(1)

#head(ace_loc_fd)

while(TRUE) {
  if (format(tempDate, "%Y%m") == format(endDate, "%Y%m")) break
  #Sys.sleep(runif(1, min=0.1, max=1))
  targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_loc_1h.txt", sep="")
  #2011년 11월
  read.table(file=targetURL, header= FALSE, skip=17, sep="", nrows = 744) %>% rbind(ace_loc_fd) -> ace_loc_fd
  print(paste("Success", tempDate))
  tempDate <- tempDate + months(1)
}

#summary(ace_loc_fd)
#str(ace_loc_fd)

colnames(ace_loc_fd) <- c("YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDs_OF_THE_DAY", "GES_X", "GES_Y", "GES_Z")
write.csv(ace_loc_fd, file="ace_loc_1h.csv", row.names = FALSE)
