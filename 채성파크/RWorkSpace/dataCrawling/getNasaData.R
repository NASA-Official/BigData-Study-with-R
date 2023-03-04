library(lubridate)
library(dplyr)

startDate <- as.Date("2001-08-01")
tempDate <- startDate
endDate <- Sys.Date()

#시작날 데이터 가져오기
baseURL <- "https://sohoftp.nascom.nasa.gov/sdb/goes/ace/monthly/"
targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_epam_1h.txt", sep="")

ace_epam_fd <- read.table(file=targetURL, header= FALSE, skip=17, sep="")
tempDate <- tempDate + months(1)

head(ace_epam_fd)

while(TRUE) {
  if (format(tempDate, "%Y%m") == format(endDate, "%Y%m")) break
  #Sys.sleep(runif(1, min=0.6, max=1.5))
  targetURL <- paste(baseURL,format(tempDate, "%Y%m"),"_ace_epam_1h.txt", sep="")
  #2011년 11월
  read.table(file=targetURL, header= FALSE, skip=17, nrows=744, sep="") %>% rbind(ace_epam_fd) -> ace_epam_fd
  print(paste("Success", tempDate))
  tempDate <- tempDate + months(1)
}

#summary(ace_epam_fd)
#str(ace_epam_fd)

colnames(ace_epam_fd) <- c("YEAR", "MONTH", "DAY", "TIME", "MODIFIED_JULIAN_DAY", "SECONDs_OF_THE_DAY", "ELECTRON_DATA_STATE", "E_38_53", "E_175_315", "PROTONS_DATA_STATE", "P_47_65", "P_112_187", "P_310_580", "P_761_1220", "P_060_1910", "RATIO")
write.csv(ace_epam_fd, file="ace_epam_1h.csv", row.names = FALSE)
