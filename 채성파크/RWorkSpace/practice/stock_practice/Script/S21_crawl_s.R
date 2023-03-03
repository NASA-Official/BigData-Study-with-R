library(XML)
theurl<-"http://finance.daum.net/item/quote_yyyymmdd_sub.daum?page=2&code=005930&modify=1"
theurl<-"http://finance.daum.net/item/quote_yyyymmdd_sub.daum?page=1&code=041140&modify=1"
sise_html<-readHTMLTable(theurl)

sise_table<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),]
names(sise_table)<-c("date","Open","High","Low","Close","p_diff","r_diff","Volume")
sise_table$date<-paste("20",sise_table$date,sep="")
sise_table[,2]<-as.numeric(gsub(",","",as.character(sise_table[,2])))
sise_table[,3]<-as.numeric(gsub(",","",as.character(sise_table[,3])))
sise_table[,4]<-as.numeric(gsub(",","",as.character(sise_table[,4])))
sise_table[,5]<-as.numeric(gsub(",","",as.character(sise_table[,5])))
sise_table[,6]<-gsub("▼","-",as.character(sise_table[,6]))
sise_table[,6]<-gsub("↓","-",sise_table[,6])
sise_table[,6]<-gsub("▲","",sise_table[,6])
sise_table[,6]<-as.numeric(gsub(",","",sise_table[,6]))
sise_table[,7]<-as.numeric(gsub("%","",as.character(sise_table[,7])))
sise_table[,8]<-as.numeric(gsub(",","",as.character(sise_table[,8])))
