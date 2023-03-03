library(doMC)
registerDoMC(6)
library(foreach)
library(XML)
load("selected.rdata")
sise_t<-foreach(i=1:nrow(selected), .combine='rbind') %dopar% {
  for (k in 1:30){
    theurl<-paste("http://stock.daum.net/item/quote_yyyymmdd_sub.daum?page=",k,"&code=",selected$code[i],"&modify=0",sep="")
    sise_html<-readHTMLTable(theurl)
    sise_table_tmp<-sise_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),]
    if (k==1) {
      sise_table<-sise_table_tmp
    } else {
      sise_table<-rbind(sise_table,sise_table_tmp)
    }
  }
  names(sise_table)<-c("date","Open","High","Low","Close","p_diff","r_diff","Volume")
  sise_table$code<-selected$code[i]
  sise_table$name<-selected$name[i]
  return(sise_table)
}

sise_t$date<-as.character(sise_t$date)
sise_t$date<-paste("20",sise_t$date,sep="")
sise_t[,2]<-as.numeric(gsub(",","",as.character(sise_t[,2])))
sise_t[,3]<-as.numeric(gsub(",","",as.character(sise_t[,3])))
sise_t[,4]<-as.numeric(gsub(",","",as.character(sise_t[,4])))
sise_t[,5]<-as.numeric(gsub(",","",as.character(sise_t[,5])))
sise_t[,6]<-gsub("▼","-",as.character(sise_t[,6]))
sise_t[,6]<-gsub("↓","-",sise_t[,6])
sise_t[,6]<-gsub("▲","",sise_t[,6])
sise_t[,6]<-as.numeric(gsub(",","",sise_t[,6]))
sise_t[,7]<-as.numeric(gsub("%","",as.character(sise_t[,7])))
sise_t[,8]<-as.numeric(gsub(",","",as.character(sise_t[,8])))

str(sise_t)

# Dup Check
table(duplicated(sise_t))
ind<-which(duplicated(sise_t)==T)
if (length(ind)>0) sise<-sise_t[-ind,] else sise<-sise_t

# Eye Check
head(selected,1);tail(selected,1)
head(sise);tail(sise)

# Save the result
save(sise,sise_t,file="sise.rdata")
