library(doMC)
registerDoMC(6)

library(foreach)

library(XML)
load("selected.rdata")

volume_t<-foreach(i=1:nrow(selected), .combine='rbind') %dopar% {
  for (k in 1:10){
    theurl<-paste("http://stock.daum.net/item/foreign_yyyymmdd.daum?page=",k,"&code=",selected$code[i],sep="")
    volume_html<-readHTMLTable(theurl)
    if (length(volume_html[[1]])>0) {
      volume_table_tmp<-volume_html[[1]][c(2:6,9:13,16:20,23:27,30:34,37:41),c(1:5)]
      if (k==1) {
        volume_table<-volume_table_tmp
      } else {
        volume_table<-rbind(volume_table,volume_table_tmp)
      }
    }
  }
  volume_table$code<-selected$code[i]
  volume_table$name<-selected$name[i]
  return(volume_table)
} # 1 min on macbook pro 15 inch

names(volume_t)<-c("date","f_tot","f_rate","f_net","o_net","code","name")
volume_t$date<-paste("20",volume_t$date,sep="")
volume_t[,2]<-as.numeric(gsub(",","",as.character(volume_t[,2])))
volume_t[,3]<-as.numeric(gsub("%","",as.character(volume_t[,3])))
volume_t[,4]<-as.numeric(gsub(",","",as.character(volume_t[,4])))
volume_t[,5]<-as.numeric(gsub(",","",as.character(volume_t[,5])))

# Dup Check
table(duplicated(volume_t))
ind<-which(duplicated(volume_t)==T)
volume<-volume_t[-ind,]

# Eye Check
head(selected,1);tail(selected,1)
head(volume);tail(volume)

# Save the result
save(volume,volume_t,file="volume.rdata")
