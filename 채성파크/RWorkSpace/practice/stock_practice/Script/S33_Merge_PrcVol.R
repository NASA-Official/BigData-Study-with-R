load("sise.rdata")
load("volume.rdata")
head(sise);head(volume)

sise$date <- gsub("\\.","-",sise$date)
volume$date <- gsub("\\.","-",volume$date)
table(duplicated(sise))
table(duplicated(volume))

pv<-merge(sise[,c(1,5:10)],volume, by=c("date","code","name"))
pv$date<-as.Date(pv$date)
pv<-pv[order(pv$code,rev(pv$date)),]
pv<-pv[order(pv$date,decreasing=T),]
pv<-pv[order(pv$code),]

pv$org_tot<-0
pv$prv_tot<-0
pv$total<-0
pv$for_r<-0
pv$org_r<-0
pv$prv_r<-0
pv$close_r<-0
pv$vol_r<-0
pv$vol_var<-0
pv$o_net_cont<-0 #

mycode<-unique(pv$code)
table(pv$date)

for (i in 1:length(mycode)){
  pv_tmp<-pv[pv$code==mycode[i],]
  # head(pv_tmp,2);tail(pv_tmp,2)
  for (k in nrow(pv_tmp):1) {
    if(k==nrow(pv_tmp)) {
      if(pv_tmp$o_net[k]>0) pv_tmp$o_net_cont[k]<-1
      pv_tmp$org_tot[k]<-pv_tmp$o_net[k]
      pv_tmp$prv_tot[k]<--(pv_tmp$f_net[k]+pv_tmp$o_net[k])
      } else {
        if(pv_tmp$o_net[k]>0) { #
          pv_tmp$o_net_cont[k]<-pv_tmp$o_net_cont[k+1]+1 #
        } else pv_tmp$o_net_cont[k]<-0 #
        pv_tmp$org_tot[k]<-pv_tmp$org_tot[k+1]+pv_tmp$o_net[k]
        pv_tmp$prv_tot[k]<-pv_tmp$prv_tot[k+1]-(pv_tmp$f_net[k]+pv_tmp$o_net[k])
      }
  }
  pv_tmp$org_tot<-pv_tmp$org_tot-min(pv_tmp$org_tot)
  pv_tmp$prv_tot<-pv_tmp$prv_tot-min(pv_tmp$prv_tot)
  pv_tmp$total<-pv_tmp$f_tot+pv_tmp$org_tot+pv_tmp$prv_tot
  pv_tmp$for_r<-pv_tmp$f_tot/pv_tmp$total*100
  pv_tmp$org_r<-pv_tmp$org_tot/pv_tmp$total*100
  pv_tmp$prv_r<-pv_tmp$prv_tot/pv_tmp$total*100
  pv_tmp$close_r<-pv_tmp$Close/max(pv_tmp$Close)*100
  pv_tmp$vol_r<-pv_tmp$Volume/max(pv_tmp$Volume)*100
  pv_tmp$vol_var<-100-min(pv_tmp$Volume)/max(pv_tmp$Volume)
  if (i==1) stock_pv<-pv_tmp else stock_pv<-rbind(stock_pv,pv_tmp)
  print(paste(i,"/",length(mycode)))
}

head(stock_pv)
save(stock_pv,mycode,file="stock_pv.rdata")
