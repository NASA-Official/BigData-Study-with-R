library(sqldf)
library(quantmod)
library(caret)

load("sise.rdata")
load("ft.rdata")
head(sise);tail(sise)
mycode<-sqldf("select distinct(code) code, name from sise order by code")

tr_end.dt<-"2012-01-01/2014-08-31"
pred_st.dt<-"2014-04-01/"
sim.dt<-"2014-04-01/2014-10-31"
i<-9
for (i in 1:nrow(mycode)){
  stock.data<-sise[sise$code==mycode[i,1],-c(6,7,9,10)]
  # head(stock.data)
  stock.data$Adjusted <- stock.data$Close
  stock.data$date <- gsub("\\.","-",stock.data$date)
  # head(stock.data)
  row.names(stock.data) <- stock.data[,"date"]
  stock.data <- as.xts(stock.data[,-1],dateFormat="POSIXct")
  stock.id<-mycode[i,]
  # str(stock.data)
  feature.data <- addFewerFeatures(stock.data)
  names(feature.data)
  colnames(feature.data)<-c("ROC1","ROC2","ROC3","ROC5","VROC","BBands_pctB","SMI","SMI_sig","SMI3MA","SMI3MA_sig","MACD","MACD_sig","Stoch_fastK","Stoch_1fastD","Stoch_2fastD","Stoch_3fastD","Stoch_4fastD","Stoch_slowD","Stoch2MA.fastK","Stoch2MA.fastD","Stoch2MA.slowD","StochRSI_fastK","StochRSI_fastD","StochRSI_slowD","RSI","ATR.tr","ATR.atr","ATR.trueHigh","ATR.trueLow","Aroon.up","Aroon.down","Aroon.osci","Volatility","EMA7","EMA50","EMA120","EMA7to50","EMA7to120","EMA50to120","SMA7","SMA50","SMA120","SMA7to50","SMA7to120","SMA50to120","CMO15","DEMA20")
  feature.data <- merge(feature.data, T.sig(T.ind(stock.data)))
  feature.data <- na.omit(feature.data)
  colnames(feature.data)[ncol(feature.data)] <- "NEXTDAY"
  train.data = na.omit(as.data.frame(feature.data[tr_end.dt]))
  rownames(train.data) = NULL
  train.data$NEXTDAY = as.factor(train.data$NEXTDAY)
  levels(train.data$NEXTDAY) <- list(s="-1", b="1")
  test.data = na.omit(as.data.frame(feature.data[pred_st.dt]))
  rownames(test.data) = NULL
  test.data$NEXTDAY = as.factor(test.data$NEXTDAY)
  levels(test.data$NEXTDAY) <- list(s="-1", b="1")
  formula <- as.formula('NEXTDAY ~ .')
  # names(train.data)
  
  # Model: randomForest 254 secs
  fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
  rfGrid <- expand.grid(mtry=(1:8)*2)
  
  rf.model <- train(formula,data=train.data,method="rf",trControl=fitControl,tuneGrid=rfGrid) # 184 sec
  rfImp <- varImp(rf.model)
  confusionMatrix(rf.model)
  plot(rf.model,main="plot(rf.model)") # result plot
  plot(rfImp,main="plot(rfImp)") # result plot
  
  simul.data <- stock.data[sim.dt]
  attr(simul.data,"dimnames")
  names(train.data)
  names(test.data)
  sim_data<-addFewerFeatures(simul.data)
  names(sim_data)<-c("ROC1","ROC2","ROC3","ROC5","VROC","BBands_pctB","SMI","SMI_sig","SMI3MA","SMI3MA_sig","MACD","MACD_sig","Stoch_fastK","Stoch_1fastD","Stoch_2fastD","Stoch_3fastD","Stoch_4fastD","Stoch_slowD","Stoch2MA.fastK","Stoch2MA.fastD","Stoch2MA.slowD","StochRSI_fastK","StochRSI_fastD","StochRSI_slowD","RSI","ATR.tr","ATR.atr","ATR.trueHigh","ATR.trueLow","Aroon.up","Aroon.down","Aroon.osci","Volatility","EMA7","EMA50","EMA120","EMA7to50","EMA7to120","EMA50to120","SMA7","SMA50","SMA120","SMA7to50","SMA7to120","SMA50to120","CMO15","DEMA20")
  simul.feature.data <- na.omit(sim_data)
  market <- (simul.data[paste(sep="", as.character(start(simul.feature.data)), "/")])
  rownames(simul.feature.data) = NULL
  
  simul.rf.pred <- predict(rf.model, simul.feature.data)
  sig <- simul.rf.pred
  
  simul.results <- T.simulator(market, simul.rf.pred)
  if (sig[length(sig)]=="b") Res<-"BUY " else Res<-"SELL"
  print(paste(i,Res,mycode[i,1],mycode[i,2], "Profit = ", round(last(simul.results$Profit),2),"acc=",round(max(rf.model$results$Accuracy),2)))
  
  # Results Visualization
  candleChart(simul.results, TA=NULL, theme='white',name=paste(stock.id$code,stock.id$name))
  addClose <- newTA(FUN=Cl,col=1,legend='Close')
  addClose(on=1)
  addVo()
  addBBands(n=10)
  add.Predicted <- newTA(FUN=Predicted, col=6, legend='Predicted', type='b')
  add.Predicted()
  add.Profit <- newTA(FUN=Profit, col=6, legend='Profit', type='b')
  add.Profit()
  add.Tind <- newTA(FUN=Tind, col='red', legend='Tind', type='b')
  add.Tind()
  add.Tsig <- newTA(FUN=Tsig, col='red', legend='Tsig', type='b')
  add.Tsig()
  
  n_png<-paste("png_",mycode[i,],".png",sep="")
  dev.copy(png,n_png)
  dev.off()
  
  # Data & Model Backup
  date_tmp<-rownames(as.data.frame(feature.data[pred_st.dt]))
  n1<-length(simul.rf.pred);n2<-length(date_tmp);n3<-nrow(test.data);
  date<-date_tmp[(n2-n1+1):n2]
  tt_d<-test.data[(n2-n1+1):n2,c(14,1:13)]
  rf_pred<-simul.rf.pred
  
  if(i==1) mdl_rf<-list(mycode[i,],rf.model)
  if(i>1) mdl_rf<-c(mdl_rf,list(mycode[i,],rf.model))
  if(i%%20==0) save(mdl_rf,file=paste("data/model_20141001_",i,".rdata",sep=""))
}

length(mdl_rf)
save(mdl_rf,file='model.rdata')
