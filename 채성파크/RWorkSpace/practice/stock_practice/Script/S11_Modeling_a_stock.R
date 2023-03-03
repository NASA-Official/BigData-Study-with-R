########################
# (1) Data Acquisition #
########################

install.packages("Quandl") # in case Quandl has not been installed 
library(Quandl)
stck<-Quandl("GOOG/KRX_090430", trim_start="2011-01-02", trim_end="2014-10-31")
head(stck);tail(stck)
stck$Adjusted <- stck$Close
row.names(stck) <- stck[,"Date"] # preparation of transforming time series

#################
# (2) Variables #
#################

install.packages("quantmod") # in case quantmod has not been installed
library(quantmod)
stock.data <- as.xts(stck[,-1],dateFormat="POSIXct")
head(stock.data)

x<-stock.data

?ROC
res <- ROC(x[, "Close"], type="discrete", n=1) # ROC1, apply ROC {TTR} (Rate of Change)
res <- merge(res, ROC(x[, "Close"], type="discrete", n=2), all=FALSE) # ROC2
res <- merge(res, ROC(x[, "Close"], type="discrete", n=3), all=FALSE) # ROC3
res <- merge(res, ROC(x[, "Close"], type="discrete", n=5), all=FALSE) # ROC5
res <- merge(res, ROC(x[, "Close"], type="discrete", n=10), all=FALSE) # ROC10
res <- merge(res, ROC(x[, "Volume"], type="discrete", n=3), all=FALSE) # ROC_VOL
names(res)<-c("ROC1","ROC2","ROC3","ROC5","ROC10","ROC_VOL")

?BBands
res <- merge(res, BBands(HLC(x), n=10)[,4], all=FALSE) # pctB
n<-ncol(res);names(res)[n]<-"pctB"

?SMI
res <- merge(res, SMI(x[, c("High","Low","Close")]), all=FALSE) # SMI, SMI_sig
res <- merge(res, SMI(x[, c("High","Low","Close")], maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA))), all=FALSE) # SMI3MA, SMI3MA_sig
n<-ncol(res);names(res)[c(n-3,n)]<-c("SMI","SMI_sig","SMI3MA","SMI3MA_sig")

?MACD
res <- merge(res, MACD(Cl(x), nFast=6, nSlow=13, nSig=5), all=FALSE) # MACD, MACD_sig
n<-ncol(res);names(res)[c(n-1,n)]<-c("MACD","MACD_sig")

?stoch
res <- merge(res, stoch(x[, c("High","Low","Close")], nFastK=7), all=FALSE) # Stoch_fastK7, Stoch_fastD7, Stoch_slowD7
res <- merge(res, stoch(HLC(x), maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)), nFastK=7)[,1], all=FALSE) # Stoch2MA_fastK, Stoch2MA_fastD, Stoch2MA_slowD
res <- merge(res, stoch(RSI(Cl(x))), all=FALSE) # StochRSI.fastK, StochRSI.fastD, StochRSI.slowD
n<-ncol(res);names(res)[c((n-8):n)]<-c("Stoch_fastK7","Stoch_fastD7","Stoch_slowD7","Stoch2MA_fastK","Stoch2MA_fastD","Stoch2MA_slowD","StochRSI.fastK","StochRSI.fastD","StochRSI.slowD")

?ATR
res <- merge(res, ATR(HLC(x))[,c(1,2)], all=FALSE) # ATR_tr, ATR_atr, ATR_trueHigh, ATR_trueLow
n<-ncol(res);names(res)[c((n-3):n)]<-c("ATR_tr","ATR_atr","ATR_trueHigh","ATR_trueLow")

?aroon
res <- merge(res, aroon(x[, c("High", "Low")]), all=FALSE) # Arron_Up, Arron_Dn, Arron_osc
n<-ncol(res);names(res)[c((n-2):n)]<-c("Arron_Up","Arron_Dn","Arron_osc")

?volatility
res <- merge(res, volatility(OHLC(x), n=5, calc="close"), all=FALSE) # Volatile
n<-ncol(res);names(res)[n]<-"Volatile"

?EMA
res <- merge(res, EMA(Cl(x),7), all=FALSE) # EMA7
res <- merge(res, EMA(Cl(x),50), all=FALSE) # EMA50
res <- merge(res, EMA(Cl(x),120), all=FALSE) # EMA120
res <- merge(res, (EMA(Cl(x),7) - EMA(Cl(x),50)) / EMA(Cl(x),50), all=FALSE) # EMA7to50
res <- merge(res, (EMA(Cl(x),7) - EMA(Cl(x),120)) / EMA(Cl(x),120), all=FALSE) # EMA7to120
res <- merge(res, (EMA(Cl(x),50) - EMA(Cl(x),120)) / EMA(Cl(x),120), all=FALSE) # EMA50to120
n<-ncol(res);names(res)[c((n-5):n)]<-c("EMA7","EMA50","EMA120","EMA7to50","EMA7to120","EMA50to120")

?SMA
res <- merge(res, SMA(Cl(x),7), all=FALSE) # SMA7
res <- merge(res, SMA(Cl(x),50), all=FALSE) # SMA50
res <- merge(res, SMA(Cl(x),120), all=FALSE) # SMA120
res <- merge(res, (SMA(Cl(x),7) - SMA(Cl(x),50)) / SMA(Cl(x),50), all=FALSE) # SMA7to50
res <- merge(res, (SMA(Cl(x),7) - SMA(Cl(x),120)) / SMA(Cl(x),120), all=FALSE) # SMA7to120
res <- merge(res, (SMA(Cl(x),50) - SMA(Cl(x),120)) / SMA(Cl(x),120), all=FALSE) # SMA50to120
n<-ncol(res);names(res)[c((n-5):n)]<-c("SMA7","SMA50","SMA120","SMA7to50","SMA7to120","SMA50to120")

?CMO
res <- merge(res, CMO(Cl(x),n=15), all=FALSE) # CMO15
n<-ncol(res);names(res)[n]<-"CMO15"

?DEMA
res <- merge(res, DEMA(Cl(x),20), all=FALSE) # DEMA20
n<-ncol(res);names(res)[n]<-"DEMA20"

names(res)
res[119:120,]

# sum of next 5-day margins that is more than 2% or less than -2%
n.days<-5;tgt.margin<-0.02

r <- matrix(NA, ncol=n.days, nrow=nrow(x))
head(r)
for(i in 1:n.days) r[,i] <- Next(Delt(Cl(x), k=i), i)
z <- apply(r,1,function(x) 100 * sum(x[x > tgt.margin | x < -tgt.margin]))
if (is.xts(x)) z<-xts(z, time(x)) else z
head(z)

# increasing or decreasing of z with respect to the previous day's sum
sig <- z
df.sig <- coredata(sig)
head(sig)
n <- nrow(sig)
for (i in 1:n) {
  if (!is.na(df.sig[i]) & !is.na(df.sig[i+1]) & df.sig[i] < df.sig[i+1]) {
    sig[i,] <- 1
  } else {
    sig[i,] <- -1
  }
}
head(sig)

# combine data with sig
feature.data <- na.omit(merge(res, NEXTDAY=sig))
names(feature.data)
head(feature.data)


##########################
# (3) Apply randomForest #
##########################

# apply randomForest
train.data<-as.data.frame(feature.data["2012-01-01/2014-08-31"])
train.data$NEXTDAY<-factor(train.data$NEXTDAY)
head(train.data);tail(train.data)

test.data<-as.data.frame(feature.data["2014-09-01/"])
test.data$NEXTDAY<-factor(test.data$NEXTDAY,levels=levels(train.data$NEXTDAY))
head(test.data);tail(test.data)

library(randomForest)
mdl_rf<-randomForest(NEXTDAY~.,data=train.data)
mat_tr<-table(predict(mdl_rf),train.data$NEXTDAY)
sum(diag(mat_tr))/sum(mat_tr)
mat_tr[2,2]/sum(mat_tr[2,])

mat_tt<-table(predict(mdl_rf,newdata=test.data),test.data$NEXTDAY)
sum(diag(mat_tt))/sum(mat_tt)
mat_tt[2,2]/sum(mat_tt[2,])

######################
# (4) P&L Simulation #
######################
quotes<-stock.data["2014-09-01/"]
pred <- predict(mdl_rf, test.data)

# sum of next 5-day margins that is more than 2% or less than -2% during simulation period
r <- matrix(NA, ncol=n.days, nrow=nrow(quotes))
head(r)
for(i in 1:n.days) r[,i] <- Next(Delt(Cl(quotes), k=i), i)
ind <- apply(r,1,function(x) 100 * sum(x[x > tgt.margin | x < -tgt.margin]))
if (is.xts(quotes)) ind<-xts(ind, time(quotes)) else ind
head(ind)

# increasing or decreasing of ind with respect to the previous day's sum during simulation period
sig <- ind
head(sig)
df.sig <- coredata(sig)
head(df.sig)
m <- nrow(sig)
for (i in 1:m) {
  if (!is.na(df.sig[i]) & !is.na(df.sig[i+1]) & df.sig[i] < df.sig[i+1]) {
    sig[i, ] <- 1
  } else {
    sig[i, ] <- -1
  }
}

head(sig)

# Calculate P&L
n <- NROW(quotes)
df.quotes <- coredata(quotes)
cols <- c("Timestamp", "Open", "High", "Low", "Close", "Volume", "Adjusted", "Predicted", "bucket", "Price", "Profit", "Tind", "Tsig")
r <- as.data.frame(matrix(NA, ncol=length(cols), nrow=n))
colnames(r) <- cols
head(r)

prev_sig <- -1
bucket <- 0
prev_price <- 0
price <- 0
gains <- 0
profit <- 0
for(i in 1:n) {
  if (bucket == 1) {
    price <- df.quotes[i,'Open']
  } else if (bucket == -1) {
    price <- df.quotes[i,'Open']
  } else if (bucket == 2) {
    price <- df.quotes[i,'Close']
  }
  gains <- ifelse(prev_price == 0, 0, 100 * (price - prev_price) / prev_price)
  profit <- ifelse(bucket == 1, profit, profit + gains)
  
  tind <- ifelse(is.na(round(ind[i], 3)), 0, round(ind[i], 3))
  tsig <- ifelse(is.na(sig[i]), 0, sig[i])
  r[i,] <- c(as.character(index(quotes[i,])),df.quotes[i,'Open'], df.quotes[i,'High'], df.quotes[i,'Low'],df.quotes[i,'Close'], df.quotes[i,'Volume'], df.quotes[i,'Adjusted'],pred[i], bucket, price, profit, tind, tsig)        
  if (prev_sig == 1 & pred[i] == -1) {
    bucket <- -1
  } else if (prev_sig == -1 & pred[i] == 1) {
    bucket <- 1
  } else if (prev_sig == 1) {
    bucket <- 2
  } else bucket <- 0
  prev_sig <- pred[i]
  prev_price <- price
}

# 자료 형 변환
str(r)
for (i in 1:length(cols)) {
  if (i==1) {
    r[,i] <- as.factor(r[,i])
  } else {
    r[,i] <- as.numeric(r[,i])
  }
}
simul.results <-as.xts(r[, 2:length(cols)], order.by=as.POSIXct(r$Timestamp))  
head(simul.results)

# Results Visualization
candleChart(simul.results, TA=NULL, theme='white')
addClose <- newTA(FUN=Cl,col=1,legend='Close')
addClose(on=1)
addVo()
addBBands(n=10)

Predicted <- function(x) x[, "Predicted"]
Profit <- function(x) x[, "Profit"]
Tind <- function(x) x[, "Tind"]
Tsig <- function(x) x[, "Tsig"]

add.Predicted <- newTA(FUN=Predicted, col=6, legend='Predicted', type='b')
add.Predicted()
add.Profit <- newTA(FUN=Profit, col=6, legend='Profit', type='b')
add.Profit()
add.Tind <- newTA(FUN=Tind, col='red', legend='Tind', type='b')
add.Tind()
add.Tsig <- newTA(FUN=Tsig, col='red', legend='Tsig', type='b')
add.Tsig()

