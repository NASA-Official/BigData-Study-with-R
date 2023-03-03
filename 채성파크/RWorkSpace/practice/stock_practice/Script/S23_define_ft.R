############################
# (22) Define the function #
############################

T.ind <- function(x, tgt.margin=0.02, n.days=5) {
  r <- matrix(NA, ncol=n.days, nrow=NROW(x))
  for(i in 1:n.days) r[,i] <- Next(Delt(Cl(x), k=i), i)
  z <- apply(r,1,function(x) 100 * sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(x)) z<-xts(z, time(x)) else z
  return(z)
}

T.sig <- function(ind, bound=0) {
  sig <- ind
  df.sig <- coredata(sig)
  n <- nrow(sig)
  for (i in 1:n) {
    if (!is.na(df.sig[i]) & !is.na(df.sig[i+1]) & df.sig[i] < df.sig[i+1]) {
      sig[i, ] <- 1
    } else {
      sig[i, ] <- -1
    }
  }  
  return(sig)
}


############################
# (22) Define the function #
############################

myROC1.roc <- function(x) ROC(x[, "Close"], type="discrete", n=1)
myROC2.roc <- function(x) ROC(x[, "Close"], type="discrete", n=2)
myROC3.roc <- function(x) ROC(x[, "Close"], type="discrete", n=3)
myROC5.roc <- function(x) ROC(x[, "Close"], type="discrete", n=5)
myROC10.roc <- function(x) ROC(x[, "Close"], type="discrete", n=10)
myVROC <- function(x) ROC(x[, "Volume"], type="discrete", n=3)

myBBands.pctB <- function(x) BBands(HLC(x), n=10)[,4]

mySMI.SMI <- function(x) SMI(x[, c("High","Low","Close")])[,1]
mySMI.signal <- function(x) SMI(x[, c("High","Low","Close")])[,2]
mySMI3MA.smi <- function(x) SMI(x[, c("High","Low","Close")],maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)))[,1]
mySMI3MA.signal <- function(x) SMI(x[, c("High","Low","Close")],maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)))[,2]

myMACD.macd <- function(x) MACD(Cl(x), nFast=6, nSlow=13, nSig=5)[,1]
myMACD.signal <- function(x) MACD(x[, "Close"], nFast=6, nSlow=13, nSig=5)[,2]

myStoch.fastK <- function(x) stoch(x[, c("High","Low","Close")], nFastK=7)[,1]
myStoch.1fastD <- function(x) stoch(x[, c("High","Low","Close")], nFastK=7)[,2]
myStoch.2fastD <- function(x) stoch(x[, c("High","Low","Close")], nFastK=6)[,2]
myStoch.3fastD <- function(x) stoch(x[, c("High","Low","Close")], nFastK=5)[,2]
myStoch.4fastD <- function(x) stoch(x[, c("High","Low","Close")], nFastK=4)[,2]
myStoch.slowD <- function(x) stoch(x[, c("High","Low","Close")], nFastK=7)[,3]

myStoch2MA.fastK <- function(x) stoch(HLC(x), maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)), nFastK=7)[,1]
myStoch2MA.fastD <- function(x) stoch(HLC(x), maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)), nFastK=7)[,2]
myStoch2MA.slowD <- function(x) stoch(HLC(x), maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)), nFastK=7)[,3]

myStochRSI.fastK <- function(x) stoch(RSI(Cl(x)))[,1]
myStochRSI.fastD <- function(x) stoch(RSI(Cl(x), n=7), nFastK=7)[,2]
myStochRSI.slowD <- function(x) stoch(RSI(Cl(x)))[,3]

myRSI.rsi <- function(x) RSI(x[, c("Close")], n=7)[,1]

myATR.tr <- function(x) ATR(HLC(x))[,1]
myATR.atr <- function(x) ATR(HLC(x))[,2]
myATR.trueHigh <- function(x) ATR(HLC(x))[,3]
myATR.trueLow <- function(x) ATR(HLC(x))[,4]

myAroon.up <- function(x) aroon(x[, c("High", "Low")])[,1]
myAroon.down <- function(x) aroon(x[, c("High", "Low")])[,2]
myAroon.osci <- function(x) aroon(x[, c("High", "Low")], n=10)[,3]

myVolatility <- function(x) volatility(OHLC(x), n=5, calc="close")

myEMA7 <- function(x) EMA(Cl(x),7)
myEMA50 <- function(x) EMA(Cl(x),50)
myEMA120 <- function(x) EMA(Cl(x),120)
myEMA7to50 <- function(x) (EMA(Cl(x),7) - EMA(Cl(x),50)) / EMA(Cl(x),50)
myEMA7to120 <- function(x) (EMA(Cl(x),7) - EMA(Cl(x),120)) / EMA(Cl(x),120)
myEMA50to120 <- function(x) (EMA(Cl(x),50) - EMA(Cl(x),120)) / EMA(Cl(x),120)

mySMA7 <- function(x) SMA(Cl(x),7)
mySMA50 <- function(x) SMA(Cl(x),50)
mySMA120 <- function(x) SMA(Cl(x),120)
mySMA7to50 <- function(x) (SMA(Cl(x),7) - SMA(Cl(x),50)) / SMA(Cl(x),50)
mySMA7to120 <- function(x) (SMA(Cl(x),7) - SMA(Cl(x),120)) / SMA(Cl(x),120)
mySMA50to120 <- function(x) (SMA(Cl(x),50) - SMA(Cl(x),120)) / SMA(Cl(x),120)

myCMO15 <- function(x) CMO(Cl(x),n=15)
myDEMA20 <- function(x) DEMA(Cl(x),20)

########################
# (23) Merge Variables #
########################

addFewerFeatures = function(x) {
  res <- myROC1.roc(x)
  res <- merge(res, myROC2.roc(x), all=FALSE)
  res <- merge(res, myROC3.roc(x), all=FALSE)
  res <- merge(res, myROC5.roc(x), all=FALSE)
  res <- merge(res, myVROC(x), all=FALSE)
  
  res <- merge(res, myBBands.pctB(x), all=FALSE)
  
  res <- merge(res, mySMI.SMI(x), all=FALSE)
  res <- merge(res, mySMI.signal(x), all=FALSE)
  res <- merge(res, mySMI3MA.smi(x), all=FALSE)
  res <- merge(res, mySMI3MA.signal(x), all=FALSE)
  
  res <- merge(res, myMACD.macd(x), all=FALSE)
  res <- merge(res, myMACD.signal(x), all=FALSE)
  
  res <- merge(res, myStoch.fastK(x), all=FALSE)
  res <- merge(res, myStoch.1fastD(x), all=FALSE)
  res <- merge(res, myStoch.2fastD(x), all=FALSE)
  res <- merge(res, myStoch.3fastD(x), all=FALSE)
  res <- merge(res, myStoch.4fastD(x), all=FALSE)
  res <- merge(res, myStoch.slowD(x), all=FALSE)
  
  res <- merge(res, myStoch2MA.fastK(x), all=FALSE)
  res <- merge(res, myStoch2MA.fastD(x), all=FALSE)
  res <- merge(res, myStoch2MA.slowD(x), all=FALSE)
  
  res <- merge(res, myStochRSI.fastK(x), all=FALSE)
  res <- merge(res, myStochRSI.fastD(x), all=FALSE)
  res <- merge(res, myStochRSI.slowD(x), all=FALSE)
  
  res <- merge(res, myRSI.rsi(x), all=FALSE)
  
  res <- merge(res, myATR.tr(x), all=FALSE)  
  res <- merge(res, myATR.atr(x), all=FALSE)  
  res <- merge(res, myATR.trueHigh(x), all=FALSE)  
  res <- merge(res, myATR.trueLow(x), all=FALSE)  
  
  res <- merge(res, myAroon.up(x), all=FALSE)
  res <- merge(res, myAroon.down(x), all=FALSE)
  res <- merge(res, myAroon.osci(x), all=FALSE)
  
  res <- merge(res, myVolatility(x), all=FALSE)
    
  res <- merge(res, myEMA7(x), all=FALSE) 
  res <- merge(res, myEMA50(x), all=FALSE) 
  res <- merge(res, myEMA120(x), all=FALSE) 
  res <- merge(res, myEMA7to50(x), all=FALSE) 
  res <- merge(res, myEMA7to120(x), all=FALSE) 
  res <- merge(res, myEMA50to120(x), all=FALSE) 
  
  res <- merge(res, mySMA7(x), all=FALSE) 
  res <- merge(res, mySMA50(x), all=FALSE) 
  res <- merge(res, mySMA120(x), all=FALSE) 
  res <- merge(res, mySMA7to50(x), all=FALSE) 
  res <- merge(res, mySMA7to120(x), all=FALSE) 
  res <- merge(res, mySMA50to120(x), all=FALSE) 
  
  res <- merge(res, myCMO15(x), all=FALSE)
  res <- merge(res, myDEMA20(x), all=FALSE)
  
  return(res)
}

#####################################
# (24) Functions for P&L Simulation #
#####################################

T.simulator <- function(quotes, pred) {
  cov.pred <- function(p) {
    ifelse(p == "b", 1, 
           ifelse(p == "s", -1, 0))
  }
  
  n <- NROW(quotes)
  ind <- T.ind(quotes)
  pred <- cov.pred(pred)
  sig <- T.sig(ind)
  df.quotes <- coredata(quotes)
  
  cols <- c("Timestamp", "Open", "High", "Low", "Close", "Volume", "Adjusted", 
            "Predicted", "bucket", "Price", "Profit", "Tind", "Tsig")
  r <- as.data.frame(matrix(NA, ncol=length(cols), nrow=n))
  colnames(r) <- cols
  
  # 기본 정보 저장
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
  
  # 수익률 계산
  prev_sig <- -1
  for (i in 1:n) {
    if (prev_sig != sig[i]) {
      
    }
  }
  
  # 자료 형 변환
  for (i in 1:length(cols)) {
    if (i==1) {
      r[,i] <- as.factor(r[,i])
    } else {
      r[,i] <- as.numeric(r[,i])
    }
  }
  
  
  z <-as.xts(r[, 2:length(cols)], order.by=as.POSIXct(r$Timestamp))
  
  return(z)
}

Predicted <- function(x) x[, "Predicted"]
Profit <- function(x) x[, "Profit"]
Tind <- function(x) x[, "Tind"]
Tsig <- function(x) x[, "Tsig"]

save(list=ls(),file="ft.rdata")
