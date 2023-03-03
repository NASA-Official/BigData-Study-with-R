library(quantmod)
library(Quandl)

stck_sec<-Quandl("GOOG/KRX_005930", trim_start="2014-06-13", trim_end="2014-12-12")
head(stck_sec)
stck_sec$Adjusted <- stck_sec$Close
row.names(stck_sec) <- stck_sec[,"Date"]

stock.data_sec <- as.xts(stck_sec[,-1],dateFormat="POSIXct")

candleChart(stock.data_sec,TA=NULL, theme='white',name="KRX_005930 Samsung Electronics Co Ltd")
addRSI(n = 14)

candleChart(stock.data_sec,TA=NULL, theme='white',name="KRX_005930 Samsung Electronics Co Ltd")
addCMO(n = 20)

candleChart(stock.data_sec,TA=NULL, theme='white',name="KRX_005930 Samsung Electronics Co Ltd")
addCMF(n = 20)

stck_sfmi<-Quandl("GOOG/KRX_000810", trim_start="2014-02-26", trim_end="2014-08-25")
head(stck_sfmi)
stck_sfmi$Adjusted <- stck_sfmi$Close
row.names(stck_sfmi) <- stck_sfmi[,"Date"]
stock.data_sfmi <- as.xts(stck_sfmi[,-1],dateFormat="POSIXct")

candleChart(stock.data_sfmi,TA=NULL, theme='white',name="KRX_000810 Samsung Fire & Marine Insurance Co Ltd")
addADX(n = 14)

candleChart(stock.data_sfmi,TA=NULL, theme='white',name="KRX_000810 Samsung Fire & Marine Insurance Co Ltd")
addCMF(n = 20)

stck_lgd<-Quandl("GOOG/KRX_034220", trim_start="2014-06-13", trim_end="2014-12-12")
head(stck_lgd)
stck_lgd$Adjusted <- stck_lgd$Close
row.names(stck_lgd) <- stck_lgd[,"Date"]

stock.data_lgd <- as.xts(stck_lgd[,-1],dateFormat="POSIXct")
candleChart(stock.data_lgd,TA=NULL, theme='white',name="KRX_034220 LG Display Co Ltd")
addCCI(n = 20)

candleChart(stock.data_lgd,TA=NULL, theme='white',name="KRX_034220 LG Display Co Ltd")
addCMO(n = 20)

candleChart(stock.data_lgd,TA=NULL, theme='white',name="KRX_034220 LG Display Co Ltd")
addCMF(n = 20)
