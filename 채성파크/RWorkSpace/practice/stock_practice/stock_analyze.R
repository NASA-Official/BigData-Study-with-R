install.packages("sqldf")
install.packages("reshape2")
install.packages("tcltk")
install.packages("rpart")
install.packages("party")
install.packages("partykit")
install.packages("caret", dependencies = TRUE)
install.packages("doParallel")

library(sqldf)
library(reshape2)
library(party)
library(rpart)
library(party)
library(partykit)
library(ggplot2)
library(caret)
library(doParallel)
registerDoParallel(7)

#파일 읽어와 변수에 저장
load("sise.rdata")
stock <- sise

#데이터 확인
str(stock)

#열 이름 소문자로 변경
colnames(stock) <- tolower(colnames(stock))

#데이터 확인
str(stock)

#최고가 - 시작가 데이터 추가
stock$gap_high <- stock$high - stock$open
#시작가 - 최저가 데이터 추가
stock$gap_low <- stock$open - stock$low

#최고가가 시작가보다 10% 이상 높은 애들만 추출
ind_high <- which(stock$gap_high/stock$open > 0.10)
#최저가가 시작가보다 10% 이상 낮은 애들만 추출
ind_low <- which(stock$gap_low/stock$open > 0.10)

str(ind_high)
str(ind_low)

stock$target_high <- 0
stock[ind_high, "target_high"] <- 1

stock$target_low <- 0
stock[ind_low, "target_low"] <- 1

View(stock)

prop.table(table(stock$target_high))
prop.table(table(stock$target_low))

#stock_mart1에 타깃 일자에 대한 기준값들 저장

#훈련 데이터
stock_1m <- sqldf("select name, code, avg(high) as high_1m, avg(low) as low_1m, avg(open) as open_1m, avg(close) as close_1m, sum(target_high) as t_h_1m, sum(target_low) as t_l_1m from stock where date <= '2014.08.31' and date >= '2014.08.01' group by code, name")
stock_mart1 <- sqldf("select stock.* from stock where date = '2014.09.01'")

stock_3m <- sqldf("select name, code, avg(high) as high_3m, avg(low) as low_3m, avg(open) as open_3m, avg(close) as close_3m, sum(target_high) as t_h_3m, sum(target_low) as t_l_3m from stock where date <= '2014.08.31' and date >= '2014.06.02' group by code, name")
stock_5m <- sqldf("select name, code, avg(high) as high_5m, avg(low) as low_5m, avg(open) as open_5m, avg(close) as close_5m, sum(target_high) as t_h_5m, sum(target_low) as t_l_5m from stock where date <= '2014.08.31' and date >= '2014.04.01' group by code, name")

#테스트
test1 <- sqldf('select stock_3m.*, stock_1m.high_1m, stock_1m.low_1m, stock_1m.open_1m, stock_1m.close_1m, stock_1m.t_h_1m, stock_1m.t_l_1m from stock_3m, stock_1m where stock_1m.name = stock_3m.name')
test2 <- sqldf('select test1.*, stock_5m.high_5m, stock_5m.low_5m, stock_5m.open_5m, stock_5m.close_5m, stock_5m.t_h_5m, stock_5m.t_l_5m from test1, stock_5m where test1.name = stock_5m.name')
test3 <- sqldf('select test2.*, stock_mart1.target_high, stock_mart1.volume from test2, stock_mart1 where test2.name = stock_mart1.name')

test3$target_high <- factor(test3$target_high)
str(test3)
test4 <- na.omit(test3)

#검증 데이터
stock_1m1 <- sqldf("select name, code, avg(high) as high_1m, avg(low) as low_1m, avg(open) as open_1m, avg(close) as close_1m, sum(target_high) as t_h_1m, sum(target_low) as t_l_1m from stock where date <= '2014.09.01' and date >= '2014.08.02' group by code, name")
stock_mart11 <- sqldf("select stock.* from stock where date = '2014.09.02'")

stock_3m1 <- sqldf("select name, code, avg(high) as high_3m, avg(low) as low_3m, avg(open) as open_3m, avg(close) as close_3m, sum(target_high) as t_h_3m, sum(target_low) as t_l_3m from stock where date <= '2014.09.01' and date >= '2014.06.03' group by code, name")
stock_5m1 <- sqldf("select name, code, avg(high) as high_5m, avg(low) as low_5m, avg(open) as open_5m, avg(close) as close_5m, sum(target_high) as t_h_5m, sum(target_low) as t_l_5m from stock where date <= '2014.09.01' and date >= '2014.04.02' group by code, name")

#테스트
test11 <- sqldf('select stock_3m1.*, stock_1m1.high_1m, stock_1m1.low_1m, stock_1m1.open_1m, stock_1m1.close_1m, stock_1m1.t_h_1m, stock_1m1.t_l_1m from stock_3m1, stock_1m1 where stock_1m1.name = stock_3m1.name')
test21 <- sqldf('select test11.*, stock_5m1.high_5m, stock_5m1.low_5m, stock_5m1.open_5m, stock_5m1.close_5m, stock_5m1.t_h_5m, stock_5m1.t_l_5m from test11, stock_5m1 where test11.name = stock_5m1.name')
test31 <- sqldf('select test21.*, stock_mart11.target_high, stock_mart11.volume from test21, stock_mart11 where test21.name = stock_mart11.name')

test31$target_high <- factor(test31$target_high)
colnames(test31) <- tolower(colnames(test31))
str(test31)
test41 <- na.omit(test31)

#검증데이터2
stock_1m2 <- sqldf("select name, code, avg(high) as high_1m, avg(low) as low_1m, avg(open) as open_1m, avg(close) as close_1m, sum(target_high) as t_h_1m, sum(target_low) as t_l_1m from stock where date <= '2014.09.02' and date >= '2014.08.03' group by code, name")
stock_mart12 <- sqldf("select stock.* from stock where date = '2014.09.02'")

stock_3m2 <- sqldf("select name, code, avg(high) as high_3m, avg(low) as low_3m, avg(open) as open_3m, avg(close) as close_3m, sum(target_high) as t_h_3m, sum(target_low) as t_l_3m from stock where date <= '2014.09.02' and date >= '2014.06.04' group by code, name")
stock_5m2 <- sqldf("select name, code, avg(high) as high_5m, avg(low) as low_5m, avg(open) as open_5m, avg(close) as close_5m, sum(target_high) as t_h_5m, sum(target_low) as t_l_5m from stock where date <= '2014.09.02' and date >= '2014.04.03' group by code, name")

#테스트
test12 <- sqldf('select stock_3m2.*, stock_1m2.high_1m, stock_1m2.low_1m, stock_1m2.open_1m, stock_1m2.close_1m, stock_1m2.t_h_1m, stock_1m2.t_l_1m from stock_3m2, stock_1m2 where stock_1m2.name = stock_3m2.name')
test22 <- sqldf('select test12.*, stock_5m2.high_5m, stock_5m2.low_5m, stock_5m2.open_5m, stock_5m2.close_5m, stock_5m2.t_h_5m, stock_5m2.t_l_5m from test12, stock_5m2 where test12.name = stock_5m2.name')
test32 <- sqldf('select test22.*, stock_mart12.target_high, stock_mart12.volume from test22, stock_mart12 where test22.name = stock_mart12.name')

test32$target_high <- factor(test32$target_high)
colnames(test32) <- tolower(colnames(test32))
str(test32)
test42 <- na.omit(test32)

stock.ctree1 <- ctree(target_high~., data=test3[,c(1:2)])
table(predict(stock.ctree1), test3$target_high)
