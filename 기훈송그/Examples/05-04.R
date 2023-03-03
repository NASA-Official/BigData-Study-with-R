# 배열
# 행 인덱스, 열 인덱스, 외부 차원
theArray <- array(1:12, dim = c(2,3,2))
theArray

theArray[1, , ]
theArray[, 1, ]
theArray[, , 1]
