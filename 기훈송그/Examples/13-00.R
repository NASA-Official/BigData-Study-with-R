# purrr
# map
theList <- list(A = matrix(1:9, 3),
                B = 1:5,
                C = matrix(1:4, 2),
                D = 2)
lapply(theList, sum)

library(purrr)
theList %>% map(sum)

theList2 <- theList
theList2[[1]][2,1] <- NA
theList2[[2]][4] <- NA

# NA가 있을 경우
theList2 %>% map(function(x) sum(x, na.rm = TRUE))
theList2 %>% map(sum, na.rm = TRUE)

buildDF <- function(x) {
  data.frame(A = 1:x, B = x:1)
}
listOfLengths <- list(3,4,1,5)

# 나누어진 데이터 프레임
listOfLengths %>% map(buildDF)
# 하나의 데이터 프레임
listOfLengths %>% map_df(buildDF)

# 조건에 만족하면 수정
theList %>% map_if(is.matrix, function(x) x *2)

