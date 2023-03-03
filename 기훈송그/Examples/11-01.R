# 그룹별 데이터 조작
theMatrix <- matrix(1:9, nrow = 3)
View(theMatrix)
# apply - 행이나 열을 순회하며 행 또는 열을 하나의 입력값으로 취급하면서 세 번째 인자의 함수를 실행
# 첫 번째 = 작업하는 객체
# 두 번째 = 함수를 적용시킬 마진
# 세 번째 = 함수
# 나머지 = 명시한 함수의 인자

# 행에 대한 합
apply(theMatrix, 1, sum )
rowSums(theMatrix)

# 열에 대한 합
apply (theMatrix, 2, sum)
colSums(theMatrix)

theMatrix[2, 1] <- NA
apply(theMatrix, 1, sum)
# NA 제외하고 계산
apply(theMatrix, 1, sum, na.rm = TRUE)
rowSums(theMatrix, na.rm = TRUE)

# lapply - 리스트의 각 요소에 함수 적용하고 결과값을 리스트로 반환
theList <- list(A = matrix(1:9, 3), B = 1:5, C = matrix(1:4, 2), D = 2)
lapply(theList, sum)

# sapply - lapply와 같으나 벡터 반환
sapply(theList, sum)

# mapply - 각 리스트에 대해 함수 적용
firstList <- list(A = matrix(1:16, 4), B = matrix(1:16, 2), C=1:5)
secondList <- list(A = matrix(1:16, 4), B = matrix(1:16, 8), C=15:1)
mapply(identical, firstList, secondList)

simpleFunc <- function(x, y){
  NROW(x) + NROW(y)
}
mapply(simpleFunc, firstList, secondList)
