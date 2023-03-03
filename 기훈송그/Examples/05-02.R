# 리스트
# 2개의 요소를 가진 리스트
# 처음은 3개의 요소를 가진 벡터이고
# 두 번째는 5개의 요소를 가진 벡터이다.
# 표현식을 괄호로 감싸면 실행 후 자동으로 출력됨
(list3 <- list(c(1, 2, 3), 3:7))

list6 <-
  list(theDataFrame = theDF,
       TheVector = 1:10,
       TheList = list3)
names(list6)
list6

# 특정 크기를 가진 빈 리스트
(emptyList <- vector(mode = "list", length = 4))

list6[1]
# 리스트의 개별 요소에 접근
list6[[1]]
list6[[1]]$Sport

list6[[1]][, "Second"]
list6[[1]][, "Second", drop = FALSE]

# 길이 확인
length(list6)
# 새 요소 추가
list6[[4]] <- 2
list6[["NewElement"]] <- 3:6
length((list6))
