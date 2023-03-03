# 행렬
# 5x2 행렬
A <- matrix(1:10, nrow = 5)
B <- matrix(21:30, nrow = 5)

# 2x10 행렬
C <- matrix(21:40, nrow = 2)

A + B
# 요소 사이의 곱
A * B
# 행렬 곱
A %*% t(B)
A == B

# 데이터 프레임과 비슷하게 행,열 이름 지정 가능
colnames(A) <- c("Left", "Right")
rownames(A) <- c("1st", "2nd", "3rd", "4th", "5th")
colnames(B) <- c("First", "Second")
rownames(B) <- c("One", "Two", "Three", "Four", "Five")
colnames(C) <- LETTERS[1:10]
rownames(C) <- c("Top", "Bottom")

# 전치하면 행과 열의 이름을 바꿈
t(A)
# 왼쪽 행렬의 행의 이름, 오른쪽 행렬의 열의 이름을 유지
A %*% C
