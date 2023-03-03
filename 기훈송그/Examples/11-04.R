install.packages("data.table")
library(data.table)
# data.table은 data.frame과 같지만 기능을 개선을 했다.
theDF <- data.frame(A = 1:10,
                    B = letters[1:10],
                    C = LETTERS[11:20],
                    D = rep(c("One", "Two", "Three"), length.out = 10))

theDT <- data.table(A = 1:10,
                    B = letters[1:10],
                    C = LETTERS[11:20],
                    D = rep(c("One", "Two", "Three"), length.out = 10))

diamondsDT <- data.table(diamonds)
tables()
setkey(theDT, D)
theDT
key(theDT)
tables()
theDT["One"]
theDT[c("One", "Two"),]
setkey(diamondsDT, cut, color)
diamondsDT[J("Ideal", "E"),]
diamondsDT[J("Ideal", c("E", "D")),]
