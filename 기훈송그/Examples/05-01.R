x <- 10:1
y <- -4:5
q <- c("Hotkey", "Football", "Baseball", "Curling", "Rugby", "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")

theDF <-data.frame(First = x, Second = y, Sport = q)
theDF

nrow(theDF)
ncol(theDF)
dim(theDF)

names(theDF)
names(theDF)[3]

rownames(theDF)
rownames(theDF) <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")

rownames(theDF) <- NULL
theDF
head(theDF)
head(theDF, n = 7)

class(theDF)
theDF$Sport

theDF[3, 2]
