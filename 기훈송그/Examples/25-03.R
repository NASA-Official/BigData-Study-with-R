# 계층적 군집화
wineH <- hclust(d = dist(wineTrain))
plot(wineH)

View(wbInfo)

str(wbInfo)

# 거리 계산
keep.cols <- which(!names(wbInfo) %in% c(
    "iso2c", "iso3c", "country", "year",
    "capital", "status", "lastupdated", "longitude", "latitude"
))

wbDaisy <- daisy(x = wbInfo[, keep.cols])
wbH <- hclust(wbDaisy)
plot(wbH)

wineH1 <- hclust(dist(wineTrain), method = "single")
wineH2 <- hclust(dist(wineTrain), method = "complete")
wineH3 <- hclust(dist(wineTrain), method = "average")
wineH4 <- hclust(dist(wineTrain), method = "centroid")

par(mfrow = c(2, 2))
plot(wineH1, labels = FALSE, main = "Single")
plot(wineH2, labels = FALSE, main = "Complete")
plot(wineH3, labels = FALSE, main = "Average")
plot(wineH4, labels = FALSE, main = "Centroid")

# 나무 플롯팅
plot(wineH)
# 3개의 군집으로 나눔
rect.hclust(wineH, k = 3, border = "red")
# 13개의 군집으로 나눔
rect.hclust(wineH, k = 13, border = "blue")

# 나무 플롯팅
plot(wineH)
# 200 높이에서 자름
rect.hclust(wineH, h = 200, border = "red")
# 800 높이에서 자름
rect.hclust(wineH, h = 800, border = "blue")
