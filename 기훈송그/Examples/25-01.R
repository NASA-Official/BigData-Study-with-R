# 군집화
# K-평균 군집화
wineUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.table(wineUrl,
    header = FALSE, sep = ",", stringsAsFactors = FALSE,
    col.names = c(
        "Cultivar", "Alcohol", "Malic.acid", "Ash", "Alcalinity.of.ash", "Magnesium",
        "Total.phenols", "Flavanoids", "Nonflavanoid.phenols", "Proanthocyanin", "Color.intensity",
        "Hue", "OD280.OD315.of.diluted.wines", "Proline"
    )
)

head(wine)

# Cultivar(품종), 품종에 따른 포도주의 화학적 조성을 조사한 것이므로
# 이것에 의해 대부분의 그룹 멤버십이 결정될 수 있으므로 배제
wineTrain <- wine[, which(names(wine) != "Cultivar")]

# 클러스터의 개수 설정, 클러스터 수만큼 관찰값 분류
set.seed(278613)
wineK3 <- kmeans(x = wineTrain, centers = 3)
wineK3

library(useful)
plot(wineK3, data = wineTrain)
plot(wineK3, data = wine, class = "Cultivar")

# 초기 랜덤 조건 바꿔보기, nstart 인자
set.seed(278613)
wineK3N25 <- kmeans(wineTrain, centers = 3, nstart = 25)

wineK3$size
wineK3N25$size

wineBest <- FitKMeans(wineTrain, max.clusters = 20, nstart = 25, seed = 278613)
wineBest

PlotHartigan(wineBest)

table(wine$Cultivar, wineK3N25$cluster)

plot(
    table(wine$Cultivar, wineK3N25$cluster),
    main = "Confusion Matrix for Wine Clustering",
    xlab = "Cultivar", ylab = "Cluster"
)

library(cluster)
theGap <- clusGap(wineTrain, FUNcluster = pam, K.max = 20)
gapDF <- as.data.frame(theGap$Tab)
gapDF

# logW 곡선
ggplot(gapDF, aes(x = 1:nrow(gapDF))) + # nolint
    geom_line(aes(y = logW), color = "blue") +
    geom_point(aes(y = logW), color = "blue") +
    geom_line(aes(y = E.logW), color = "green") +
    geom_point(aes(y = E.logW), color = "green") +
    labs(x = "Number of Clusters")

# 갭 곡선
ggplot(gapDF, aes(x = 1:nrow(gapDF))) + # nolint
    geom_line(aes(y = gap), color = "blue") +
    geom_point(aes(y = gap), color = "blue") +
    geom_errorbar(aes(ymin = gap - SE.sim, ymax = gap + SE.sim), color = "red") +
    labs(x = "Number of Clusters", y = "Gap")
