# PAM (Partitioning Around Medoids)
indicators <- c(
    "BX.KLT.DINV.WD.GD.ZS", "NY.GDP.DEFL.KD.ZG", "NY.GDP.MKTP.CD",
    "NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CD",
    "NY.GDP.PCAP.KD.ZG", "TG.VAL.TOTL.GD.ZS"
)
install.packages("WDI", repos = "http://cran.us.r-project.org")
library(WDI)

wbInfo <- WDI(
    country = "all", indicator = indicators,
    start = 2011, end = 2011, extra = TRUE
)

# 그룹화 된 정보는 제거
wbInfo <- wbInfo[wbInfo$region != "Aggregates", ]

# 아무 값도 없는 나라 제외
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[, indicators])) > 0), ]

# iso 국가 코드가 없는 행 제외
wbInfo <- wbInfo[!is.na(wbInfo$iso2c), ]

# 행 이름을 국가로, 군집화에 사용하진 않음
rownames(wbInfo) <- wbInfo$iso2c

# region, income, lending을 팩터로 정확히 지정
wbInfo$region <- factor(wbInfo$region)
wbInfo$income <- factor(wbInfo$income)
wbInfo$lending <- factor(wbInfo$lending)

View(wbInfo)

# 유지할 열 찾기
# 이 벡터에 없는 것들
keep.cols <- which(!names(wbInfo) %in%
    c("iso2c", "country", "year", "capital", "iso3c"))

# 군집화 적합 실행
library(cluster)
wbPam <- pam(
    x = wbInfo[, keep.cols], k = 12,
    keep.diss = TRUE, keep.data = TRUE
)
# 메도이드로 사용되는 관찰값 보기
wbPam$medoids

plot(wbPam, which.plots = 2, main = "")

download.file(
    url = "http://jaredlander.com/data/worldmap.zip",
    destfile = "data/worldmap.zip", method = "auto"
)
unzip(zipfile = "data/worldmap.zip", exdir = "data")

# 중요한 것은 .shp 파일. 나머지는 R이 알아서 한다.
install.packages("maptools", repos = "http://cran.us.r-project.org")
library(maptools)
world <- readShapeSpatial(
    "data/world_country_admin_boundary_shapefile_with_fips_codes.shp"
)

library(dplyr)
world@data$FipsCntry <- as.character(
    recode(world@data$FipsCntry,
        AU = "AT", AS = "AU", VM = "VN", BM = "MM", SP = "ES",
        PO = "PT", IC = "IL", SF = "ZA", TU = "TR", IZ = "IQ",
        UK = "GB", EI = "IE", SU = "SD", MA = "MG", MO = "MA",
        JA = "JP", SW = "SE", SN = "SG"
    )
)

# id라는 새 열 만들기, 데이터의 행 이름이 들어갈 것
world@data$id <- rownames(world@data)

# 데이터 프레임으로 변환
install.packages("broom", repos = "http://cran.us.r-project.org")
library(broom)

# 원본에는 없으나 다음 두 행이 추가로 필요함
install.packages("gpclib", repos = "http://cran.us.r-project.org")
library(gpclib)
gpclibPermit()

world.df <- tidy(world, region = "id")
head(world.df)
world.df <- left_join(world.df, world@data[, c("id", "CntryName", "FipsCntry")],
    by = "id"
)

clusterMembership <- data.frame(
    FipsCntry = names(wbPam$clustering),
    Cluster = wbPam$clustering,
    stringsAsFactors = FALSE
)

head(clusterMembership)

world.df <- left_join(world.df, clusterMembership, by = "FipsCntry")
world.df$Cluster <- as.character(world.df$Cluster)
world.df$Cluster <- factor(world.df$Cluster, levels = 1:12)

library(ggplot2)
ggplot() +
    geom_polygon(data = world.df, aes(x = long, y = lat, group = group, fill = Cluster, color = Cluster)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank()
    )

wbPam$clusinfo
