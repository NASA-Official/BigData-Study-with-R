sport <- c("Hockey", "Baseball", "Football")
league <- c("NHL", "MLB", "NFL")
trophy <-
  c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy")
trophies1 <- cbind(sport, league, trophy)
trophies2 <- data.frame(
  sport = c("Basketball", "Golf"),
  league = c("NBA", "PGA"),
  trophy = c("Larry O'Brien Championship Trophy", "Wanamaker Trophy"),
  stringsAsFactors = FALSE
)

# row가 늘어난다.
trophies <- rbind(trophies1, trophies2)

# col 기준으로 정렬
cbind(Sport=sport, Association=league, Prize=trophy)

download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
              destfile = "./R_Study/data/ForeignAid.zip")

unzip("./R_Study/data/ForeignAid.zip", exdir = "./R_Study/data")

library(stringr)
# 파일들의 리스트를 먼저 얻는다.
theFiles <- dir("./R_Study/data/", pattern = "\\.csv")
# 파일 순회
for (a in theFiles) {
  nameToUse <- str_sub(string = a, start = 12, end = 18)
  temp <- read.table(file = file.path("./R_Study/data", a),
                     header = TRUE, sep = ",",
                     stringsAsFactors = FALSE)
  # 읽은 데이터를 워크스페이스에 할당
  assign(x = nameToUse, value = temp)
}

Aid90s00s <- merge(x = Aid_90s, y = Aid_00s,
                   by.x = c("Country.Name", "Program.Name"),
                   by.y = c("Country.Name", "Program.Name"))

install.packages("plyr")
library(plyr)
Aid90s00sJoin <- join(x = Aid_90s, y = Aid_00s,
                      by = c("Country.Name", "Program.Name"))

frameNames <- str_sub(string = theFiles, start = 12, end = 18)
frameList <- vector("list", length(frameNames))
names(frameList) <- frameNames

for(a in frameNames) {
  frameList[[a]] <- eval(parse(text = a))
}

head(frameList[[1]])
allAid <- Reduce(function(...) {
  join(..., by = c("Country.Name", "Program.Name"))},
  frameList)

install.packages("useful")
library(useful)
corner(allAid, c = 15)

library(data.table)
dt90 <- data.table(Aid_90s, key = c("Country.Name", "Program.Name"))
dt00 <- data.table(Aid_00s, key = c("Country.Name", "Program.Name"))
dt0090 <- dt90[dt00]
