# R로 데이터 읽어오기
# CSV 파일 읽기
theURL <- "http://www.jaredlander.com/data/TomatoFirst.csv"

# file = 파일경로
# header = 첫 행이 열 이름으로 사용되는가?
# sep = 구분자
tomato <- read.table(file = theURL, header = TRUE, sep = ",")

x <- 10:1
y <- -4:5
q <-
  c(
    "Hotkey",
    "Football",
    "Baseball",
    "Curling",
    "Rugby",
    "Lacrosse",
    "Basketball",
    "Tennis",
    "Cricket",
    "Soccer"
  )
theDF <-
  data.frame(
    First = x,
    Second = y,
    Sport = q,
    stringsAsFactors = TRUE
  )
theDF$Sport

# read_delim
# 어떤 기준에 의해 구분돼 기록되어 있는 것을 읽는다
install.packages("readr")
library(readr)
tomato2 <- read_delim(file = theURL, delim = ",")
tomato2

# fread
install.packages("data.table")
library(data.table)
tomato3 <- fread(input = theURL,
                 sep = ",",
                 header = TRUE)

# read_excel
# 티블 객체로 읽어온다.
# 티블은 스크린의 크기에 맞춰 보여줄 수 있는 부분만 출력한다.
path <- "./R_Study/Examples/data/ExcelExample.xlsx"
download.file(url = "http://www.jaredlander.com/data/ExcelExample.xlsx",
              destfile = path,
              mode = "wb")

install.packages("readxl")
library(readxl)
excel_sheets(path)
tomatoXL <- read_excel(path)
tomatoXL

# 시트 위치로 가져오기
wineXL1 <- read_excel(path, sheet = 2)
View(wineXL1)

# 시트 이름으로 가져오기
wineXL2 <- read_excel(path, sheet = "Wine")
View(wineXL2)

# DB에서 데이터 읽기
pathDB <- "./R_Study/Examples/data/diamonds.db"
download.file(
  "http://www.jaredlander.com/data/diamonds.db",
  destfile = pathDB,
  mode = "wb"
)

install.packages("RSQLite")
drv <- dbDriver('SQLite')
class(drv)

conn <- dbConnect(drv, pathDB)
class(conn)

dbListTables(conn)
dbListFields(conn, name = "diamonds")
dbListFields(conn, name = "DiamondColors")

diamondsTable <- dbGetQuery(conn, "SELECT * FROM diamonds")

# HTML 테이블
install.packages("XML")
install.packages("rvest")
library(XML)
library(rvest)
theURL <-
  "https://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- read_html(theURL) %>% html_table()

# 웹 데이터 스크래핑
library(rvest)
ribalta <-
  read_html("https://www.jaredlander.com/data/ribalta.html")
class(ribalta)

install.packages("stringi")
ribalta %>% html_nodes('ul') %>% html_nodes('span')

ribalta %>% html_nodes('.street')
ribalta %>% html_nodes('.street') %>% html_text()
ribalta %>% html_nodes("#longitude") %>% html_attr("value")

# table.food-items가 여러 개인데 그중 5번째의 것
ribalta %>% html_nodes("table.food-items") %>% magrittr::extract2(5) %>% html_table()

# JSON
library(jsonlite)
pizza <- fromJSON("https://www.jaredlander.com/data/PizzaFavorites.json")
pizza
class(pizza)
