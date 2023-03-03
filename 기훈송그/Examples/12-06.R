diamonds %>% mutate(price / carat)
diamonds %>% select(carat, price) %>% mutate(price / carat)
diamonds %>% select(carat, price) %>% mutate(Ratio = price / carat)
diamonds %>% select(carat, price) %>% mutate(Ratio = price / carat, Double = Ratio * 2)

library(magrittr)
diamonds2 <- diamonds

diamonds2 %<>% select(carat, price) %>% mutate(Ratio = price / carat, Double = Ratio * 2)
diamonds2 <- diamonds2 %>% mutate(Quadruple = Double * 2)

summarize(diamonds, mean(price))
diamonds %>% summarize(mean(price))

diamonds %>% summarize(AvgPrice = mean(price),
                       MedianPrice = median(price),
                       AvgCarat = mean(carat))

diamonds %>% group_by(cut) %>% summarize(AvgPrice = mean(price))
diamonds %>% group_by(cut) %>% summarize(AvgPrice = mean(price),
                                         SumCarat = sum(carat))
diamonds %>% group_by(cut, color) %>% summarize(AvgPrice = mean(price),
                                         SumCarat = mean(carat))


diamonds %>% group_by(cut) %>% summarize(AvgPrice = mean(price),
                                         SumCarat = sum(carat)) %>% arrange(AvgPrice)

diamonds %>% group_by(cut) %>% summarize(AvgPrice = mean(price),
                                         SumCarat = sum(carat)) %>% arrange(desc(AvgPrice))

topN <- function(x, N=5) {
  x %>% arrange(desc(price)) %>% head(N)
}
diamonds %>% group_by(cut) %>% do(topN(., N = 3))

diaDBSource <- src_sqlite("./code/R_Study/data/diamonds.db")
diaDBSource

diaDBSource2 <- DBI::dbConnect(RSQLite::SQLite(), "./code/R_Study/data/diamonds.db")
diaDBSource2

diaTab <- tbl(diaDBSource, "diamonds")
diaTab %>% group_by(cut) %>% summarize(Price = mean(price))
