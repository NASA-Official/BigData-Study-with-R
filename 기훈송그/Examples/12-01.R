library(magrittr)
data(diamonds, package = "ggplot2")
dim(head(diamonds, n = 4))
diamonds %>% head(4) %>% dim

library(ggplot2)
class(diamonds)
head(diamonds)

install.packages("dplyr")
library(dplyr)
head(diamonds)
diamonds

select(diamonds, carat, price)
diamonds %>% select(carat, price)
diamonds %>% select(c(carat, price))

diamonds %>% select(one_of('carat', 'price'))
theCols <- c('carat', 'price')
diamonds %>% select(one_of(theCols))
diamonds[, c('carat', 'price')]
select(diamonds, 1, 7)
diamonds %>% select(1,7)

diamonds %>% select(starts_with('c'))
diamonds %>% select(ends_with('e'))
diamonds %>% select(contains('l'))
diamonds %>% select(matches('r.+t'))
diamonds %>% select(-carat, -price)
diamonds %>% select(-c(1, 7))
diamonds %>% select(-one_of('carat', 'price'))
diamonds %>% filter(cut == "Ideal")
diamonds %>% filter(cut %in% c("Ideal", "Good"))
diamonds %>% filter(price >= 1000)
diamonds %>% filter(carat > 2, price < 14000)

theCol <- "cut"
theCut <- "Ideal"
diamonds %>% filter_(sprintf("%s == '%s'", theCol, theCut))

install.packages("lazyeval")
library(lazyeval)
interp(~a == b, a = as.name(theCol), b = theCut)
diamonds %>% filter(UQE(as.name(theCol)) == theCut)


