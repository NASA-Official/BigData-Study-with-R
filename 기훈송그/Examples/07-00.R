install.packages("ggplot2")
library(ggplot2)
data(diamonds)

ggplot(data = diamonds) + geom_histogram(aes(x = carat))

ggplot(data = diamonds) + geom_density(aes(x = carat), fill = "grey50")

g <- ggplot(diamonds, aes(x = price, y = price))
g + geom_point(aes(color = color))

g + geom_point(aes(color = color)) + facet_wrap(cut ~ clarity)
