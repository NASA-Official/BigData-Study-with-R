# 1.로지스틱 회귀
acs <- read.table(
  "http://jaredlander.com/data/acs_ny.csv",
  sep = ",",
  header = TRUE,
  stringsAsFactors = FALSE
)

acs$Income <- with(acs, FamilyIncome >= 150000)
library(ggplot2)

install.packages("useful")
library(useful)

income1 <-
  glm(
    Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType,
    data = acs,
    family = binomial(link = "logit")
  )
summary(income1)

invlogit <- function(x) {
  1 / (1 + exp(-x))
}
invlogit(income1$coefficients)

# 2.포아송 회귀
children1 <-
  glm(
    NumChildren ~ FamilyIncome + FamilyType + OwnRent,
    data = acs,
    family = poisson(link = "log")
  )

ggplot(acs, aes(x = NumChildren)) + geom_histogram(binwidth = 1)

summary(children1)

install.packages("coefplot")
install.packages("stringi")
library(stringi)
library(coefplot)
coefplot(children1)

# 표준화된 잔차
z <- (acs$NumChildren - children1$fitted.values) / sqrt(children1$fitted.values)

# 과대산포 팩터
sum(z^2) / children1$df.residual

# 과대산포 p-값
pchisq(sum(z^2), children1$df.residual)

children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, 
                 data = acs, family = quasipoisson(link = "log"))
multiplot(children1, children2)
