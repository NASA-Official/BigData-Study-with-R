train <- read.csv("./data/spaceship_titanic/train.csv",
    encoding = "UTF-8",
    na.strings = c("", "na", "NA"),
    header = TRUE,
    stringsAsFactor = TRUE
)

test <- read.csv("./data/spaceship_titanic/test.csv",
    encoding = "UTF-8",
    na.strings = c("", "na", "NA"),
    header = TRUE,
    stringsAsFactor = TRUE
)

# 비율 확인
total <- nrow(train) + nrow(test)
scales::percent(nrow(train) / total) # 67%
scales::percent(nrow(test) / total) # 33%

# 데이터 전처리
test$Transported <- NA
all <- rbind(train, test)
str(all)

library(dplyr)
all <- all %>% select(-Name, -PassengerId)

# NA 찾기
sapply(all, function(x) {
    sum(is.na(x))
})

install.packages("mlr", repos = "http://cran.us.r-project.org")
library(mlr)

all$Age <- impute(all$Age, mean)
all$CryoSleep <- impute(all$CryoSleep, mean)


library(ggplot2)
library(GGally)
ggcorr(train, name = "asdf", label = TRUE) +
    theme(legend.position = "none") +
    labs(title = "Spaceship") +
    theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5, size = 12))

ggpairs(train[, -1], aes(color = Transported, alpha = 0.75),
    lower = list(continuous = "smooth"), cardinality_threshold = NULL
) +
    theme_bw() +
    labs(title = "Spaceship") +
    theme(plot.title = element_text(
        face = "bold", color = "black",
        hjust = 0.5, size = 12
    ))
