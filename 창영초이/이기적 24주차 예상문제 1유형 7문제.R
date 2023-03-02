
options(repos = c(CRAN = "http://cran.rstudio.com"))

# install.packages("dplyr", dependencies = T)
# install.packages("tidyverse", dependencies = T)
# install.packages("lubridate", dependencies = T)


library(dplyr)
library(tidyverse)
library(lubridate)

main <- read.csv(
  file = 'https://raw.githubusercontent.com/Datamanim/datarepo/main/nasa/nasa.csv',
  fileEncoding = 'UTF-8-BOM'
)

ds1 <- main

# hazardous 컬럼 값들의 unique한 value 빈도는?

uniq.count <- ds1$hazardous
table(uniq.count)


# Q2. relative_velocity 컬럼의 평균 이상 값을 가지는 데이터의 숫자는?
ds2 <- main

mean(ds2$relative_velocity)


temp2 <- ds2 %>% filter(relative_velocity >= mean(ds2$relative_velocity))
count(temp2)


# miss_distance 컬럼의 중앙값 이상 값을 가지는 데이터의 숫자는?

ds3 <- main
temp3 <- ds3 %>% filter(miss_distance >= median(miss_distance))
count(temp3)


# est_diameter_min 컬럼의 평균값 이상 , est_diameter_max컬럼의 평균값 이하를 가지는 데이터의 숫자는?  
ds4 <- main

temp4 <- ds4 %>% filter(est_diameter_min >= mean(est_diameter_min) & est_diameter_max <= mean(est_diameter_max))
count(temp4)


# relative_velocity는 중앙값이상 miss_distance는 평균값 이하의 값을 가지는 데이터의 숫자는?

ds5 <- main
temp5 <- ds5 %>% filter(relative_velocity >= median(relative_velocity) & miss_distance <= mean(miss_distance))
count(temp5)


# hazardous의 값에 따른 miss_distance 평균을 구하여라
ds6 <- main
temp6 <- ds6 %>% group_by(hazardous) %>% summarise(n =  mean(ds6$miss_distance))
temp6



# relative_velocity와 miss_distance의 상관계수를 구하여라
ds7 <- main
temp7 <- cor(ds7$miss_distance , ds7$relative_velocity)
temp7



