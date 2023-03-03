install.packages("reshape2")
library(reshape2)

melt00 <- melt(
  Aid_00s,
  id.vars = c("Country.Name", "Program.Name"),
  variable.name = "Year",
  value.name = "Dollars"
)

tail(melt00, 10)

library(scales)
library(stringr)
library(ggplot2)

# Year열에서 FY 제거
melt00$Year <- as.numeric(str_sub(melt00$Year, 3, 6))

# 데이터를 연도에 따라서 집계
meltAgg <- aggregate(Dollars ~ Program.Name + Year,
                     data = melt00,
                     sum,
                     na.rm = TRUE)
# 프로그램 이름에서 앞의 10 문자만 유지
meltAgg$Program.Name <-
  str_sub(meltAgg$Program.Name, start = 1, end = 10)
# 플롯 그리기
ggplot(meltAgg, aes(x = Year, y = Dollars)) +
  geom_line(aes(group = Program.Name)) +
  facet_wrap(~ Program.Name) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2009, by = 2)) +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 1,
    hjust = 0
  )) +
  scale_y_continuous(labels = multiple_format(extra = dollar, multiple =
                                                "B"))

# dcast
# 첫 번째 = 사용될 데이터
# 두 번째 = 포뮬러
# 세 번째 = 새로운 열들로 뿌려질 값을 담은 열의 이름
cast00 <-
  dcast(melt00, Country.Name + Program.Name ~ Year, value.var = "Dollars")
