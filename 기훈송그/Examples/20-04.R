# 생존 분석
library(survival)
head(bladder)

survObject <- with(bladder[100:105,], Surv(stop, event))
survObject[, 1:2]

# 콕스 비례 위험 모형
cox1 <-
  coxph(Surv(stop, event) ~ rx + number + size + enum, data = bladder)
summary(cox1)
plot(survfit(cox1),
     xlab = "Days",
     ylab = "Survival Rate",
     conf.int = TRUE)

# rx는 플라시보와 치료 여부에 관한 변수
cox2 <-
  coxph(Surv(stop, event) ~ strata(rx) + number + size + enum, data = bladder)
summary(cox2)
plot(survfit(cox2),
     xlab = "Days",
     ylab = "Survival Rate",
     conf.int = TRUE,
     col = 1:2)
legend(
  "bottomleft",
  legend = c(1, 2),
  lty = 1,
  col = 1:2,
  text.col = 1:2,
  title = "rx"
)

# 비례 위험 가정 검정
cox.zph(cox1)
cox.zph(cox2)

# 앤더슨 길
ag1 <- coxph(Surv(start, stop, event) ~ rx + number + size + enum + cluster(id), data = bladder2)
ag2 <- coxph(Surv(start, stop, event) ~ strata(rx) + number + size + enum + cluster(id), data = bladder2)
plot(survfit(ag1), conf.int = TRUE)
plot(survfit(ag2), conf.int = TRUE, col = 1:2,
     text.col = 1:2, title = "rx")

