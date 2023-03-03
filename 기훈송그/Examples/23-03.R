# 일반화 가법 모형 (Generalized Additive Models)
creditNames <- c(
    "Checking", "Duration", "CreditHistory", "Purpose", "CreditAmount",
    "Savings", "Employment", "InstallmentRate", "GenderMarital",
    "OtherDebtors", "YearsAtResidence", "RealEstate", "Age", "OtherInstallment",
    "Housing", "ExistingCredits", "Job", "NumLiable", "Phone", "Foreign", "Credit"
)
theURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
credit <- read.table(theURL, sep = " ", header = FALSE, col.names = creditNames, stringsAsFactors = FALSE)

# 변환 전
head(credit[, c("CreditHistory", "Purpose", "Employment", "Credit")])

creditHistory <- c(
    A30 = "All Paid", A31 = "All Paid This Bank",
    A32 = "Up To Date", A33 = "Late Payment", A34 = "Critical Account"
)

purpose <- c(
    A40 = "car (new)", A41 = "car (used)",
    A42 = "furniture/equipment", A43 = "radio/television",
    A44 = "domestic appliances", A45 = "repairs",
    A46 = "education", A47 = "(vacation - does not exist?)",
    A48 = "retraining", A49 = "business", A410 = "others"
)

employment <- c(
    A71 = "unemployed", A72 = "< 1 year",
    A73 = "1 - 4 years", A74 = "4 - 7 years", A75 = ">= 7 years"
)

credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]

# 신용을 좋음, 나쁨으로 표시
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")

# 신용도 좋음을 베이스 레벨로 설정
credit$Credit <- factor(credit$Credit, levels = c("Good", "Bad"))

# 변환 후
head(credit[, c("CreditHistory", "Purpose", "Employment", "Credit")])

# 뚜렷한 선형관계가 없음을 확인
install.packages("useful", repos = "http://cran.us.r-project.org")
library(useful)
ggplot(credit, aes(x = CreditAmount, y = Credit)) +
    geom_jitter(position = position_jitter(height = .2)) +
    facet_grid(CreditHistory ~ Employment) +
    xlab("Credit Amount") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
    scale_x_continuous(labels = multiple)

ggplot(credit, aes(x = CreditAmount, y = Age)) +
    geom_point(aes(color = Credit)) +
    facet_grid(CreditHistory ~ Employment) +
    xlab("Credit Amount") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
    scale_x_continuous(labels = multiple)

library(mgcv)
# 로지스틱 GAM
# 텐서 곱을 CreditAmount에, 스플라인을 Age에 적용
creditGam <- gam(
    Credit ~ te(CreditAmount) + s(Age) + CreditHistory + Employment,
    data = credit, family = binomial(link = "logit")
)
summary(creditGam)

# 평활기(smoother)는 자동 적합됨
# 전자는 텐서 곱, 후자는 스플라인
# 그늘진 영역은 평활에 대한 신뢰 구간을 나타냄
plot(creditGam, select = 1, se = TRUE, shade = TRUE)
plot(creditGam, select = 2, se = TRUE, shade = TRUE)
