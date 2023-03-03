# https://www.kaggle.com/code/mirichoi0218/classification-breast-cancer-or-not-with-15-ml
cancer_data <- read.csv("./data/breast_cancer_wisconsin/data.csv",
    stringsAsFactors = FALSE,
    header = TRUE
)
# X 열 제거
cancer_data$X <- NULL
# id 제거
cancer_data <- cancer_data[, -1]
# diagnosis를 factor 형으로
cancer_data$diagnosis <- as.factor(cancer_data$diagnosis)

install.packages("GGally", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(GGally)

# Mean plot
ggpairs(cancer_data[, c(2:11, 1)], aes(color = diagnosis, alpha = 0.75), lower = list(continuous = "smooth")) +
    theme_bw() +
    labs(title = "Cancer Mean") +
    theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5, size = 12))

# Standard Error plot
ggpairs(cancer_data[, c(12:21, 1)], aes(color = diagnosis, alpha = 0.75), lower = list(continuous = "smooth")) +
    theme_bw() +
    labs(title = "Cancer SE") +
    theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5, size = 12))

# Worst plot
ggpairs(cancer_data[, c(22:31, 1)], aes(color = diagnosis, alpha = 0.75), lower = list(continuous = "smooth")) +
    theme_bw() +
    labs(title = "Cancer Worst") +
    theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5, size = 12))

# Mean ggcorr plot
ggcorr(cancer_data[, c(2:11)], name = "corr", label = TRUE) +
    theme(legend.position = "none") +
    labs(title = "Cancer Mean") +
    theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5, size = 12))

# SE ggcorr plot
ggcorr(cancer_data[, c(12:21)], name = "corr", label = TRUE) +
    theme(legend.position = "none") +
    labs(title = "Cancer SE") +
    theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5, size = 12))

# Worst ggcorr plot
ggcorr(cancer_data[, c(22:31)], name = "corr", label = TRUE) +
    theme(legend.position = "none") +
    labs(title = "Cancer Worst") +
    theme(plot.title = element_text(face = "bold", color = "black", hjust = 0.5, size = 12))

# Principal Component Analysis (PCA)
# PCA uses standardized data so that it can avoid data distortion caused by scale difference.
install.packages("factoextra", repos = "http://cran.us.r-project.org")
library(factoextra)
cancer_data_pca <- transform(cancer_data)

# In the results of PCA, if the cumulative proportion is 85% or above,
# it can be determined by the number of principal components.

# PC6까지 누적이 전체 데이터의 88.759%
all_pca <- prcomp(cancer_data_pca[, -1], cor = TRUE, scale = TRUE)
summary(all_pca)

# PC3까지 누적이 88.779%
mean_pca <- prcomp(cancer_data_pca[, c(2:11)], scale = TRUE)
summary(mean_pca)

# PC4까지 누적이 86.774%
se_pca <- prcomp(cancer_data_pca[, c(12:21)], scale = TRUE)
summary(se_pca)

# PC3까지 누적이 85.860%
worst_pca <- prcomp(cancer_data_pca[, c(22:31)], scale = TRUE)
summary(worst_pca)

# PCA 변수
all_var <- get_pca_var(all_pca)
all_var

# 변수와 PCA 사이의 관계
library("corrplot")
corrplot(all_var$cos2, is.corr = FALSE)

# train:test = 7:3으로 랜덤 추출
nrows <- NROW(cancer_data)
set.seed(218) ## fix random value
index <- sample(1:nrows, 0.7 * nrows) ## shuffle and divide

train <- cancer_data[index, ]
test <- cancer_data[-index, ]

prop.table(table(train$diagnosis))
prop.table(table(test$diagnosis))

# applying models
library(caret)

## C5.0
install.packages("C50", repos = "http://cran.us.r-project.org")
library(C50)
learn_c50 <- C5.0(train[, -1], train$diagnosis)
pre_c50 <- predict(learn_c50, test[, -1])
cm_c50 <- confusionMatrix(pre_c50, test$diagnosis)
cm_c50

## C5.0 - Tune
acc_test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL

for (i in 1:50) {
    learn_imp_c50 <- C5.0(train[, -1], train$diagnosis, trials = i)
    p_c50 <- predict(learn_imp_c50, test[, -1])
    accuracy1 <- confusionMatrix(p_c50, test$diagnosis)
    accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(t = seq(1, 50), cnt = accuracy2)

opt_t <- subset(acc, cnt == max(cnt))[1, ]
sub <- paste(
    "Optimal number of trials is",
    opt_t$t, "(accuracy :", opt_t$cnt, ") in C5.0"
)

learn_imp_c50 <- C5.0(train[, -1], train$diagnosis, trials = opt_t$t)
pre_imp_c50 <- predict(learn_imp_c50, test[, -1])
cm_imp_c50 <- confusionMatrix(pre_imp_c50, test$diagnosis)
cm_imp_c50

## rpart
install.packages("rpart", repos = "http://cran.us.r-project.org")
library(rpart)
learn_rp <- rpart(diagnosis ~ ., data = train, control = rpart.control(minsplit = 2))
pre_rp <- predict(learn_rp, test[, -1], type = "class")
cm_rp <- confusionMatrix(pre_rp, test$diagnosis)
cm_rp

## prune
learn_pru <- prune(learn_rp, cp = learn_rp$cptable[which.min(learn_rp$cptable[, "xerror"]), "CP"])
pre_pru <- predict(learn_pru, test[, -1], type = "class")
cm_pru <- confusionMatrix(pre_pru, test$diagnosis)
cm_pru

## OneR
install.packages("RWeka", repos = "http://cran.us.r-project.org")
library("RWeka")
learn_1r <- OneR(diagnosis ~ ., data = train)
pre_1r <- predict(learn_1r, test[, -1])
cm_1r <- confusionMatrix(pre_1r, test$diagnosis)
cm_1r

# JRip
learn_jrip <- JRip(diagnosis ~ ., data = train)
pre_jrip <- predict(learn_jrip, test[, -1])
cm_jrip <- confusionMatrix(pre_jrip, test$diagnosis)
cm_jrip

# naiveBayes
library(e1071)

acc_test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL

for (i in 1:30) {
    learn_imp_nb <- naiveBayes(train[, -1], train$diagnosis, laplace = i)
    p_nb <- predict(learn_imp_nb, test[, -1])
    accuracy1 <- confusionMatrix(p_nb, test$diagnosis)
    accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(l = seq(1, 30), cnt = accuracy2)

opt_l <- subset(acc, cnt == max(cnt))[1, ]
sub <- paste(
    "Optimal number of laplace is",
    opt_l$l, "(accuracy :", opt_l$cnt, ") in naiveBayes"
)

learn_nb <- naiveBayes(train[, -1], train$diagnosis)
pre_nb <- predict(learn_nb, test[, -1])
cm_nb <- confusionMatrix(pre_nb, test$diagnosis)
cm_nb

# random forest
library(randomForest)
learn_rf <- randomForest(diagnosis ~ .,
    data = train,
    ntree = 500, proximity = TRUE, importance = TRUE
)
pre_rf <- predict(learn_rf, test[, -1])
cm_rf <- confusionMatrix(pre_rf, test$diagnosis)
cm_rf

# ctree
install.packages("party", repos = "http://cran.us.r-project.org")
library(party)
learn_ct <- ctree(diagnosis ~ .,
    data = train,
    controls = ctree_control(maxdepth = 2)
)
pre_ct <- predict(learn_ct, test[, -1])
cm_ct <- confusionMatrix(pre_ct, test$diagnosis)
cm_ct

# KNN
library(class)

acc_test <- numeric()

for (i in 1:30) {
    predict <- knn(
        train = train[, -1], test = test[, -1],
        cl = train[, 1], k = i, prob = TRUE
    )
    acc_test <- c(acc_test, mean(predict == test[, 1]))
}

acc <- data.frame(k = seq(1, 30), cnt = acc_test)

opt_k <- subset(acc, cnt == max(cnt))[1, ]
sub <- paste(
    "Optimal number of k is", opt_k$k,
    "(accuracy :", opt_k$cnt, ") in KNN"
)

pre_knn <- knn(
    train = train[, -1], test = test[, -1],
    cl = train[, 1], k = opt_k$k, prob = TRUE
)
cm_knn <- confusionMatrix(pre_knn, test$diagnosis)
cm_knn

# K-Means
predict.kmeans <- function(newdata, object) {
    centers <- object$centers
    n_centers <- nrow(centers)
    dist_mat <- as.matrix(dist(rbind(centers, newdata)))
    dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
    max.col(-dist_mat)
}
library(caret)
learn_kmeans <- kmeans(train[, -1], centers = 2)
pre_kmeans <- predict.kmeans(test[, -1], learn_kmeans)
pre_kmeans <- ifelse(pre_kmeans == 1, "B", "M")
pre_kmeans <- as.factor(pre_kmeans)
cm_kmeans <- confusionMatrix(pre_kmeans, test$diagnosis)
cm_kmeans

# GBM
install.packages("gbm", repos = "http://cran.us.r-project.org")
library(gbm)
test_gbm <- gbm(
    diagnosis ~ .,
    data = train, distribution = "gaussian", n.trees = 10000,
    shrinkage = 0.01, interaction.depth = 4, bag.fraction = 0.5,
    train.fraction = 0.5, n.minobsinnode = 10, cv.folds = 3,
    keep.data = TRUE, verbose = FALSE, n.cores = 1
)
best.iter <- gbm.perf(test_gbm, method = "cv", plot.it = FALSE)
fitControl <- trainControl(method = "cv", number = 5, returnResamp = "all")
learn_gbm <- train(diagnosis ~ .,
    data = train, method = "gbm",
    distribution = "bernoulli", trControl = fitControl, verbose = FALSE,
    tuneGrid = data.frame(
        .n.trees = best.iter, .shrinkage = 0.01,
        .interaction.depth = 1, .n.minobsinnode = 1
    )
)
pre_gbm <- predict(learn_gbm, test[, -1])
cm_gbm <- confusionMatrix(pre_gbm, test$diagnosis)
cm_gbm

# adaBoost
install.packages("ada", repos = "http://cran.us.r-project.org")
library(rpart)
library(ada)
control <- rpart.control(cp = -1, maxdepth = 14, maxcompete = 1, xval = 0)
learn_ada <- ada(diagnosis ~ .,
    data = train, test.x = train[, -1], test.y = train[, 1],
    type = "gentle", control = control, iter = 70
)
pre_ada <- predict(learn_ada, test[, -1])
cm_ada <- confusionMatrix(pre_ada, test$diagnosis)
cm_ada

# SVM
learn_svm <- svm(diagnosis ~ ., data = train)
pre_svm <- predict(learn_svm, test[, -1])
cm_svm <- confusionMatrix(pre_svm, test$diagnosis)
cm_svm

# SVM - Tune
gamma <- seq(0, 0.1, 0.005)
cost <- 2^(0:5)
parms <- expand.grid(cost = cost, gamma = gamma) ## 231

acc_test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL

for (i in 1:NROW(parms)) {
    learn_svm <- svm(diagnosis ~ ., data = train, gamma = parms$gamma[i], cost = parms$cost[i])
    pre_svm <- predict(learn_svm, test[, -1])
    accuracy1 <- confusionMatrix(pre_svm, test$diagnosis)
    accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(p = seq(1, NROW(parms)), cnt = accuracy2)

opt_p <- subset(acc, cnt == max(cnt))[1, ]
sub <- paste(
    "Optimal number of parameter is", opt_p$p,
    "(accuracy :", opt_p$cnt, ") in SVM"
)

learn_imp_svm <- svm(diagnosis ~ .,
    data = train,
    cost = parms$cost[opt_p$p], gamma = parms$gamma[opt_p$p]
)
pre_imp_svm <- predict(learn_imp_svm, test[, -1])
cm_imp_svm <- confusionMatrix(pre_imp_svm, test$diagnosis)
cm_imp_svm

# 모두 비교
col <- c("#ed3b3b", "#0099ff")
par(mfrow = c(3, 5))
fourfoldplot(cm_c50$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("C5.0 (", round(cm_c50$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_imp_c50$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("Tune C5.0 (", round(cm_imp_c50$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_rp$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("RPart (", round(cm_rp$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_pru$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("Prune (", round(cm_pru$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_1r$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("OneR (", round(cm_1r$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_jrip$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("JRip (", round(cm_jrip$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_ct$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("CTree (", round(cm_ct$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_nb$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("NaiveBayes (", round(cm_nb$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_knn$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("Tune KNN (", round(cm_knn$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_kmeans$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("KMeans (", round(cm_kmeans$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_rf$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("RandomForest (", round(cm_rf$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_gbm$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("GBM (", round(cm_gbm$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_ada$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("AdaBoost (", round(cm_ada$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_svm$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("SVM (", round(cm_svm$overall[1] * 100), "%)", sep = "")
)
fourfoldplot(cm_imp_svm$table,
    color = col, conf.level = 0, margin = 1,
    main = paste("Tune SVM (", round(cm_imp_svm$overall[1] * 100), "%)", sep = "")
)

# 최적의 모델 찾기
opt_predict <- c(
    cm_c50$overall[1], cm_imp_c50$overall[1], cm_rp$overall[1],
    cm_pru$overall[1], cm_1r$overall[1], cm_jrip$overall[1], cm_ct$overall[1],
    cm_nb$overall[1], cm_knn$overall[1], cm_kmeans$overall[1], cm_rf$overall[1],
    cm_gbm$overall[1], cm_ada$overall[1], cm_svm$overall[1], cm_imp_svm$overall[1]
)
names(opt_predict) <- c(
    "c50", "imp_c50", "rpart", "prune",
    "1r", "jrip", "ctree", "nb", "knn",
    "kmeans", "rf", "gbm", "ada", "svm", "imp_svm"
)
best_predict_model <- subset(opt_predict, opt_predict == max(opt_predict))
best_predict_model # imp_svm

# imp_svm으로 predict 해보기
patient <- read.csv("./data/breast_cancer_wisconsin/data.csv",
    stringsAsFactors = FALSE,
    header = TRUE
)
patient$X <- NULL

View(patient)

cancer_diagnosis_predict_p <- function(new, method = learn_imp_svm) {
    new_pre <- predict(method, new[, -1])
    new_res <- as.character(new_pre)
    return(paste("Patient ID: ", new[, 1], "  =>  Result: ",
        new_res,
        sep = ""
    ))
}

cancer_diagnosis_predict_s <- function(new, method = learn_imp_svm) {
    new_pre <- predict(method, new[, -1])
    new_res <- as.character(new_pre)
    return(new_res)
}

t <- patient[-index, ]
orgin <- t$diagnosis
t$diagnosis <- NULL
r <- cancer_diagnosis_predict_s(t)

head(r)

sub <- data.frame(
    id = t$id,
    predict_diagnosis = ifelse(r == "M", "M", "B"),
    orgin_diagnosis = orgin
)
sub$correct <- ifelse(
    sub$predict_diagnosis == sub$orgin_diagnosis, "True", "False"
)

install.packages("knitr", repos = "http://cran.us.r-project.org")
library(knitr)
kable(head(sub, 10))
