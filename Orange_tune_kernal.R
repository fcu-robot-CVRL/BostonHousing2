# 載入必要套件

pkg.list = c("mlbench", "e1071", "randomForest", "gbm", "caret", "dplyr", "gtools","UBL","DataExplorer")

for(pkg in pkg.list) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 載入資料集
data("BostonHousing2")
names(BostonHousing2)
bh <- BostonHousing2[,-5]


# 切割 5 5old
set.seed(1) # 設定固定亂數種子
i.set = 1:nrow(bh)
u <- ceiling(nrow(bh)/5)
i.test.list = list()
i = 1
while(length(i.set) > u) {
  i.test.list[[i]] = sample(i.set, u)
  i.set = setdiff(i.set, i.test.list[[i]])
  i = i+1
}
i.test.list[[i]] = i.set

# baseline 建模
pred.list = list()
for(i in 1:5) {
  # 由於 randomForest 套件無法處理多餘 53 分類，因此進行粗略處理
  levels(bh$town) = c(levels(bh$town), "other")
  bh$town[bh$town %in% names(table(bh$town))[table(bh$town)<6]] <- "other"
  bh$town <-as.factor(as.character(bh$town))
  
  trn = bh[-i.test.list[[i]], ]
  test = bh[i.test.list[[i]], ]
  
  m.svm <- svm(cmedv ~ ., trn)
  pred.list[["svm"]][[i]] = predict(m.svm, test)
  
  m.rf <- randomForest(cmedv ~ ., trn)
  pred.list[["rf"]][[i]] = predict(m.rf, test)
  
  m.gbm <- gbm(cmedv ~ ., data = trn)
  pred.list[["gbm"]][[i]] = predict(m.gbm, test)
}

# baseline 評估
cal.r2 <- function(pred, actual) {
  tot = sum((actual-mean(actual))^2)
  res = sum((actual-pred)^2)
  r2 = 1-(res/tot)
  return(r2)
}

actual = bh$cmedv[unlist(i.test.list)]
r2.list = list()
models = c("svm", "rf", "gbm")
for(m in models) {
  r2.list[[m]] = cal.r2( unlist(pred.list[[m]]),actual)
}

rm(BostonHousing2, m.gbm, m.rf, m.svm, pred.list, test, trn,
   i, i.set, m, pkg, pkg.list, u)

####### 以上程式碼勿動，以下請自行實作 ##########
data("BostonHousing2")
bh <- BostonHousing2[,-5]
levels(bh$town) = c(levels(bh$town), "other")
bh$town[bh$town %in% names(table(bh$town))[table(bh$town)<13]] <- "other"
bh$town <-as.factor(as.character(bh$town))

# bh$crim = log(bh$crim)
bh$zn = sqrt(bh$zn)
bh$nox_squre = bh$nox^7
bh$rm_squre <- bh$rm^7
bh$age_sqrt = sqrt(bh$age)
bh$dis = 1/bh$dis
bh$lstat =log(bh$lstat)
bh$ptratio_log = log(bh$ptratio)
bh$lstat_square <- (bh$lstat)^2
bh$tax_log = log(bh$tax)
bh$b_squre = bh$b^58
bh$rm_lstat <- bh$rm * bh$lstat
bh$rm_dis <- bh$rm / bh$dis
bh$nox_dis <- bh$nox * bh$dis
bh$age_rad <- bh$age * bh$rad
bh$tax_ptratio <- bh$tax * bh$ptratio


vars <- c("rm", "lstat", "dis", "nox", "tax", "ptratio")
for (i in 1:(length(vars)-1)) {
  for (j in (i+1):length(vars)) {
    colname <- paste(vars[i], vars[j], sep = "_x_")
    bh[[colname]] <- bh[[vars[i]]] * bh[[vars[j]]]
  }
}

pred.svm = NULL
# pred.rf = NULL
# pred.gbm = NULL
for(i in 1:5) {
  
  trn = bh[-i.test.list[[i]], ]
  test = bh[i.test.list[[i]], ]
  print(head(trn))
  plot(density(trn$cmedv))
  trn_pca <- trn [,-5]
  test_pca <- test [,-5]
  
  # Append target variable back only after PCA transformation
  trn_pca$cmedv <- trn$cmedv
  test_pca$cmedv <- test$cmedv
  
  tune.svm <- tune(
    svm, 
    cmedv ~ ., 
    data = trn_pca, 
    kernal = "radial",
    ranges = list(
      cost = c(10.56), 
      gamma = c(0.017),
      epsilon = c(0.05) )
  )
  best.model <- tune.svm$best.model
  
  print(summary(best.model))
  # SVM model on PCA-transformed data
  # m.svm_pca <- svm(cmedv ~ .,trn_pca)
  # m.rf_pca <- randomForest(cmedv ~ ., trn_pca)
  # m.gbm_pca <- gbm(cmedv ~ ., data = trn_pca)
  # 保存預測結果
  pred.svm = c(pred.svm, predict(best.model, test_pca))
  # pred.rf = c(pred.rf, predict(m.rf_pca, test_pca))
  # pred.gbm = c(pred.gbm, predict(m.gbm_pca, test_pca))
}

# pred.rf=NULL
# for(i in 1:5) {
#   
#   trn = bh[-i.test.list[[i]], ]
#   test = bh[i.test.list[[i]], ]
#   trn_pca <- trn [,-5]
#   test_pca <- test [,-5]
#   
#   # Append target variable back only after PCA transformation
#   trn_pca$cmedv <- trn$cmedv
#   test_pca$cmedv <- test$cmedv
#   
#   
#   # SVM model on PCA-transformed data
#   m.rf_pca <- randomForest(cmedv ~ .,trn_pca)
#   
#   # 保存預測結果
#   pred.rf = c(pred.rf, predict(m.rf_pca, test_pca))
# }

###### 填入所使用的方法，評估時使用 ####### 
method = "svm"
###### 填入所使用的方法，評估時使用 ####### 

##### 以下以原始標籤做為評估展示之用，請換成自己模型的預測結果 ########
pred.result = pred.svm
##### 以下以原始標籤做為評估展示之用，請換成自己模型的預測結果 ########

######## 以下為評估部分 ########

print(sprintf("baseline: %.3f, result: %.3f, gain: %.7f",
              r2.list[[method]],
              cal.r2( pred.result,actual),
              (cal.r2( pred.result,actual)-r2.list[[method]])/r2.list[[method]]
))