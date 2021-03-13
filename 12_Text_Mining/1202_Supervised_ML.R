############################################
###2.2.学生の活動に関する記事の男女の分類###
############################################
#R-3.0.2で実行すること
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/"
setwd(current_directory)

setwd("./2_Text_mining/")
#install.packages("class")
library(class) #knn,knn.cv
#install.packages("kernlab")
library(kernlab) #ksvm
library(MASS) #truehist
#install.packages("randomForest")
library(randomForest)

#####1.データの入力#####
DM.new <- read.csv("2.2_DM.csv") #tf*idf*normによるターム文書行列
data <- t(DM.new[,4:ncol(DM.new)]) #t()は転置行列
rownames(data) <- colnames(DM.new)[4:ncol(DM.new)]
colnames(data) <- DM.new[,1]
rownames(data) #行名の4文字目は性別を表す
data <- as.data.frame(data)

substr(rownames(data),4,4) #行名の4文字目は性別を表す
table(substr(rownames(data),4,4)) #女性29名,男性57名
data <- cbind(data,gender=as.factor(substr(rownames(data),4,4)))

#####2.機械学習により男女を判別する学習器を作成し,予測精度を算出#####
####2.1 kNN法(k近隣法)####
#k-近傍法に関する交差検証法を行う関数
knn_est_k <- function(x,cl,klist) {
  #入力:(x[行列]:入力データ,cl:出力ラベル,
  #     klist[ベクトル]:学習に用いるkの集合)
  #出力:(cv:各kに対する予測誤差の推定値,
  #     k:交差検証法で選ばれた最適なk)
  cv <- NULL
  for (k in klist) {
    kn <- knn.cv(x,cl,k=k) #k-近傍法によるラベルの推定値
    cv <- c(cv,mean(kn!=cl)) #k-近傍法の予測誤差の推定値
  }
  list(cv=cv,k=which.min(cv)) #Error(k)と最小を達成するkを出力
}

#学習データとテストデータの用意
set.seed(0) #安定した結果を得るための乱数セット
trainNO <- sample(nrow(data),round(nrow(data)*0.7,0),replace=FALSE)　#replace=TRUE:非復元一様分布からの抽出
train_dat <- data[trainNO,]
test_dat <- data[-trainNO,]

klist <- c(1:10)
x <- train_dat[,1:(ncol(train_dat)-1)]
y <- train_dat[,"gender"]

#最適なkの推定
kcv <- knn_est_k(x,y,klist)
str(kcv)

#テストデータの準備
xtest <- test_dat[,1:(ncol(test_dat)-1)]
ytest <- test_dat[,"gender"]

#予測誤差の計算
test_err <- NULL
for (k in klist) {
  test_err <- c(test_err,mean(knn(x,xtest,y,k=k)!=ytest))
  print(paste("k = ",k,"is done"))
}
#mean(knn(x,xtest,y,k=k)!=ytest)は,誤予測率

#Error(k)と予測誤差をプロット
errs <- c(kcv$cv,test_err)
ylim=c(min(errs),max(errs))

plot(klist,kcv$cv,xlab="k",ylab="error rate",
     ylim=ylim,main="cross-validation and prediction error",
     type="l",lwd=3,col=2,lty=1)
lines(klist,test_err,lwd=3,col=4,lty=2)
legend("topright",legend=c("x-val error","prediction error"),bty="n",
       col=c(2,4),lty=c(1,1),lwd=c(3,3))

#50回実行し,結果を格納する
#(kはx-val errorを最小にするkを選ぶ)
niter <- 50 #試行回数
knn.result <- rep(0,niter) #予測誤差を格納する場所

t<-proc.time()
for(i in 1:niter) {
  trainNO <- sample(nrow(data),round(nrow(data)*0.7,0),replace=FALSE)　#replace=TRUE:非復元一様分布からの抽出
  train_dat <- data[trainNO,]
  test_dat <- data[-trainNO,]
  klist <- c(1:10)
  dim(train_dat)
  x <- train_dat[,1:(ncol(train_dat)-1)]
  y <- train_dat[,"gender"]
  xtest <- test_dat[,1:(ncol(test_dat)-1)]
  ytest <- test_dat[,"gender"]
  test_err <- NULL
  for (k in klist) {
    test_err <- c(test_err,mean(knn(x,xtest,y,k=k)!=ytest))
    print(paste("i =",i,",","k = ",k,"is done"))
  }
  knn.result[i] <- test_err[kcv$k]
}
knn.time <- proc.time()-t #処理時間

truehist(knn.result,ylab="frequency",xlab="prediction error rate",
         main="prediction error rate of kNN")

####2.2 SVM(サポートベクターマシーン)####
niter <- 50 #試行回数
svm.result <- rep(0,niter) #予測誤差を格納する場所

t<-proc.time()
for(i in 1:niter) {
  trainNO <- sample(nrow(data),round(nrow(data)*0.7,0),replace=FALSE)　#replace=TRUE:非復元一様分布からの抽出
  train_dat <- data[trainNO,]
  test_dat <- data[-trainNO,]
  x <- as.matrix(train_dat[,1:(ncol(train_dat)-1)])
  y <- train_dat[,"gender"]
  xtest <- as.matrix(test_dat[,1:(ncol(test_dat)-1)])
  ytest <- test_dat[,"gender"]
  
  #ガウシアンカーネル
  rbfsvm <- ksvm(x,y,type="C-svc",kernel="rbfdot",scale="FALSE")
  #ガウシアンカーネル:rbfdot:exp(-σ||x-x'||^2)
  #-> データに事前知識がないときに使われる。表現力が高いカーネル関数
  #scale="FALSE"はデータが疎(ほとんどが0)であるときに指定する
  svm.result[i] <- mean(predict(rbfsvm,xtest)!=ytest) #予測誤差
  print(paste("i =",i,"is done"))
}
svm.time <- proc.time()-t #処理時間

truehist(svm.result,ylab="frequency",xlab="prediction error rate",
         main="prediction error rate of SVM")

####2.3 ランダムフォレスト####
##計算速度アップのため,LSI法で次元を縮約する
DM <- t(data[,-ncol(data)])
dim(DM) #5352x86
DM.svd <- svd(DM) #特異値分解
DM86 <- t(DM.svd$u)%*%DM #次元を5785次元から86次元へ縮約
dim(DM86) #86x86
DM86 <- t(DM86)

#試しに1回やってみる
seed.set(0) #結果を安定的に得るための乱数セット
trainNO <- sample(nrow(DM86),round(nrow(DM86)*0.7,0),replace=FALSE)　#replace=TRUE:非復元一様分布からの抽出
train_dat <- as.matrix(DM86[trainNO,])
test_dat <- as.matrix(DM86[-trainNO,])
train_y <- data[trainNO,ncol(data)]
test_y <- data[-trainNO,ncol(data)]
train_rf <- randomForest(x=train_dat,y=train_y,mtry=ncol(train_dat),ntree=500)
plot(train_rf) #tree数を増加させていく毎にerror rateが減少する様子がわかる
train_rf$err.rate
#緑:男性の予測error rate
#黒:OBB(Out-of-Bag)(交差検証法)によるerror rate(に対応する)
#赤:女性の予測error rate
#女性の予測精度が極めて悪い
vali_rfp <- predict(train_rf,test_dat)
(vali_rft<- table(test_y,vali_rfp)) #混同行列(縦軸が正解,横軸が予測)
(accuracyTraining_rf <- sum(diag(vali_rft))/sum(vali_rft)) #精度(正解率)
(FPR_rf <- vali_rft[1,2]/sum(vali_rft[1,])) #False Positive Rate
(FNR_rf <- vali_rft[2,1]/sum(vali_rft[2,])) #False Negative Rate
#精度は61.5%

##ランダムフォレスト法で予測し,精度を検証
niter <- 50 #試行回数
rf.result <- rep(0,niter) #予測誤差を格納する場所

for(i in 1:niter) {
  trainNO <- sample(nrow(DM86),round(nrow(DM86)*0.7,0),replace=FALSE)　#replace=TRUE:非復元一様分布からの抽出
  train_dat <- as.matrix(DM86[trainNO,])
  test_dat <- as.matrix(DM86[-trainNO,])
  train_y <- data[trainNO,ncol(data)]
  test_y <- data[-trainNO,ncol(data)]
  train_rf <- randomForest(x=train_dat,y=train_y,ntree=500)
  vali_rfp <- predict(train_rf,test_dat)
  vali_rft<- table(test_y, vali_rfp)
  rf.result[i] <- 1 - sum(diag(vali_rft))/sum(vali_rft) #予測誤差
  print(paste("i =",i,"is done"))
}

truehist(rf.result,ylab="frequency",xlab="prediction error rate",
         main="prediction error rate of Random Forest")

#####3.計算速度と精度を比較#####
par(mfrow=c(1,1))
boxplot(knn.result,svm.result,rf.result,
        names=c("knn","svm","random forest"),
        main="Prediction Error Rate")
#一番下の線は最小値,一番の上は最大値(外れ値は除く)
#箱は下から順にfirst quantile,中央値,third quantile
