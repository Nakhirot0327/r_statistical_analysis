#setwd("各自の作業フォルダ") #各自の作業ディレクトリを設定して下さい
library(MASS)

#####1.1 コイントスの結果シュミレーション#####
###まずは,コインの枚数10枚,試行回数は100回###
niter <- 100 #試行の回数。niterはnumber of iterationsの略称としてよく使う
coins <- 10 #各試行でcoinの数
res <- matrix(0,nrow=niter,ncol=coins) #各試行の結果(表裏)を格納する行列
num <- numeric(niter) #各試行の表の数を格納する配列

#niter回試行を実施する
for(i in 1:niter) {
  res[i,] <- rbinom(n=coins,size=1,prob=0.5) #表と裏が出る確率は0.5としている
  num[i] <- sum(res[i,])
  print(paste("i =",i,"is done"))
}

#結果
table(num) #表の枚数毎に,試行の回数が表示される

#結果を棒グラフで表示
barplot(table(num),ylab="",xlab="",ylim=c(0,50)) #頻度で表示
barplot(table(num)/niter,ylab="",xlab="") #確率で表示

###コインの枚数を1,000枚,試行回数を10,000回で実験###
niter <- 10000 #試行の回数
coins <- 1000 #各試行でcoinの数
res <- matrix(0,nrow=niter,ncol=coins) #各試行の結果(表裏)を格納する行列
num <- numeric(niter) #各試行の表の数を格納する配列

#niter回試行を実施する
for(i in 1:niter) {
  res[i,] <- rbinom(n=coins,size=1,prob=0.5) #表と裏が出る確率は0.5としている
  num[i] <- sum(res[i,])
  print(paste("i =",i,"is done"))
}

#結果
table(num) #表の枚数毎に,試行の回数が表示される

#結果を棒グラフで表示
barplot(table(num),ylab="",xlab="",ylim=c(0,50)) #頻度で表示
barplot(table(num)/niter,ylab="",xlab="") #確率で表示

#####1.2 中心極限定理の成立確認#####
####1.2.1 母集団が正規分布に従っている場合####
pop <- rnorm(30000) #30000個の正規分布に従う母集団を作成
truehist(pop,prob=FALSE)
#母集団は正規分布に従うことを確認

#正規分布である母集団(pop)から,
#無作為に10個の標本を抽出することを10000回繰り返し,
#標本平均が正規分布に従うことを確認する
niter <- 10000 #試行の回数
nsamp <- 10 #標本数
samp <- matrix(0,nrow=niter,ncol=nsamp) #標本を格納する行列
res <- numeric(niter) #標本平均を格納する配列

for(i in 1:niter) {
  sampNO <- sample(length(pop),nsamp,replace=FALSE) #replace=TRUE:非復元一様分布からの抽出
  samp[i,] <- pop[sampNO]
  res[i] <- mean(samp[i,])
  print(paste("i =",i,"is done"))
}

truehist(res,prob=FALSE,xlab="標本平均",ylab="頻度",main="標本平均の分布")
abline(v=mean(pop),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(pop),y=300,labels="母平均",cex=2)
text(x=mean(pop),y=200,labels=round(mean(pop),2),cex=2)
#形状が正規分布であることに注目

####1.2.2 母集団が正規分布に従っていない場合####
dat <- read.csv("1_sample_data.csv") #サンプルの銀行顧客データ
head(dat) #headは上位6人のデータを表示する関数
dim(dat) #29344人のデータであるとわかる

truehist(dat$AGE,prob=FALSE,xlab="年齢",ylab="頻度")
#明らかに正規分布でないことがわかる
mean(dat$AGE) #母集団の平均値

###1.2.2.1 標本数が大きい(30以上)の場合###
#正規分布ではない母集団(dat$CRED)から,
#無作為に30人の標本を抽出することを10000回繰り返し,
#標本平均が正規分布に従うことを確認する
niter <- 10000 #試行の回数
nsamp <- 30 #標本数
samp <- matrix(0,nrow=niter,ncol=nsamp) #標本を格納する行列
res <- numeric(niter) #標本平均を格納する配列

for(i in 1:niter) {
  sampNO <- sample(nrow(dat),nsamp,replace=FALSE) #replace=TRUE:非復元一様分布からの抽出
  samp[i,] <- dat$AGE[sampNO]
  res[i] <- mean(samp[i,])
  print(paste("i =",i,"is done"))
}

truehist(res,prob=FALSE,xlab="標本の平均年齢",ylab="頻度",main="標本平均の分布")
abline(v=mean(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(dat$AGE),y=300,labels="母平均",cex=2)
text(x=mean(dat$AGE),y=200,labels=round(mean(dat$AGE),2),cex=2)
#形状が正規分布であることに注目

###1.2.2.2 標本数が小さい(30未満)の場合###
#正規分布ではない母集団(dat$AGE)から,
#無作為に3人の標本を抽出することを10000回繰り返し,
#標本平均が正規分布に従うことを確認する
niter <- 10000 #試行の回数
nsamp <- 5 #標本数
samp <- matrix(0,nrow=niter,ncol=nsamp) #標本を格納する行列
res <- numeric(niter) #標本平均を格納する配列

for(i in 1:niter) {
  sampNO <- sample(nrow(dat),nsamp,replace=FALSE) #replace=TRUE:非復元一様分布からの抽出
  samp[i,] <- dat$AGE[sampNO]
  res[i] <- mean(samp[i,])
  print(paste("i =",i,"is done"))
}

truehist(res,prob=FALSE,xlab="標本の平均年齢",ylab="頻度",main="標本平均の分布")
abline(v=mean(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(dat$AGE),y=300,labels="母平均",cex=2)
text(x=mean(dat$AGE),y=200,labels=round(mean(dat$AGE),2),cex=2)
#形状が正規分布であることに注目(厳密には異なる)

###失敗例###
data <- c(4000,6500,10000,7500,20000,3000,25000,1500,8000,4000,
          5000,5000,15000,4500,1500,3000,30000,6000,1500)
truehist(data,prob=FALSE,breaks=seq(0,30000,by=2000))
#明らかに正規分布ではない
mean(data) #母集団の平均

#正規分布ではない母集団(data)から,
#無作為に10人の標本を抽出することを10000回繰り返し,
#標本平均が正規分布に従うかどうかを確認する
niter <- 10000 #試行の回数
nsamp <- 5 #標本数
samp <- matrix(0,nrow=niter,ncol=nsamp) #標本を格納する行列
res <- numeric(niter) #標本平均を格納する配列

for(i in 1:niter) {
  sampNO <- sample(length(data),nsamp,replace=FALSE) #replace=TRUE:非復元一様分布からの抽出
  samp[i,] <- data[sampNO]
  res[i] <- mean(samp[i,])
  print(paste("i =",i,"is done"))
}

truehist(res,prob=FALSE,xlab="標本平均",ylab="頻度",main="標本平均の分布")
abline(v=mean(data),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(data),y=300,labels="母平均",cex=2)
text(x=mean(data),y=200,labels=round(mean(data),2),cex=2)
#正規分布と形状が異なることに注目
#このように,標本数が少ない(30未満が目安)場合,
#標本平均は必ずしも正規分布に従うわけではない。

#####1.3 大数の法則の確認#####
head(dat) #架空の銀行の顧客データを再び使用する
dim(dat) #29344人のデータ
mean(dat$AGE) #母平均

#母集団(dat$AGE)から,標本を抽出する。
#その際,標本を5,10,30,100と変え,
#各々の標本平均の分布を500回の試行によって求め,
#母平均に対する位置関係を確認する
nsamp <- c(5,10,30,100) #標本数
niter <- length(nsamp) #試行回数
niter2 <- 500 #各標本数についての試行回数
samp <- NULL #標本を格納する配列
res <- NULL #標本平均を格納する配列

for(i in 1:niter) {
  samp[[i]] <- matrix(0,nrow=niter2,ncol=nsamp[i])
  res[[i]] <- numeric(niter2)
  for(j in 1:niter2) {
    sampNO <- sample(nrow(dat),nsamp[i],replace=FALSE) #replace=TRUE:非復元一様分布からの抽出
    samp[[i]][j,] <- dat$AGE[sampNO]
    res[[i]][j] <- mean(samp[[i]][j,])
  }
  print(paste("i =",i,"is done"))
}

par(mfrow=c(2,2))
#標本数5の場合の標本平均の分布
truehist(res[[1]],prob=FALSE,xlab="標本の平均年齢",ylab="頻度",
         main="標本平均の分布(標本数5)",xlim=c(20,80),ylim=c(0,150))
abline(v=mean(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(dat$AGE),y=100,labels="母平均",cex=1)
text(x=mean(dat$AGE),y=60,labels=round(mean(dat$AGE),2),cex=1)

#標本数10の場合の標本平均の分布
truehist(res[[2]],prob=FALSE,xlab="標本の平均年齢",ylab="頻度",
         main="標本平均の分布(標本数10)",xlim=c(20,80),ylim=c(0,150))
abline(v=mean(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(dat$AGE),y=100,labels="母平均",cex=1)
text(x=mean(dat$AGE),y=60,labels=round(mean(dat$AGE),2),cex=1)

#標本数30の場合の標本平均の分布
truehist(res[[3]],prob=FALSE,xlab="標本の平均年齢",ylab="頻度",
         main="標本平均の分布(標本数30)",xlim=c(20,80),ylim=c(0,150))
abline(v=mean(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(dat$AGE),y=100,labels="母平均",cex=1)
text(x=mean(dat$AGE),y=60,labels=round(mean(dat$AGE),2),cex=1)

#標本数100の場合の標本平均の分布
truehist(res[[4]],prob=FALSE,xlab="標本の平均年齢",ylab="頻度",
         main="標本平均の分布(標本数100)",xlim=c(20,80),ylim=c(0,150))
abline(v=mean(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
text(x=mean(dat$AGE),y=100,labels="母平均",cex=1)
text(x=mean(dat$AGE),y=60,labels=round(mean(dat$AGE),2),cex=1)
#標本数が増加するにつれて,標本平均の分布が母平均に収束していくことがわかる
par(mfrow=c(1,1))