#setwd("各自の作業フォルダ") #各自の作業ディレクトリを設定して下さい
library(MASS)

####2.1 不偏分散の一致性と不偏性の確認####
dat <- read.csv("1_sample_data.csv") #架空の銀行顧客データ
head(dat)
dim(dat) #29344人のデータであるとわかる
truehist(dat$AGE,prob=FALSE,xlab="年齢",ylab="頻度")

variance <- function(x) var(x)*(length(x)-1)/length(x) #母分散を求める関数
variance(dat$AGE) #母分散

#母集団(dat$AGE)から,標本を抽出する。
#その際,標本を5,10,30,100と変え,
#各々の不偏分散の分布を500回の試行によって求め,
#母分散に対する位置関係を確認する
nsamp <- c(5,10,30,100) #標本数
niter <- length(nsamp) #試行回数
niter2 <- 500 #各標本数についての試行回数
samp <- NULL #標本を格納する配列
res <- NULL #不偏分散を格納する配列

for(i in 1:niter) {
  samp[[i]] <- matrix(0,nrow=niter2,ncol=nsamp[i])
  res[[i]] <- numeric(niter2)
  for(j in 1:niter2) {
    sampNO <- sample(nrow(dat),nsamp[i],replace=FALSE) #replace=TRUE:非復元一様分布からの抽出
    samp[[i]][j,] <- dat$AGE[sampNO]
    res[[i]][j] <- var(samp[[i]][j,])
  }
  print(paste("i =",i,"is done"))
}

par(mfrow=c(2,2))
#標本数5の場合の不偏分散の分布
truehist(res[[1]],prob=FALSE,xlab="年齢の不偏分散",ylab="頻度",
         main="不偏分散の分布(標本数5)",ylim=c(0,150),xlim=c(0,1000))
abline(v=variance(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[1]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母分散,青線:不偏分散の平均値

#標本数10の場合の不偏分散の分布
truehist(res[[2]],prob=FALSE,xlab="年齢の不偏分散",ylab="頻度",
         main="不偏分散の分布(標本数10)",ylim=c(0,150),xlim=c(0,1000))
abline(v=variance(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[2]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母分散,青線:不偏分散の平均値

#標本数30の場合の不偏分散の分布
truehist(res[[3]],prob=FALSE,xlab="年齢の不偏分散",ylab="頻度",
         main="不偏分散の分布(標本数30)",ylim=c(0,150),xlim=c(0,1000))
abline(v=variance(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[3]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母分散,青線:不偏分散の平均値

#標本数100の場合の不偏分散の分布
truehist(res[[4]],prob=FALSE,xlab="年齢の不偏分散",ylab="頻度",
         main="不偏分散の分布(標本数100)",ylim=c(0,150),xlim=c(0,1000))
abline(v=variance(dat$AGE),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[4]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母分散,青線:不偏分散の平均値
#不偏分散の一致性と不偏性について何が言えるか,考えてみましょう
par(mfrow=c(1,1))

####2.2 中央値の性質の確認####
set.seed(1)
data <- rchisq(n=30000,df=4) #サンプルデータを作成
data <- c(data,rep(100,300),rep(50,1000),rep(75,600))

truehist(data,prob=FALSE,xlab="",ylab="頻度")
abline(v=median(data),col="#ff00ff80",lty=1,lwd=3)
text(x=median(data),y=5000,labels="母中央値",cex=1)
text(x=median(data),y=4600,labels=round(median(data),2),cex=1)

mean(data) #中央値と2倍以上異なる
#このように,一部外れ値を伴う場合は,平均値を使うよりも中央値を
#使う方が全体の情報の要約として適している場合がある
#以下,標本中央値と母平均値の関係について確認する。

#母集団(data)から,標本を抽出する。
#その際,標本を5,10,30,100と変え,
#各々の中央値の分布を500回の試行によって求め,
#母中央値に対する位置関係を確認する
nsamp <- c(5,10,30,100) #標本数
niter <- length(nsamp) #試行回数
niter2 <- 500 #各標本数についての試行回数
samp <- NULL #標本を格納する配列
res <- NULL #標本中央値を格納する配列

for(i in 1:niter) {
  samp[[i]] <- matrix(0,nrow=niter2,ncol=nsamp[i])
  res[[i]] <- numeric(niter2)
  for(j in 1:niter2) {
    sampNO <- sample(length(data),nsamp[i],replace=FALSE) #replace=TRUE:非復元一様分布からの抽出
    samp[[i]][j,] <- data[sampNO]
    res[[i]][j] <- median(samp[[i]][j,])
  }
  print(paste("i =",i,"is done"))
}

par(mfrow=c(2,2))
#標本数5の場合の標本中央値の分布
truehist(res[[1]],prob=FALSE,xlab="標本中央値",ylab="頻度",
         main="標本中央値の分布(標本数5)",xlim=c(1,10),ylim=c(0,200))
abline(v=median(data),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[1]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母中央値,青線:標本中央値の平均値

#標本数10の場合の標本中央値の分布
truehist(res[[2]],prob=FALSE,xlab="標本中央値",ylab="頻度",
         main="標本中央値の分布(標本数10)",xlim=c(1,10),ylim=c(0,200))
abline(v=median(data),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[2]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母中央値,青線:標本中央値の平均値

#標本数30の場合の標本中央値の分布
truehist(res[[3]],prob=FALSE,xlab="標本中央値",ylab="頻度",
         main="標本中央値の分布(標本数30)",xlim=c(1,10),ylim=c(0,200))
abline(v=median(data),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[3]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母中央値,青線:標本中央値の平均値

#標本数100の場合の標本中央値の分布
truehist(res[[4]],prob=FALSE,xlab="標本中央値",ylab="頻度",
         main="標本中央値の分布(標本数100)",xlim=c(1,10),ylim=c(0,200))
abline(v=median(data),col="#ff00ff80",lty=1,lwd=3)
abline(v=mean(res[[4]]),col="#0000ff80",lty=1,lwd=3)
#赤線:母中央値,青線:標本中央値の平均値
#中央値の一致性と不偏性について何が言えるか,考えてみましょう
par(mfrow=c(1,1))