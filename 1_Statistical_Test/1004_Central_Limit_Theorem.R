######1.中心極限定理と大数の法則の確認######
library(MASS) #for truehist()

#手順1:一様乱数の生成
data <- c(4000,6500,10000,7500,20000,3000,25000,1500,8000,4000,
          5000,5000,15000,4500,1500,3000,30000,6000,1500)
truehist(data,prob=FALSE) #観測データの分布
n <- 10^5
x <- runif(n) #一様乱数の生成

#手順2:対象のデータの密度分布を求める
d <- density(data,cut=0)

#手順3:観測データのクオンタイルからシュミレーションデータを生成
simx <- quantile(data,x)
truehist(simx,prob=FALSE) #生成したデータの分布

#確率密度の比較
sim.d <- density(simx,cut=0)
maxy <- max(d$y,sim.d$y)
plot(d,type="l",ylim=c(0,maxy),xlab="price",ylab="density",main=paste("確率密度の比較"))
lines(sim.d,type="l",col=2)
legend("topright", legend=c("observed", "simulated"), lty=1, col=1:2)

data
samp <- matrix(0,nrow=100000,ncol=17)
avg <- numeric(100000)

for(i in 1:100000) {
  samp[i,] <- sample(x=data,size=17)
  avg[i] <- mean(samp[i,])
}

truehist(avg,prob=TRUE)
