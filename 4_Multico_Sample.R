#多重共線性　実例
multi <- read.csv("4_Multico_Sample.csv")
head(multi)

multi.lm <- lm(NumOfCar~.,data=multi)
summary(multi.lm)
par(mfrow=c(2,2)) #グラフレイアウト調整
plot(multi.lm)

#install.packages("DAAG") #初回のみ実行
library(DAAG)
vif(multi.lm)
cor(multi) #NumOfCarと最も相関係数が大きいRetailerで回帰分析

multi.lm2 <- lm(NumOfCar~Retailer,data=multi)
summary(multi.lm2)

#多重共線性のグラフ例
library(rgl)
s <- as.matrix(seq(0,20,length=20))
t <- as.matrix(seq(0,20,length=20))

u <- rep(0,10)

for (i in 1:10) {
  u[i] <- rep(rnorm(1,5,3),1)
}

plot3d(x=0,y=0,z=0,type="s",pch =80,col="red",xlim=c(0,20),ylim=c(0,20),
       zlim=c(0,20),xlab="x",ylab="y",zlab="z")
plot3d(x=s,y=t,z=u,type="s",pch =80,col="blue",add=TRUE)