#########1.データの取り込み,整形#########
getwd()
setwd("C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/0519_Seminer Data")
raw <- read.csv("4_Linear Regression.csv")
head(raw)

#SPC:Sales per customer,顧客単価(JPY)
#OpenH:Hours open,営業時間
#NOHWT:Number of households within trade area,商圏内世帯数(世帯)
#NOPWT:Number of people with in trade area,商圏内人口(人)
#IPH:Income per households within trade area,商圏内世帯の平均世帯年収(JPY)
#PPT:Profit per Tubo,坪当たり営業利益(JPY)
#線形回帰で、顧客単価を予測するモデルを作成することが目標

#ヒストグラム
par(mfrow=c(6,1),mar=c(0,3,3,0),ps=10)　#レイアウト調整
for (i in 1:ncol(raw)) {
  #ヒストグラムの数が増えると余白の調整が必要。Zoomしてみると良い
  hist(raw[,i],main=names(raw[i]))
}

#欠損値の確認
sum(is.na(raw))

#全て量的変数の場合はplotで傾向を把握
plot(raw) 

#k-meansでplotも可能。クラスター分析の演習参照
par(mfrow=c(1,1),mar=c(3,3,3,3))　#レイアウト調整
cl <- kmeans(scale(raw), 3, 20) #時間があればWSSのグラフも描いてみて下さい。(各自コーディング)
plot(raw, col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8)

#相関係数
cor(raw)

#########2.顧客単価を目的変数として線形回帰#########
lm_a <- lm(SPC~.,data=raw)
summary(lm_a)

#(a)説明変数の選択
lm_b <- step(lm_a) #AICに基づいた変数選択法(デフォルトはbackward)
summary(lm_b)

#(b)モデルの評価
#(b-1):残差分析
par(mfrow=c(2,2),mar=c(4,4,4,4),ps=15)
plot(lm_b) #残差分析。外れ値の行番号が表示される

#外れ値を外して再度回帰分析
raw_rv <- raw[-5,]
lm_c <- step(lm(SPC~.,data=raw_rv))

summary(lm_c)
plot(lm_c) #残差分析

#Residuals vs Fitted values:SPC:目的変数値に応じた残差の値を縦軸に表示。
#NormalQ-Q:観測値の標準化後残差が正規分布に従う場合の期待値をx軸、
#観測値の標準化後残差をy軸にとったプロット。
#Scale-Location:Residual vs Fitted valuesのy軸について平方根をとったもの。
#Residuals vs Leverage:Leverage(てこ比)とは回帰分析の観察点(サンプル)毎に
#説明変数のデータを変えずに目的変数yの値を1だけ変えたときの予測値の変化量
#cook's distanceは検証用に使われる距離

#stepの結果単回帰直線となったので、図示
par(mfrow=c(1,1),mar=c(5,5,5,5))
plot(raw[,1]~raw[,5],data=raw_rv,xlab="商圏内世帯の平均世帯年収(JPY)",
     ylab="顧客単価(JPY)",main="顧客単価の回帰分析",
     xlim=c(2800000,6500000),ylim=c(25000,55000))
abline(lm_c)
par(new=T)

#(b-2):95%信頼区間
lm_con <- predict(lm_c,interval="confidence")

#図示出来るように昇順並び替え
con_lwr <- data.frame(raw_rv[,5],lm_con[,2])
con_lwr <- con_lwr[order(con_lwr[,1]),]
con_upr <- data.frame(raw_rv[,5],lm_con[,3])
con_upr <- con_upr[order(con_upr[,1]),]

par(ps=20) #フォント調整
lines(con_lwr,type="l",xlim=c(2800000,6500000),ylim=c(25000,55000),ylab="",xlab="",col="blue",lty=1)
par(new=T)
lines(con_upr,type="l",xlim=c(2800000,6500000),ylim=c(25000,55000),ylab="",xlab="",col="blue",lty=1)
par(new=T)

#(b-3):95%予測区間
lm_prd <- predict(lm_c,interval="prediction")
lm_con <- predict(lm_c,interval="confidence")

prd_lwr <- data.frame(raw_rv[,5],lm_prd[,2])
prd_lwr <- prd_lwr[order(prd_lwr[,1]),]
prd_upr <- data.frame(raw_rv[,5],lm_prd[,3])
prd_upr <- prd_upr[order(prd_upr[,1]),]

lines(prd_lwr,type="l",xlim=c(2800000,6500000),ylim=c(25000,55000),ylab="",xlab="",col="red",lty=1)
par(new=T)
lines(prd_upr,type="l",xlim=c(2800000,6500000),ylim=c(25000,55000),ylab="",xlab="",col="red",lty=1)
par(new=T)

par(ps=10) #フォント調整
legend("topleft",legend=c("95%信頼区間","95%予測区間"),col=c("blue","red"),lty=c(1))

#########[Additional]3.ブートストラップ法の利用#########
intr <- numeric(10000)
coef <- numeric(10000)

for(i in 1:10000){
  trainNO <- sample(nrow(raw),nrow(raw)*0.67,replace=TRUE)　#replace=TRUE:非復元一様分布からの抽出
  train_dat <- raw[trainNO,]
  res <- lm(train_dat[,1]~train_dat[,5])  #train_datで回帰分析
  intr[i] <- res$coefficients[1]   # 定数項の取り出し
  coef[i] <- res$coefficients[2]   # 係数の取り出し
}

par(mar=c(3,3,3,3),mfrow=c(1, 2)) #余白幅の調整,グラフ画面の分割
hist(intr)                        #定数項のヒストグラム

#95%の信頼区間の境界値を書き加える
abline(v=quantile(intr, c(0.025, 0.975)), col="red")

hist(coef)                        #係数のヒストグラム
#95%の信頼区間の境界値を書く
abline(v=quantile(coef, c(0.025, 0.975)), col="red")

#########[Additional]4.主成分分析#########
raw_s <- scale(raw) #データの標準化
cor(raw_s)　#相関行列
PC <- princomp(raw_s)　#主成分分析
summary(PC)
attributes(PC)
PC$loadings

#第1,2主成分でプロット(69%を説明)
par(mfrow=c(1, 1))
par(ps=15,mar=c(5,3,3,3))
plot (data.matrix(raw_s) %*% unclass(loadings(PC))[,c(1,2)])
biplot(PC)
