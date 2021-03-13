#########1.データの取り込み,整形#########
sakura <- read.csv("http://r.livedocs.net/data/sakura.css", header=T) #webデータのダウンロード
sakura
#Year:年,Date:3/1から満開日まで数えた経過日数,T_0406:前年4-6月の平均気温,
#T_0709:前年4-6月の平均気温,T_1012:前年10-12月の平均気温,
#T_0103:開花直前の1-3月の平均気温

#欠損値の確認
sum(is.na(sakura))

#ヒストグラムで分布の確認
par(mfrow=c(5,1),mar=c(2,2,2,2),ps=10)
#marは余白幅の調整。順に底辺,左,上,右の順。(余白が無く若干文字が重なります)
for (i in 2:ncol(sakura)) {
  #ヒストグラムの数が増えると余白の調整が必要。Zoomしてみると良い
  hist(sakura[,i],main=names(sakura[i]))
}

#plotして全体像を把握
par(mfrow=c(5,1),mar=c(3,3,3,3))　#レイアウト調整
plot(sakura[,2:6])

#[Additional]k-meansでplotも可能
cl <- kmeans(scale(sakura[,2:6]), 3, 20) #クラスターの数はクラスター分析の演習参照
plot(sakura, col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8)

#相関係数の確認
cor(sakura[2:6])

#########2.満開日までの経過日数を目的変数として線形回帰#########
lm_sakura <- lm(Date~.,data=sakura)
summary(lm_sakura)
lm_sakura <- step(lm_sakura)　#細部の注意点は演習2で説明します
summary(lm_sakura)
attributes(lm_sakura)

#(参考)説明変数が2つであるので、空間にplotしてみる
#install.packages("rgl") ※初回のみ左記を実行
library(rgl)
len <- 40
s <- as.matrix(seq(6.5,10.5,length = len))
t <- as.matrix(seq(12.5,14.5,length = len))

plot3d(x=sakura$T_0103,y=sakura$T_1012,z=sakura$Date,type="s",pch =80,col="red")

for (i in 1:len) {
  plot3d(x=s[i],y=t,z=-3.053*s[i]+4.736*t-10.292,type="s",pch =80,col="blue",add=TRUE)
}

#3Dグラフについては、http://www.kkaneko.com/rinkou/r/rscatterplot3d.html,
#http://help.scilab.org/docs/5.3.0/ja_JP/plot3d.html等に詳しく記載がある
