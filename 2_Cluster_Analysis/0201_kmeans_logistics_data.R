######1.データの取り込み・準備######
library(cluster)
#kmeansの含まれるパッケージをダウンロード

dist <- read.csv("2_cluster_analysis.csv")
dist_sc <- scale(dist) #データの標準化
dist_sc <- data.frame(dist_sc) #標準化によりデータ型が変わったので修正
class(dist_sc) #データ型がdata.frameであるか確認

#欠損値の有無

######2.(とりあえず)クラスターを形成#####
set.seed(0) #安定的な結果を得るための乱数セット
km <- kmeans(dist_sc,centers=3,nstart=7)
km

head(dist_sc)

plot(dist, col=km$cluster)
points(km$centers,col=1:3,pch=8)

######3.適切なクラスター数を確認#####
wss <- numeric(15)

for (i in 1:length(wss)) {
  wss[i] <- sum(kmeans(dist_sc,centers=i)$withinss)
}

plot(1:15,wss,type="b",main="WSS graph",
     xlab="Number of Clusters",ylab="Within groups sum of squares")

######4.再度クラスタを形成#####
km <- kmeans(dist_sc,centers=4,nstart=7)
km

plot(dist, col=km$cluster)
points(km$centers,col=1:4,pch=8)
km$size

#縦軸m3/distribution,横軸distanceのグラフ
select <- c(1,3)
plot(dist[,select], col=km$cluster, main="DCからの距離(km)と1回あたりの配送量(m3)",
     xlab="DCからの距離(km)",ylab="1回あたりの配送量(m3)")
