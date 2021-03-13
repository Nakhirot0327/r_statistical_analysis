############################################
###2.2.学生の活動に関する記事の分類(実行)###
############################################
#R-3.0.2で実行すること
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/"
setwd(current_directory)

setwd("./2_Text_mining/")
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("igraph")
library(igraph)

#####1.データの入力#####
DM.new <- read.csv("2.2_DM.csv") #tf*idfと正規化によるターム文書行列
data <- t(DM.new[,4:ncol(DM.new)]) #t()は転置行列
rownames(data) <- colnames(DM.new)[4:ncol(DM.new)]
colnames(data) <- DM.new[,1]
#dataは縦軸が文書名,横軸がタームの行列

#####2.教師無し学習#####
####2.1 非階層クラスター分析####
###クラスター分析
##適切なクラスター数を確認
set.seed(3) #安定的な結果を得るための乱数セット
wss <- numeric(15)
for (i in 1:length(wss)) {
  wss[i] <- sum(kmeans(data,centers=i)$withinss)
}
plot(1:15,wss,type="b",main="WSS graph",
     xlab="Number of Clusters",ylab="Within groups sum of squares")
#群内平方和がクラスタ数の増加に対して一定の減少を示すので
#適切なクラスタ数が設定できない。
#そこで,クラスタ数を1:15とし,分類度合の良さから判断することにする
max.center.num <- 15
DM.km <- NULL #クラスタ分析の結果の格納先
data.cl <- NULL #決定木用のデータ
fit <- NULL #決定木の結果の格納場所
fit2 <- NULL #決定木の結果の格納場所
xerror <- NULL #x-val xerrorの格納場所
setwd("./2.2_Result/2.2.1.1_decision_tree/")

for(i in 2:max.center.num) {
  ##クラスター分析
  DM.km[[i]] <- kmeans(data,centers=i)
  data.cl[[i]] <- cbind(data,DM.km[[i]]$cluster)
  colnames(data.cl[[i]])[ncol(data.cl[[i]])] <- "data.group"
  data.cl[[i]] <- as.data.frame(data.cl[[i]])
  data.cl[[i]][,ncol(data.cl[[i]])] <- as.factor(data.cl[[i]][,ncol(data.cl[[i]])])
  
  ##決定木分析
  fit[[i]] <- rpart(data.group~., data=data.cl[[i]],
                    control=rpart.control(minsplit=round(nrow(data)/20,0),cp=0.0001), #cpの値は任意で定めた
                    method="class",parms=list(split="information"))
  png(file=paste(i,"xerror.png",sep=""))
  plotcp(fit[[i]]) #xerrorの減少幅が小さくなる箇所に注目※
  dev.off()
  fit2[[i]] <- prune(fit[[i]],cp=fit[[i]]$cptable[which(fit[[i]]$cptable[,4]==min(fit[[i]]$cptable[,4]))[1],1])
  xerror[[i]] <- fit[[i]]$cptable[which(fit[[i]]$cptable[,4]==min(fit[[i]]$cptable[,4]))[1],4]
  png(file=paste(i,"rpart.png",sep=""),width=1000,height=500)
  rpart.plot(fit2[[i]],type=4,extra=1)
  dev.off()
  print(paste("i =",i,"/",max(max.center.num),"is done"))
}
setwd("../")
setwd("../")
#2.2.1.1_decision_treeフォルダ内のpngファイルをフォトビューアーで確認
#明快に分類が出来ており,かつ,x-val errorが小さい場合を採用する。
#今回の場合はクラスタ数は4が適当と考えられる

###重要度の高い語の共起関係の考察
#クラスタ数4の場合の決定木による分類結果から
#重要度の高い単語を抽出する
key.words <- fit2[[4]]$variable.importance
plot(key.words,type="n",main="variable of importance",
     ylab="variable of importance")
text(1:length(key.words),key.words,names(key.words))

#本来は2.1_Dataフォルダにテキストファイルを配置し,下記を実行
#current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/2_Text_mining/2.1_Data/"
#setwd(current_directory)
#Ngram <- NULL
#files <- list.files(current_directory)
#for(i in 1:length(files)) {
#  Ngram <- rbind(Ngram,NgramDF(files[i],type=1,N=2,pos=c("名詞","形容詞","動詞")))
#  print(i)
#}
#setwd("../")

#今回はNgramのリストを読み込むことから開始
Ngram <- read.csv("2.2_Ngram.csv")
head(Ngram)
dim(Ngram)
Ngram <- Ngram[(Ngram$Ngram1 %in% names(key.words))
               |(Ngram$Ngram2 %in% names(key.words)),]

setwd("./2.2_Result/2.2.1.2_graph/")

for(i in 1:length(key.words)) {
  Ngram.sub <- Ngram[(Ngram$Ngram1 %in% names(key.words)[i])
                     |(Ngram$Ngram2 %in% names(key.words)[i]),]
  Ngram.sub.g <- graph.data.frame(Ngram.sub)
  E(Ngram.sub.g)$weight <- Ngram.sub[,3]
  png(file=paste("graph.keywords",i,".png",sep=""))
  op <- par(no.readonly=TRUE) # 現在のグラフィックスパラメータ値をopに退避する
  par(mar=c(0.5,0,0.5,0),xaxs="i",yaxs="i") #データ範囲内できれいに表示できる軸を見つける
  plot(Ngram.sub.g,vertex.label=V(Ngram.sub.g)$name,vertex.size=1,
       layout=layout.fruchterman.reingold,edge.label=E(Ngram.sub.g)$weight,
       edge.arrow.size=0.5,edge.label.family="mono",
       edge.label.color="black",vertex.label.family="mono",
       vertex.label.font=2,vertex.label.color="black")
  par(op) #グラフィックスパラメータを元に戻す
  dev.off()
  print(paste("i =",i,"/",length(key.words),"is done"))
}
setwd("../")
setwd("../")
#2.2.1.2_graphフォルダ内のpngファイルをフォトビューアーで確認
#重要な語がどのような文脈で用いられているか確認し,
#3つのグループにそれぞれ名前を付ける
rpart.plot(fit2[[4]],type=4,extra=1)

####2.2 LSI(Latent Semantic Indexing,潜在的意味インデキシング)####
DM <- t(data)
dim(DM) #5785x86
DM.svd <- svd(DM) #特異値分解
str(DM.svd) #行列U,Vと特異値dが出力されているとわかる
DMx <- t(DM.svd$u)%*%DM #次元を5785次元から86次元へ縮約

###近似の内容を確認する
info <- rep(0,length(DM.svd$d))
for(i in 1:(length(DM.svd$d))) {
  info[i] <- 100*sum((DM.svd$d[1:i])^2)/sum((DM.svd$d)^2)
}
plot(info,type="l",ylab="情報量(%)")
#最初の軸のみ,5.4%の情報を表す
#(増加の仕方が一定であることから)他の軸はどれも同等の情報を表す

for(i in 1:10) {
  abline(v=which(info>i*10)[1],col=4,lty=2)
  abline(h=info[which(info>i*10)[1]],col=4,lty=2)
  text(x=which(info>i*10)[1],y=4,which(info>i*10)[1])
}
#第63軸までで80%の情報を表す
#今回は,86名を最大でも10グループ程度に分けたいので,
#第10軸までを分析の対象とする

###第1軸-第10軸の名前を検討する
#各軸に大きく影響を与えている言葉を調べ,各軸に名前を付ける
#今回はトップ10の語を調べる
setwd("./2.2_Result/2.2.2.1_axis_name/")

axis.num <- 10
for(i in 1:axis.num) {
  png(file=paste("axis",i,".png",sep=""))
  plot(DM.svd$u[order(DM.svd$u[,i])[1:10],i],type="n",
       main=paste("axis",i,sep=""))
  text(1:10,DM.svd$u[order(DM.svd$u[,i])[1:10],i],
       labels=rownames(DM)[order(DM.svd$u[,i])][1:10])
  dev.off()
}
setwd("../")
setwd("../")
#2.2.2.1_axis_nameフォルダ内のpngファイルをフォトビューアーで確認

#軸に名前を付ける
axis.name <- c("スポ.新興国","写真","社会起業","音楽","ファッション起業",
               "料理","IT起業","地元起業","音楽.新興国","アニメ")
#グラフ上に掲載するため,以下のように省略している
#"スポ.新興国":新興国でスポーツ
#"音楽.新興国":新興国で音楽

###クラスター分析
DM10 <- t(DM.svd$u[,1:axis.num])%*%DM #次元を5785次元から10次元へ縮約
DM10 <- t(DM10)
set.seed(3) #安定的な結果を得るための乱数セット

##適切なクラスター数を確認
wss.lsi <- numeric(15)
for (i in 1:length(wss)) {
  wss.lsi[i] <- sum(kmeans(DM10,centers=i)$withinss)
}

plot(1:15,wss.lsi,type="b",main="WSS graph",
     xlab="Number of Clusters",ylab="Within groups sum of squares")
#クラスター数7が適切と思われる
center.num.lsi <- 7

##再度クラスタを形成
DM.km.lsi <- kmeans(DM10,centers=center.num.lsi)

##各クラスタの中心点の成分を確認
setwd("./2.2_Result/2.2.2.2_cluster_name/")

for(i in 1:center.num.lsi) {
  png(file=paste("cluster",i,".png",sep=""),width=1000)
  barplot(height=DM.km.lsi$centers[i,],names.arg=axis.name,
          cex.names=1,main=paste("Component of cluster",i))
  dev.off()
}
setwd("../")
setwd("../")
#2.2.2.2_cluster_nameフォルダ内のpngファイルをフォトビューアーで確認
