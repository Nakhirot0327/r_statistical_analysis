#########1.データの取り込み,整形#########
library(kernlab)
data(spam)
#メールに含まれる単語の頻度と大文字の数などをまとめたデータ
#3_Decision Tree2.R と同じ

#データ型を確認
for (i in 1:ncol(spam)) {
  print(c(names(spam[i]),class(spam[,i])))
}

#欠損値の有無
sum(is.na(spam))

#相関行列確認
spam_cor <- cor(spam[,1:57])

#install.packages("corrplot")
library(corrplot) #パッケージの呼び出し
par(mar=c(6,3,3,3),mfrow=c(1,1),ps=10) #余白幅の調整。順に底辺,左,上,右の順。
corrplot(spam_cor) #中央(labs~direct)あたりに相関が強い変数が集積している
par(mfrow=c(1,1),mar=c(3,3,3,3))　#レイアウト調整

#相関が一定値を超えるような変数を抽出する関数を定義
cor.check <- function(X,q) { #X:データ,q:閾値
  val <- NULL
  COR <- cor(X,use="na.or.complete")
  k = 1
  for (i in 1:nrow(COR)) {
    for (j in 1:ncol(COR)) {
      if (abs(COR[i,j]) >= q && i > j)　{
        #オブジェクトを格納する場合に[[]]を使用する
        val <- rbind(val,c(i,colnames(X)[i],j,colnames(X)[j],COR[i,j],abs(COR[i,j])))
        k = k + 1
      }
    }
  }
  val
}

cor.result <- cor.check(spam[,1:57],0.9)
#相関が0.9を超える変数の組を抽出
#0.9の目安については「マーケティングのための多変量解析」p.106による
cor.result

#相関が0.9を超える変数のうち片方をそれぞれ削除
delete <- rep(0,nrow(cor.result))
for (i in 1:nrow(cor.result)) {
  delete[i] <- cor.result[i,3]
}
delete <- unique(as.numeric(delete))
spam_rv <- spam[,-delete]
#厳密には目的変数との相関関係が低い方の説明変数を削除すべきだが、今回は割愛

#標準化
spam_rv[,1:(length(spam_rv)-1)] <- scale(spam_rv[,1:(length(spam_rv)-1)])
spam_s <- spam_rv

#########2.Test data, Train dataの生成#########
set.seed(2) #一定の結果を得るための乱数セット
trainNO <- sample(nrow(spam_s),nrow(spam_s)*0.8,replace=FALSE)　#replace=TRUE:非復元一様分布からの抽出
train_dat <- spam_s[trainNO,]
test_dat <- spam_s[-trainNO,]

#まずはそのまま実行
names(train_dat)
#family=binomialは目的変数が0,1の場合に使用
train.glm <- glm(type~.,data=train_dat,family=binomial)
result <- summary(train.glm)
train.glm
attributes(train.glm)
attributes(result)
result$iter

#AICに基づいた変数選択法(デフォルトはbackward)
train.glm1 <- step(train.glm)
summary(train.glm1)

#########3.モデルの評価#########
#(a)残差分析
par(mar=c(4,4,4,4),mfrow=c(2,2),ps=20)
plot(train.glm1)

#外れ値である804,4460,2537,2015,3379を除いて再度ロジスティック回帰分析
delete2 <- c(804,4460,2537,2015,3379)
spam_rv2 <- spam_s[-delete2,]
trainNO2 <- sample(nrow(spam_rv2),nrow(spam_rv2)*0.8,replace=FALSE)　#replace=TRUE:非復元一様分布からの抽出
train_dat2 <- spam_rv2[trainNO2,]
test_dat2 <- spam_rv2[-trainNO2,]

train.glm2 <- step(glm(type~.,data=train_dat2,family=binomial))

#多重共線性が無いか調べる
library(DAAG)
vif(train.glm2)
#VIFが10を超える説明変数が無いので,多重共線性の問題なし

#(b)オッズ比の評価
par(mfrow=c(1,1),ps=15)
summary(train.glm2)$coefficients

#どの要素がより影響しているか確認。オッズ比を説明変数毎に算出
odds <- sort(exp(train.glm2$coefficients),decreasing=TRUE)
plot(odds,type="n")
text(1:length(odds),odds,names(odds))

#オッズ比の95%信頼区間 ※とりあえずオッズ比10位10変数。z値, 標準偏差も付記
select <- c("num3d","capitalLong","capitalAve","charDollar","remove","charHash","free","num000","credit","num650")
oddslist <-cbind(exp(confint(train.glm2))[select,1],
                 exp(summary(train.glm2)$coefficients[select,1]),
                 exp(confint(train.glm2))[select,2],
                 exp(summary(train.glm2)$coefficients[select,2]),
                 summary(train.glm2)$coefficients[select,3:4])
colnames(oddslist) <- c("2.5%","exp(coef)","97.5%","exp(std.Er)","z val","pr(>|z|)")
oddslist

#(c)予測のヒストグラム
#まず、test_datをSurvivedがYes/Noのグループに分割
test_datNo <- subset(test_dat2, type=="nonspam")
test_datYes <- subset(test_dat2, type=="spam")

#それぞれにモデルを適用し、spamである確率を予測
test_datNo$P <- predict(train.glm2,newdata=test_datNo,type="response") #type="prob"でtypeのspam/No spamの確率を予測
test_datYes$P <- predict(train.glm2,newdata=test_datYes,type="response")

#ヒストグラムを描く
par(mfrow=c(1,1),ps=20)
head(test_datYes$P) #データの形の確認
hist(test_datYes$P, col = "#ff00ff40", border = "#ff00ff", breaks = 10, xlab="spamの確率", ylim=c(0,500), main="Validate dataのspamの確率")
hist(test_datNo$P, col = "#0000ff40", border = "#0000ff", breaks = 10, add = TRUE, ylim=c(0,500))
legend("topright",legend=c("Spam","Nonspam"),fill=c("#ff00ff40","#0000ff40")) # #ff00ff40,#0000ff40は半透明色

#(d)ROC
#Vali_datの予測結果を踏まえ、ROC curveを描く
library(ROCR) #ROCカーブを描くためのパッケージ
par(mfrow=c(1,1)) #グラフレイアウト調整
predicted.p <- predict(train.glm2,newdata=test_dat2,type="response")

predObj <- prediction(predicted.p,test_dat2$type)
rocObj <- performance(predObj,measure="tpr",x.measure="fpr")
aucObj <- performance(predObj,measure="auc")

auc <- aucObj@y.values[[1]]
auc

plot(rocObj,main ="ROC curve", col="red")
par(new=T)
y <- function(x) x
plot(y,0,1,xlab="",ylab="")
legend("bottomright",legend=c("Logistic regression"),col=c("red"),lty=c(1))

#(e)R-squared
R2 <- 1-with(train.glm2,deviance/null.deviance)
R2
