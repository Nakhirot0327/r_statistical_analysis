#########演習1.講義で紹介したサンプルデータ#########
Titanic

#tableデータを個票データに整形
TN1 <- data.frame(Titanic)
TN2 <- data.frame(lapply(TN1, function(i) rep(i, TN1[,5])))
TN3 <- TN2[,-5]

#データを確認
head(TN3)
class(TN3) #data.frameでなければrpartが使用出来ないことに注意

#Titanicデータで決定木を作成
library("rpart")
library("rpart.plot")

TN.tree <- rpart(Survived~.,data=TN3,
            control=rpart.control(minsplit=40,cp=0.00000001),method="class",
            parms=list(split="information")) #splitのデフォルトはgini
par(ps=40) #フォント調整
rpart.plot(TN.tree,type=4,extra=1)
