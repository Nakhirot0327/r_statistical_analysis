######1.t検定の練習######
#setwd("/Users/Hirotoshi/Documents/100_OTL/110_DSL/140524_DSL_Seminer/")
#各自所定のディレクトリを変更

####1.1 データの入力と図示####
dat <- read.csv("1_data.csv",sep=",")
head(dat)
#京都の大学生(143人)に対するアンケート調査
#ID:個人の識別番号,factor型
#Q1:性別(1:男性/2:女性),factor型
#Q2:アルバイトをしているか(1:はい/2:いいえ),factor型
#Q3:自宅か下宿か(1:自宅/2:下宿),factor型
#Q4:1ヶ月の支出(万円),Q5:1ヶ月の収入(万円),numeric型
#Q6:ペットを飼っているか(1:はい/2:いいえ),numeric型
#Q7:ベッドで寝るか(1:はい/2:いいえ),factor型
#Q8:習い事をしているか(1:はい/2:いいえ),factor型
#Q9:よく夢を見るか(1:はい/2:いいえ),factor型
#Q10:どの季節が一番好きか(1:春/2:夏/3:秋/4:冬),factor型
#Q11:本を月何冊読むか(1:3冊以上/2:3冊未満),factor型
#Q12:月の携帯電話代(1:6千円以上/2:6千円未満),factor型
#Q13:現在の自分の評価(1:A/2:B/3:C/4:D)(Aの方が良い),factor型
#Q14:10年前の自分の評価(1:A/2:B/3:C/4:D)(Aの方が良い),factor型

#データの型の確認
for(i in 1:ncol(dat)) {
  print(class(dat[,i]))
}

#適切な型に変更
dat[,1] <- as.factor(dat[,1])
dat[,2] <- as.factor(dat[,2])
dat[,3] <- as.factor(dat[,3])
dat[,4] <- as.factor(dat[,4])
dat[,5] <- as.numeric(dat[,5])
dat[,6] <- as.numeric(dat[,6])
dat[,7] <- as.factor(dat[,7])
dat[,8] <- as.factor(dat[,8])
dat[,9] <- as.factor(dat[,9])
dat[,10] <- as.factor(dat[,10])
dat[,11] <- as.factor(dat[,11])
dat[,12] <- as.factor(dat[,12])
dat[,13] <- as.factor(dat[,13])
dat[,14] <- as.factor(dat[,14])
dat[,15] <- as.factor(dat[,15])

#列名の変更
colnames(dat) <- c("ID","sex","job","home","consumption",
                   "income","pet","bed","lesson","dream",
                   "season","numbooks","mobile","evnow","ev10")
head(dat) #列の名前が変わった
nam <- colnames(dat)[2:length(colnames(dat))]

#mosaicplotによる図示
for(i in nam) {
  for(j in nam) {
    if(i==j) {next}
    mosaicplot(table(dat[,i],dat[,j]),xlab=i,ylab=j,main="mosaicplot of dat")
    Sys.sleep(0.8)
  }
}

####1.2 対応のないt検定####
#2カテゴリに分けれられている以下の項目について,収入差が生じるか8通りの
#対応のないt検定を行い,p値が0.05を下回る場合を検出
#Q1:性別(1:男性/2:女性),factor型
#Q2:アルバイトをしているか(1:はい/2:いいえ),factor型
#Q3:自宅か下宿か(1:自宅/2:下宿),factor型
#Q7:ベッドで寝るか(1:はい/2:いいえ),factor型
#Q8:習い事をしているか(1:はい/2:いいえ),factor型
#Q9:よく夢を見るか(1:はい/2:いいえ),factor型
#Q11:本を月何冊読むか(1:3冊以上/2:3冊未満),factor型
#Q12:月の携帯電話代(1:6千円以上/2:6千円未満),factor型

testnum <- c(2,3,4,8,9,10,12,13)
result <- NULL
k <- 1
p.value <- rep(0,length(testnum))
names(p.value) <- colnames(dat)[testnum]

for(i in testnum) {
  x <- dat[dat[,i]==1,6]
  y <- dat[dat[,i]==2,6]
  #どちらが大きいのか不明なので両側検定(alternative="two.sided")
  #対応のないt検定なので,paried=FALSE(デフォルトなので記入不要)
  #今回は等分散性は成り立つものとする(var.equal=TRUE(デフォルト))
  #信頼水準は95%とする(conf.level=0.96(デフォルト))
  print(result[[k]] <- t.test(x=x,y=y,alternative="two.sided"))
  p.value[k] <- result[[k]]$p.value
  k <- k + 1
}

barplot(p.value,main="p.value of unparied t tests to income")
abline(h=0.05,col=2,lty=2)
text(x=2.5,y=0.07,paste("p.value =",0.05),col=2)
#アルバイトをしているか否か,自宅通いか否かで収入に有意な差が生じている
#ことがわかる

#ヒストグラムで有意差を確かめる(job)
x <- dat[dat[,"job"]==1,6]
y <- dat[dat[,"job"]==2,6]
par(mfrow=c(2,1))
bins <- seq(0,max(dat[,6]),by=1)
hist(x,main=paste("アルバイトをしている人の月の収入"),xlim=c(0,max(dat[,6])),
     ylim=c(0,30),breaks=bins)
hist(y,main=paste("アルバイトをしていない人の月の収入"),xlim=c(0,max(dat[,6])),
     ylim=c(0,30),breaks=bins)
par(mfrow=c(1,1))

#ヒストグラムで有意差を確かめる(home)
x <- dat[dat[,"home"]==1,6]
y <- dat[dat[,"home"]==2,6]
par(mfrow=c(2,1))
bins <- seq(0,max(dat[,6]),by=1)
hist(x,main=paste("自宅住まいの人の月の収入"),xlim=c(0,max(dat[,6])),
     ylim=c(0,30),breaks=bins)
hist(y,main=paste("下宿住まいの人の月の収入"),xlim=c(0,max(dat[,6])),
     ylim=c(0,30),breaks=bins)
par(mfrow=c(1,1))

#注意:厳密な解釈を行うには多重性の考慮,検出力分析が必要
#今回はこれらは省略
