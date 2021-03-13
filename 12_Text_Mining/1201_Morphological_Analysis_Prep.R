############################################
###2.1.学生の活動に関する記事の分類(準備)###
############################################
#R-3.0.2で実行すること
#Tools > Global-options > text encoding をSJISに変更
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/"
setwd(current_directory)

setwd("./2_Text_mining/")
#install.packages("RMeCab", repos = "http://rmecab.jp/R") #初回のみ
library(RMeCab)
#install.packages("wordcloud")
library(wordcloud)

#####1.データの入力(形態素解析と頻度集計)#####
#本来は2.1_Dataフォルダにテキストファイルを配置し,下記を実行
#DM.wc <- docDF("2.1_Data",type=1,N=1,pos=c("名詞","動詞","形容詞"))

#今回はターム・文書行列を読み込むことから開始
DM.wc <- read.csv(file="2.1_DM.wc.csv",header=TRUE,row.names=1)
head(DM.wc) #列名を確認

#データ型を確認
for(i in 1:ncol(DM.wc)) {
  print(class(DM.wc[,i]))
}

#1-3列目のデータ型をcharacter型に修正
DM.wc[,1] <- as.character(DM.wc[,1])
DM.wc[,2] <- as.character(DM.wc[,2])
DM.wc[,3] <- as.character(DM.wc[,3])

#####2.不要な語と重複している語の処理#####
####2.1 明らかに不要な記号等を削除する####
DM.wc[,1] #6497語が抽出されたことを確認
table(DM.wc[,2]) #形容詞,動詞,名詞が抽出されたことを確認
table(DM.wc[,3]) #不要な語を推測する
delete <- NULL #不要な語を格納するため

table(DM.wc[DM.wc[,3] %in% c("サ変接続"),1]) #最初の"!"等の感嘆詞は不要
names(table(DM.wc[DM.wc[,3] %in% c("サ変接続"),1])) #1番目から54番目は不要
delete <- names(table(DM.wc[DM.wc[,3] %in% c("サ変接続"),1]))[1:54]

table(DM.wc[DM.wc[,3] %in% c("ナイ形容詞語幹"),1]) #必要

table(DM.wc[DM.wc[,3] %in% c("数"),1]) #全て不要
delete <- c(delete,names(table(DM.wc[DM.wc[,3] %in% c("数"),1])))

table(DM.wc[DM.wc[,3] %in% c("接続詞的"),1]) #必要

table(DM.wc[DM.wc[,3] %in% c("接尾"),1]) #"くん","げ","さ","さん","す","そう","ちゃん","め","よう","ら"が不要(意味を左右しない語)
delete <- c(delete,"くん","げ","さ","さん","す","そう","ちゃん","め","よう","ら")

table(DM.wc[DM.wc[,3] %in% c("代名詞"),1]) #必要
table(DM.wc[DM.wc[,3] %in% c("動詞非自立的"),1]) #必要
table(DM.wc[DM.wc[,3] %in% c("特殊"),1]) #必要

table(DM.wc[DM.wc[,3] %in% c("非自立"),1]) #"の","る","ん","もん","ちゃう","く","じゃう","せい"が不要(意味を左右しない語)
delete <- c(delete,"の","る","ん","もん","ちゃう","く","じゃう","せい")

table(DM.wc[DM.wc[,3] %in% c("副詞可能"),1]) #"XX月","あと"は不要
names(table(DM.wc[DM.wc[,3] %in% c("副詞可能"),1])) #1番目-12番目が不要
delete <- c(delete,names(table(DM.wc[DM.wc[,3] %in% c("副詞可能"),1]))[1:12])

delete #不要語一覧
DM.wc <- DM.wc[!DM.wc[,1]%in%delete,] #不要語を削除
DM.wc[,1] #6313語

DM.wc <- DM.wc[!DM.wc[,1]%in%delete,] #不要語を削除

####2.2 品詞ごとに削除すべき語を吟味####
##形容詞
DM.wc.kei <- DM.wc[DM.wc[,2] == "形容詞",]
DM.wc.kei[,1]

#平仮名表記の語の漢字表記/送り仮名が異なる語/包含関係のある語/同義語を探す
#####[残作業]:形容詞をまとめる#####
merg.kei <- NULL
merg.kei[[1]] <- list(name="良い",num=c(which(DM.wc.kei[,1]=="いい"),which(DM.wc.kei[,1]=="よい"),
                                      which(DM.wc.kei[,1]=="よろしい"),which(DM.wc.kei[,1]=="イイ"),
                                      which(DM.wc.kei[,1]=="良い")))
merg.kei[[2]] <- list(name="かっこいい",num=c(which(DM.wc.kei[,1]=="かっこいい"),which(DM.wc.kei[,1]=="かっこよい"),
                                         which(DM.wc.kei[,1]=="カッコイイ")))
merg.kei[[3]] <- list(name="すごい",num=c(which(DM.wc.kei[,1]=="すごい"),which(DM.wc.kei[,1]=="すっごい")))
merg.kei[[4]] <- list(name="にくい",num=c(which(DM.wc.kei[,1]=="にくい")))
merg.kei[[5]] <- list(name="ほしい",num=c(which(DM.wc.kei[,1]=="ほしい")))
merg.kei[[6]] <- list(name="面倒な",num=c(which(DM.wc.kei[,1]=="めんどい"),which(DM.wc.kei[,1]=="めんどくさい"),
                                       which(DM.wc.kei[,1]=="面倒くさい")))
merg.kei[[7]] <- list(name="やりきれない",num=c(which(DM.wc.kei[,1]=="やりきれない"),which(DM.wc.kei[,1]=="やり切れない")))
merg.kei[[8]] <- list(name="安い",num=c(which(DM.wc.kei[,1]=="安い"),which(DM.wc.kei[,1]=="安っぽい")))
merg.kei[[9]] <- list(name="気持ちいい",num=c(which(DM.wc.kei[,1]=="気持ちいい"),which(DM.wc.kei[,1]=="気持ちよい")))
merg.kei[[10]] <- list(name="難しい",num=c(which(DM.wc.kei[,1]=="難い"),which(DM.wc.kei[,1]=="難しい")))
merg.kei[[11]] <- list(name="欲しい",num=c(which(DM.wc.kei[,1]=="欲しい")))
merg.kei[[12]] <- list(name="ありがたい",num=c(which(DM.wc.kei[,1]=="ありがたい"),which(DM.wc.kei[,1]=="有難い")))
merg.kei[[13]] <- list(name="おいしい",num=c(which(DM.wc.kei[,1]=="おいしい"),which(DM.wc.kei[,1]=="美味しい")))
merg.kei[[14]] <- list(name="面白い",num=c(which(DM.wc.kei[,1]=="おもしろい"),which(DM.wc.kei[,1]=="面白い")))
merg.kei[[15]] <- list(name="素晴らしい",num=c(which(DM.wc.kei[,1]=="すばらしい"),which(DM.wc.kei[,1]=="素晴らしい")))
merg.kei[[16]] <- list(name="小さい",num=c(which(DM.wc.kei[,1]=="ちいさい"),which(DM.wc.kei[,1]=="小さい")))
merg.kei[[17]] <- list(name="大きい",num=c(which(DM.wc.kei[,1]=="でかい"),which(DM.wc.kei[,1]=="大きい")))
merg.kei[[18]] <- list(name="馬鹿らしい",num=c(which(DM.wc.kei[,1]=="ばからしい"),which(DM.wc.kei[,1]=="馬鹿らしい")))
merg.kei[[19]] <- list(name="勿体ない",num=c(which(DM.wc.kei[,1]=="もったいない"),which(DM.wc.kei[,1]=="勿体ない")))

add.wc <- NULL
delete2.num <- NULL

for(i in 1:length(merg.kei)) {
  add.wc[[i]] <- c(merg.kei[[i]]$name,"形容詞","追加",rep(0,ncol(DM.wc.kei)-3))
  delete2.num <- c(delete2.num,merg.kei[[i]]$num)
  for(j in merg.kei[[i]]$num) {
    add.wc[[i]][4:length(add.wc[[i]])] <- as.numeric(add.wc[[i]][4:length(add.wc[[i]])]) + DM.wc.kei[j,4:length(add.wc[[i]])]
  }
  DM.wc.kei <- rbind(DM.wc.kei,add.wc[[i]])
}

delete2.num <- sort(delete2.num)
delete2 <- DM.wc.kei[delete2.num,1]

#重複語を消す
DM.wc.kei <- DM.wc.kei[-delete2.num,]

##動詞
DM.wc.dou <- DM.wc[DM.wc[,2] == "動詞",]

merg.dou <- NULL
DM.wc.dou[,1]
#平仮名表記の語の漢字表記/送り仮名が異なる語/包含関係のある語/同義語を探す
#####[残作業]:動詞をまとめる#####
merg.dou[[1]] <- list(name="諦める",num=c(which(DM.wc.dou[,1]=="あきらめる"),which(DM.wc.dou[,1]=="諦める")))
merg.dou[[2]] <- list(name="与える",num=c(which(DM.wc.dou[,1]=="あたえる"),which(DM.wc.dou[,1]=="与える")))
merg.dou[[3]] <- list(name="言える",num=c(which(DM.wc.dou[,1]=="いえる"),which(DM.wc.dou[,1]=="言える")))
merg.dou[[4]] <- list(name="言う",num=c(which(DM.wc.dou[,1]=="いう"),which(DM.wc.dou[,1]=="言う")))
merg.dou[[5]] <- list(name="行く",num=c(which(DM.wc.dou[,1]=="いく"),which(DM.wc.dou[,1]=="行く")))
merg.dou[[6]] <- list(name="行ける",num=c(which(DM.wc.dou[,1]=="いける"),which(DM.wc.dou[,1]=="行ける")))
merg.dou[[7]] <- list(name="行こう",num=c(which(DM.wc.dou[,1]=="いこう"),which(DM.wc.dou[,1]=="行こう")))
merg.dou[[8]] <- list(name="頂く",num=c(which(DM.wc.dou[,1]=="いただく"),which(DM.wc.dou[,1]=="頂く")))
merg.dou[[9]] <- list(name="至る",num=c(which(DM.wc.dou[,1]=="いたる"),which(DM.wc.dou[,1]=="至る")))
merg.dou[[10]] <- list(name="驚く",num=c(which(DM.wc.dou[,1]=="おどろく"),which(DM.wc.dou[,1]=="驚く")))
merg.dou[[11]] <- list(name="かけ離れる",num=c(which(DM.wc.dou[,1]=="かけはなれる"),which(DM.wc.dou[,1]=="かけ離れる")))
merg.dou[[12]] <- list(name="兼ねる",num=c(which(DM.wc.dou[,1]=="かねる"),which(DM.wc.dou[,1]=="兼ねる")))
merg.dou[[13]] <- list(name="決まる",num=c(which(DM.wc.dou[,1]=="きまる"),which(DM.wc.dou[,1]=="決まる")))
merg.dou[[14]] <- list(name="決める",num=c(which(DM.wc.dou[,1]=="きめる"),which(DM.wc.dou[,1]=="決める")))
merg.dou[[15]] <- list(name="下さる",num=c(which(DM.wc.dou[,1]=="くださる"),which(DM.wc.dou[,1]=="下さる")))
merg.dou[[16]] <- list(name="来る",num=c(which(DM.wc.dou[,1]=="くる"),which(DM.wc.dou[,1]=="来る")))
merg.dou[[17]] <- list(name="捧げる",num=c(which(DM.wc.dou[,1]=="ささげる"),which(DM.wc.dou[,1]=="捧げる")))
merg.dou[[18]] <- list(name="強いる",num=c(which(DM.wc.dou[,1]=="しいる"),which(DM.wc.dou[,1]=="強いる")))
merg.dou[[19]] <- list(name="慕う",num=c(which(DM.wc.dou[,1]=="したう"),which(DM.wc.dou[,1]=="慕う")))
merg.dou[[20]] <- list(name="知れる",num=c(which(DM.wc.dou[,1]=="しれる"),which(DM.wc.dou[,1]=="知れる")))
merg.dou[[21]] <- list(name="過ぎる",num=c(which(DM.wc.dou[,1]=="すぎる"),which(DM.wc.dou[,1]=="過ぎる")))
merg.dou[[22]] <- list(name="過ごす",num=c(which(DM.wc.dou[,1]=="すごす"),which(DM.wc.dou[,1]=="過ごす")))
merg.dou[[23]] <- list(name="迫る",num=c(which(DM.wc.dou[,1]=="せまる"),which(DM.wc.dou[,1]=="迫る")))
merg.dou[[24]] <- list(name="辿り着く",num=c(which(DM.wc.dou[,1]=="たどり着く"),which(DM.wc.dou[,1]=="たどりつく"),
                                         which(DM.wc.dou[,1]=="辿り着く"),which(DM.wc.dou[,1]=="辿りつく")))
merg.dou[[25]] <- list(name="出す",num=c(which(DM.wc.dou[,1]=="だす"),which(DM.wc.dou[,1]=="出す")))
merg.dou[[26]] <- list(name="掴む",num=c(which(DM.wc.dou[,1]=="つかむ"),which(DM.wc.dou[,1]=="掴む")))
merg.dou[[27]] <- list(name="突き詰める",num=c(which(DM.wc.dou[,1]=="つきつめる"),which(DM.wc.dou[,1]=="突き詰める")))
merg.dou[[28]] <- list(name="尽くす",num=c(which(DM.wc.dou[,1]=="つくす"),which(DM.wc.dou[,1]=="尽くす")))
merg.dou[[29]] <- list(name="作る.創る",num=c(which(DM.wc.dou[,1]=="つくる"),which(DM.wc.dou[,1]=="つくりだす"),
                                          which(DM.wc.dou[,1]=="作り出す"),which(DM.wc.dou[,1]=="創り出す"),
                                          which(DM.wc.dou[,1]=="作る"),which(DM.wc.dou[,1]=="創る")))
merg.dou[[30]] <- list(name="作り上げる.創り上げる",num=c(which(DM.wc.dou[,1]=="つくり上げる"),
                                                which(DM.wc.dou[,1]=="作り上げる"),which(DM.wc.dou[,1]=="創り上げる"),
                                                which(DM.wc.dou[,1]=="作りあげる")))
merg.dou[[31]] <- list(name="伝える",num=c(which(DM.wc.dou[,1]=="つたえる"),which(DM.wc.dou[,1]=="伝える")))
merg.dou[[32]] <- list(name="突っ込む",num=c(which(DM.wc.dou[,1]=="つっこむ"),which(DM.wc.dou[,1]=="突っ込む")))
merg.dou[[33]] <- list(name="繋がる",num=c(which(DM.wc.dou[,1]=="つながる"),which(DM.wc.dou[,1]=="繋がる")))
merg.dou[[34]] <- list(name="繋ぐ",num=c(which(DM.wc.dou[,1]=="つなぐ"),which(DM.wc.dou[,1]=="つなげる"),
                                        which(DM.wc.dou[,1]=="繋ぐ"),which(DM.wc.dou[,1]=="繋げる")))
merg.dou[[35]] <- list(name="呟く",num=c(which(DM.wc.dou[,1]=="つぶやく"),which(DM.wc.dou[,1]=="呟く")))
merg.dou[[36]] <- list(name="成し遂げる",num=c(which(DM.wc.dou[,1]=="なしとげる"),which(DM.wc.dou[,1]=="成し遂げる")))
merg.dou[[37]] <- list(name="なる",num=c(which(DM.wc.dou[,1]=="なる"),which(DM.wc.dou[,1]=="成る")))
merg.dou[[38]] <- list(name="飲む",num=c(which(DM.wc.dou[,1]=="のむ"),which(DM.wc.dou[,1]=="飲む")))
merg.dou[[39]] <- list(name="のめり込む",num=c(which(DM.wc.dou[,1]=="のめりこむ"),which(DM.wc.dou[,1]=="のめり込む")))
merg.dou[[40]] <- list(name="はじめる",num=c(which(DM.wc.dou[,1]=="はじめる"),which(DM.wc.dou[,1]=="始める")))
merg.dou[[41]] <- list(name="引き出せる",num=c(which(DM.wc.dou[,1]=="ひきだせる"),which(DM.wc.dou[,1]=="引き出せる")))
merg.dou[[42]] <- list(name="引っ張る",num=c(which(DM.wc.dou[,1]=="ひっぱる"),which(DM.wc.dou[,1]=="引っ張る")))
merg.dou[[43]] <- list(name="ぶち込む",num=c(which(DM.wc.dou[,1]=="ぶちこむ"),which(DM.wc.dou[,1]=="ぶち込む")))
merg.dou[[44]] <- list(name="まとめる",num=c(which(DM.wc.dou[,1]=="まとめる"),which(DM.wc.dou[,1]=="まとめあげる")))
merg.dou[[45]] <- list(name="見える",num=c(which(DM.wc.dou[,1]=="みえる"),which(DM.wc.dou[,1]=="見える")))
merg.dou[[46]] <- list(name="見せる",num=c(which(DM.wc.dou[,1]=="みせる"),which(DM.wc.dou[,1]=="見せる")))
merg.dou[[47]] <- list(name="見つける",num=c(which(DM.wc.dou[,1]=="みつける"),which(DM.wc.dou[,1]=="見つける")))
merg.dou[[48]] <- list(name="見る.観る",num=c(which(DM.wc.dou[,1]=="みる"),which(DM.wc.dou[,1]=="見る"),which(DM.wc.dou[,1]=="観る")))
merg.dou[[49]] <- list(name="持つ",num=c(which(DM.wc.dou[,1]=="もつ"),which(DM.wc.dou[,1]=="持つ")))
merg.dou[[50]] <- list(name="もらう.貰う",num=c(which(DM.wc.dou[,1]=="もらう"),which(DM.wc.dou[,1]=="貰う"),which(DM.wc.dou[,1]=="くれる")))
merg.dou[[51]] <- list(name="もらえる.貰う",num=c(which(DM.wc.dou[,1]=="もらえる"),which(DM.wc.dou[,1]=="貰える")))
merg.dou[[52]] <- list(name="破る",num=c(which(DM.wc.dou[,1]=="やぶる"),which(DM.wc.dou[,1]=="破る")))
merg.dou[[53]] <- list(name="やる.やり遂げる",num=c(which(DM.wc.dou[,1]=="やる"),which(DM.wc.dou[,1]=="やり遂げる")))
merg.dou[[54]] <- list(name="引き出す",num=c(which(DM.wc.dou[,1]=="引きだす"),which(DM.wc.dou[,1]=="引き出す")))
merg.dou[[55]] <- list(name="起こす",num=c(which(DM.wc.dou[,1]=="起こす"),which(DM.wc.dou[,1]=="起す")))
merg.dou[[56]] <- list(name="近づく",num=c(which(DM.wc.dou[,1]=="近づく"),which(DM.wc.dou[,1]=="近付く")))
merg.dou[[57]] <- list(name="合う",num=c(which(DM.wc.dou[,1]=="合う")))
merg.dou[[58]] <- list(name="終わる",num=c(which(DM.wc.dou[,1]=="終わる")))
merg.dou[[59]] <- list(name="出す",num=c(which(DM.wc.dou[,1]=="出す")))
merg.dou[[60]] <- list(name="切る",num=c(which(DM.wc.dou[,1]=="切る")))
merg.dou[[61]] <- list(name="取り掛かる",num=c(which(DM.wc.dou[,1]=="取り掛かる"),which(DM.wc.dou[,1]=="取りかかる")))
merg.dou[[62]] <- list(name="借りる",num=c(which(DM.wc.dou[,1]=="借りる"),which(DM.wc.dou[,1]=="かりる")))
merg.dou[[63]] <- list(name="置く",num=c(which(DM.wc.dou[,1]=="置く"),which(DM.wc.dou[,1]=="おく")))

add.wc <- NULL
delete3.num <- NULL

for(i in 1:length(merg.dou)) {
  add.wc[[i]] <- c(merg.dou[[i]]$name,"動詞","追加",rep(0,ncol(DM.wc.dou)-3))
  delete3.num <- c(delete3.num,merg.dou[[i]]$num)
  for(j in merg.dou[[i]]$num) {
    add.wc[[i]][4:length(add.wc[[i]])] <- as.numeric(add.wc[[i]][4:length(add.wc[[i]])]) + DM.wc.dou[j,4:length(add.wc[[i]])]
  }
  DM.wc.dou <- rbind(DM.wc.dou,add.wc[[i]])
}

delete3.num <- sort(delete3.num)
delete3 <- DM.wc.dou[delete3.num,1]

#重複語を消す
DM.wc.dou <- DM.wc.dou[-delete3.num,]

###名詞
DM.wc.mei <- DM.wc[DM.wc[,2] == "名詞",]
delete4.num <- NULL

table(DM.wc.mei[,3])
#不要な語の抽出
delete4.num <- c(which(DM.wc.mei[,1]=="ー"),which(DM.wc.mei[,1]=="ー・・"),
                 which(DM.wc.mei[,1]=="ー・・・"),which(DM.wc.mei[,1]=="１つ"),
                 which(DM.wc.mei[,1]=="２つ"),which(DM.wc.mei[,1]=="３つ"),
                 which(DM.wc.mei[,1]=="４つ"),which(DM.wc.mei[,1]=="５つ"),
                 which(DM.wc.mei[,1]=="６つ"),which(DM.wc.mei[,1]=="ぁ"),
                 which(DM.wc.mei[,1]=="ぁああ"),which(DM.wc.mei[,1]=="ぁもう"),
                 which(DM.wc.mei[,1]=="あん"),which(DM.wc.mei[,1]=="い"),
                 which(DM.wc.mei[,1]=="ぇ"),which(DM.wc.mei[,1]=="ぇぇぇえぇぇ"),
                 which(DM.wc.mei[,1]=="ぉぉ"),which(DM.wc.mei[,1]=="ゃっきゃと"),
                 which(DM.wc.mei[,1]=="ゃり"),which(DM.wc.mei[,1]=="ろ"),
                 which(DM.wc.mei[,1]=="ゃり"),which(DM.wc.mei[,1]=="こ"),
                 which(DM.wc.mei[,1]=="ぐっ"),which(DM.wc.mei[,1]=="ぺ"),
                 which(DM.wc.mei[,1]=="ろ"))
#平仮名表記の語の漢字表記/送り仮名が異なる語/包含関係のある語/同義語を探す
merg.mei <- NULL
merg.mei[[1]] <- list(name="SNS",num=c(which(DM.wc.mei[,1]=="Facebook"),which(DM.wc.mei[,1]=="Ｆacebook"),
                                       which(DM.wc.mei[,1]=="mixi"),which(DM.wc.mei[,1]=="Twitter"),
                                       which(DM.wc.mei[,1]=="Ｔｗｉｔｔｅｒ"),which(DM.wc.mei[,1]=="ツイッター"),
                                       which(DM.wc.mei[,1]=="ツイート")))
merg.mei[[2]] <- list(name="IT",num=c(which(DM.wc.mei[,1]=="IT"),which(DM.wc.mei[,1]=="ＩＴ")))
merg.mei[[3]] <- list(name="DNA",num=c(which(DM.wc.mei[,1]=="DNA"),which(DM.wc.mei[,1]=="ＤＮＡ")))
merg.mei[[4]] <- list(name="HP",num=c(which(DM.wc.mei[,1]=="HP"),which(DM.wc.mei[,1]=="ＨＰ")))
merg.mei[[5]] <- list(name="MC",num=c(which(DM.wc.mei[,1]=="MC"),which(DM.wc.mei[,1]=="ＭＣ")))
merg.mei[[6]] <- list(name="NGO",num=c(which(DM.wc.mei[,1]=="NGO"),which(DM.wc.mei[,1]=="ＮＧＯ")))
merg.mei[[7]] <- list(name="NPO",num=c(which(DM.wc.mei[,1]=="NPO"),which(DM.wc.mei[,1]=="ＮＰＯ")))
merg.mei[[8]] <- list(name="AWARD",num=c(which(DM.wc.mei[,1]=="Award"),which(DM.wc.mei[,1]=="ＡＷＡＲＤ"),which(DM.wc.mei[,1]=="Award")))
merg.mei[[9]] <- list(name="WEB.発信",num=c(which(DM.wc.mei[,1]=="WEB"),which(DM.wc.mei[,1]=="ＷＥＢ"),which(DM.wc.mei[,1]=="ウェブ"),
                                       which(DM.wc.mei[,1]=="ホームページ"),which(DM.wc.mei[,1]=="ブログ")))
merg.mei[[10]] <- list(name="スマートフォン",
                       num=c(which(DM.wc.mei[,1]=="iphone"),which(DM.wc.mei[,1]=="iPhone"),
                             which(DM.wc.mei[,1]=="スマートフォンアプリ"),
                             which(DM.wc.mei[,1]=="スマートフォンアプリケーション")))
merg.mei[[11]] <- list(name="国際銀行",num=c(which(DM.wc.mei[,1]=="世界銀行"),
                                         which(DM.wc.mei[,1]=="世銀"),which(DM.wc.mei[,1]=="グラミン")))
merg.mei[[12]] <- list(name="大学",num=c(which(DM.wc.mei[,1]=="関西大学"),which(DM.wc.mei[,1]=="岩手大学"),
                                       which(DM.wc.mei[,1]=="九州大学"),which(DM.wc.mei[,1]=="駒澤大学"),
                                       which(DM.wc.mei[,1]=="青山学院大学"),which(DM.wc.mei[,1]=="早稲田大学"),
                                       which(DM.wc.mei[,1]=="大学"),which(DM.wc.mei[,1]=="大学院"),
                                       which(DM.wc.mei[,1]=="東京大学"),which(DM.wc.mei[,1]=="同志社大学"),
                                       which(DM.wc.mei[,1]=="立教大学"),which(DM.wc.mei[,1]=="立命館大学"),
                                       which(DM.wc.mei[,1]=="龍谷大学"),which(DM.wc.mei[,1]=="関大"),
                                       which(DM.wc.mei[,1]=="京大"),which(DM.wc.mei[,1]=="芸大"),
                                       which(DM.wc.mei[,1]=="早大"),which(DM.wc.mei[,1]=="東大"),
                                       which(DM.wc.mei[,1]=="慶"),which(DM.wc.mei[,1]=="應"),
                                       which(DM.wc.mei[,1]=="慶應義塾"),which(DM.wc.mei[,1]=="立教"),
                                       which(DM.wc.mei[,1]=="同志社"),which(DM.wc.mei[,1]=="早稲田"),
                                       which(DM.wc.mei[,1]=="大隈"),which(DM.wc.mei[,1]=="キャンパス"),
                                       which(DM.wc.mei[,1]=="三田")))
merg.mei[[13]] <- list(name="欧米",num=c(which(DM.wc.mei[,1]=="欧米"),which(DM.wc.mei[,1]=="ヨーロッパ"),
                                       which(DM.wc.mei[,1]=="アメリカ"),which(DM.wc.mei[,1]=="ロサンゼルス"),
                                       which(DM.wc.mei[,1]=="ボストン"),which(DM.wc.mei[,1]=="フランス"),
                                       which(DM.wc.mei[,1]=="パリ"),which(DM.wc.mei[,1]=="ハワイ"),
                                       which(DM.wc.mei[,1]=="ニュージーランド"),which(DM.wc.mei[,1]=="ドイツ"),
                                       which(DM.wc.mei[,1]=="デンマーク"),which(DM.wc.mei[,1]=="スウェーデン"),
                                       which(DM.wc.mei[,1]=="スイス"),which(DM.wc.mei[,1]=="スタンフォード"),
                                       which(DM.wc.mei[,1]=="シドニー"),which(DM.wc.mei[,1]=="シリコンバレー"),
                                       which(DM.wc.mei[,1]=="カナダ"),which(DM.wc.mei[,1]=="オーストラリア"),
                                       which(DM.wc.mei[,1]=="イタリア"),which(DM.wc.mei[,1]=="イギリス"),
                                       which(DM.wc.mei[,1]=="アリゾナ"),which(DM.wc.mei[,1]=="ベルギー"),
                                       which(DM.wc.mei[,1]=="ロシア"),which(DM.wc.mei[,1]=="米国"),
                                       which(DM.wc.mei[,1]=="全米"),which(DM.wc.mei[,1]=="アメリカン"),
                                       which(DM.wc.mei[,1]=="マドリッド"),which(DM.wc.mei[,1]=="ヨーロピアン")))
merg.mei[[14]] <- list(name="アジア.アフリカ.中南米",
                       num=c(which(DM.wc.mei[,1]=="ASEAN"),which(DM.wc.mei[,1]=="アジア"),
                             which(DM.wc.mei[,1]=="インドﾞ"),which(DM.wc.mei[,1]=="アフリカ"),
                             which(DM.wc.mei[,1]=="カンボジア"),which(DM.wc.mei[,1]=="ケニア"),
                             which(DM.wc.mei[,1]=="サンパウロ"),which(DM.wc.mei[,1]=="ザンビア"),
                             which(DM.wc.mei[,1]=="ソマリア"),which(DM.wc.mei[,1]=="チュニジア"),
                             which(DM.wc.mei[,1]=="トルコ"),which(DM.wc.mei[,1]=="ネパール"),
                             which(DM.wc.mei[,1]=="フィリピン"),which(DM.wc.mei[,1]=="ブラジル"),
                             which(DM.wc.mei[,1]=="ベトナム"),which(DM.wc.mei[,1]=="マレーシア"),
                             which(DM.wc.mei[,1]=="メキシコ"),which(DM.wc.mei[,1]=="ラオス"),
                             which(DM.wc.mei[,1]=="ラテンアメリカ"),which(DM.wc.mei[,1]=="韓国"),
                             which(DM.wc.mei[,1]=="香港"),which(DM.wc.mei[,1]=="台湾"),
                             which(DM.wc.mei[,1]=="中国"),which(DM.wc.mei[,1]=="東南アジア")))
merg.mei[[15]] <- list(name="地方.地元",
                       num=c(which(DM.wc.mei[,1]=="伊東"),which(DM.wc.mei[,1]=="伊豆"),
                             which(DM.wc.mei[,1]=="茨城"),which(DM.wc.mei[,1]=="横浜"),
                             which(DM.wc.mei[,1]=="沖縄"),which(DM.wc.mei[,1]=="岩手"),
                             which(DM.wc.mei[,1]=="京都"),which(DM.wc.mei[,1]=="九州"),
                             which(DM.wc.mei[,1]=="群馬"),which(DM.wc.mei[,1]=="厚木"),
                             which(DM.wc.mei[,1]=="広島"),which(DM.wc.mei[,1]=="若狭"),
                             which(DM.wc.mei[,1]=="札幌"),which(DM.wc.mei[,1]=="山形"),
                             which(DM.wc.mei[,1]=="四国"),which(DM.wc.mei[,1]=="鹿児島"),
                             which(DM.wc.mei[,1]=="湘南"),which(DM.wc.mei[,1]=="城崎"),
                             which(DM.wc.mei[,1]=="神戸"),which(DM.wc.mei[,1]=="盛岡"),
                             which(DM.wc.mei[,1]=="仙台"),which(DM.wc.mei[,1]=="大阪"),
                             which(DM.wc.mei[,1]=="淡路"),which(DM.wc.mei[,1]=="長崎"),
                             which(DM.wc.mei[,1]=="長野"),which(DM.wc.mei[,1]=="藤沢"),
                             which(DM.wc.mei[,1]=="梅田"),which(DM.wc.mei[,1]=="福岡"),
                             which(DM.wc.mei[,1]=="福島"),which(DM.wc.mei[,1]=="北海道"),
                             which(DM.wc.mei[,1]=="北信越"),which(DM.wc.mei[,1]=="枚方"),
                             which(DM.wc.mei[,1]=="名古屋"),which(DM.wc.mei[,1]=="みちのく"),
                             which(DM.wc.mei[,1]=="地方"),which(DM.wc.mei[,1]=="地方自治体")))
merg.mei[[16]] <- list(name="東京",
                       num=c(which(DM.wc.mei[,1]=="東京"),which(DM.wc.mei[,1]=="原宿"),
                             which(DM.wc.mei[,1]=="秋葉原"),which(DM.wc.mei[,1]=="渋谷"),
                             which(DM.wc.mei[,1]=="新宿"),which(DM.wc.mei[,1]=="青山"),
                             which(DM.wc.mei[,1]=="中野"),which(DM.wc.mei[,1]=="トーキョー")))
merg.mei[[17]] <- list(name="ブランド.ファッション",
                       num=c(which(DM.wc.mei[,1]=="Beauty"),which(DM.wc.mei[,1]=="CHANEL"),
                             which(DM.wc.mei[,1]=="FASHION"),which(DM.wc.mei[,1]=="GAP"),
                             which(DM.wc.mei[,1]=="HOMME"),which(DM.wc.mei[,1]=="Tシャツ"),
                             which(DM.wc.mei[,1]=="cawaii"),which(DM.wc.mei[,1]=="エシカルファッションショー"),
                             which(DM.wc.mei[,1]=="ココ・シャネル"),which(DM.wc.mei[,1]=="ジーンズ"),
                             which(DM.wc.mei[,1]=="ドレスアップ"),which(DM.wc.mei[,1]=="ネクタイ"),
                             which(DM.wc.mei[,1]=="ノーブル・エイペックス"),which(DM.wc.mei[,1]=="ノーブルタイ"),
                             which(DM.wc.mei[,1]=="パリコレ"),which(DM.wc.mei[,1]=="ファッション"),
                             which(DM.wc.mei[,1]=="ファッションショー"),which(DM.wc.mei[,1]=="メイク"),
                             which(DM.wc.mei[,1]=="メンズコスメ"),which(DM.wc.mei[,1]=="化粧"),
                             which(DM.wc.mei[,1]=="ファッション")))
merg.mei[[18]] <- list(name="タレント",
                       num=c(which(DM.wc.mei[,1]=="Perfume"),which(DM.wc.mei[,1]=="LiLiCo"),
                             which(DM.wc.mei[,1]=="AKB")))
merg.mei[[19]] <- list(name="イノベーション",
                       num=c(which(DM.wc.mei[,1]=="イノベーション"),
                             which(DM.wc.mei[,1]=="イノベータ"),
                             which(DM.wc.mei[,1]=="イノベータ―")))
merg.mei[[20]] <- list(name="グローバル",
                       num=c(which(DM.wc.mei[,1]=="インターナショナル"),
                             which(DM.wc.mei[,1]=="グローバルフェスタ"),
                             which(DM.wc.mei[,1]=="世界中")))
merg.mei[[21]] <- list(name="インターン",
                       num=c(which(DM.wc.mei[,1]=="インターン"),
                             which(DM.wc.mei[,1]=="インターンシップ")))
merg.mei[[22]] <- list(name="印象",
                       num=c(which(DM.wc.mei[,1]=="印象"),
                             which(DM.wc.mei[,1]=="インプレッション")))
merg.mei[[23]] <- list(name="学習",
                       num=c(which(DM.wc.mei[,1]=="学習"),
                             which(DM.wc.mei[,1]=="インプット"),
                             which(DM.wc.mei[,1]=="勉強")))
merg.mei[[24]] <- list(name="噂",
                       num=c(which(DM.wc.mei[,1]=="噂"),
                             which(DM.wc.mei[,1]=="ウワサ")))
merg.mei[[25]] <- list(name="エキスポ.展示",
                       num=c(which(DM.wc.mei[,1]=="EXPO"),
                             which(DM.wc.mei[,1]=="エキスポ"),
                             which(DM.wc.mei[,1]=="展示")))
merg.mei[[26]] <- list(name="エコ",
                       num=c(which(DM.wc.mei[,1]=="エコ"),
                             which(DM.wc.mei[,1]=="eco")))
merg.mei[[27]] <- list(name="エンターテイメント",
                       num=c(which(DM.wc.mei[,1]=="エンターテイナー"),
                             which(DM.wc.mei[,1]=="エンターテイメント"),
                             which(DM.wc.mei[,1]=="エンターテインメント"),
                             which(DM.wc.mei[,1]=="パフォーマー")))
merg.mei[[28]] <- list(name="カメラ.写真",
                       num=c(which(DM.wc.mei[,1]=="カメラ"),
                             which(DM.wc.mei[,1]=="カメラマン"),
                             which(DM.wc.mei[,1]=="デジカメ"),
                             which(DM.wc.mei[,1]=="写真"),
                             which(DM.wc.mei[,1]=="一眼")))
merg.mei[[29]] <- list(name="文化",
                       num=c(which(DM.wc.mei[,1]=="カルチャー"),
                             which(DM.wc.mei[,1]=="文化")))
merg.mei[[30]] <- list(name="会議.ディスカッション",
                       num=c(which(DM.wc.mei[,1]=="会議"),
                             which(DM.wc.mei[,1]=="カンファレンス"),
                             which(DM.wc.mei[,1]=="ミーティング"),
                             which(DM.wc.mei[,1]=="シンポジウム"),
                             which(DM.wc.mei[,1]=="セッション"),
                             which(DM.wc.mei[,1]=="ディスカッション"),
                             which(DM.wc.mei[,1]=="ワークショップ")))
merg.mei[[31]] <- list(name="キッカケ",
                       num=c(which(DM.wc.mei[,1]=="きっかけ"),
                             which(DM.wc.mei[,1]=="キッカケ"),
                             which(DM.wc.mei[,1]=="キッカケプロジェクト")))
merg.mei[[32]] <- list(name="料理",
                       num=c(which(DM.wc.mei[,1]=="料理"),
                             which(DM.wc.mei[,1]=="キッチン"),
                             which(DM.wc.mei[,1]=="ケーキ"),
                             which(DM.wc.mei[,1]=="パンケーキ"),
                             which(DM.wc.mei[,1]=="ブリュレフレンチトースト"),
                             which(DM.wc.mei[,1]=="ホットケーキ"),
                             which(DM.wc.mei[,1]=="レシピ"),
                             which(DM.wc.mei[,1]=="レストラン")))
merg.mei[[33]] <- list(name="代表.キャプテン",
                       num=c(which(DM.wc.mei[,1]=="キャプテン"),
                             which(DM.wc.mei[,1]=="代表")))
merg.mei[[34]] <- list(name="個性.キャラクター",
                       num=c(which(DM.wc.mei[,1]=="キャラクター"),
                             which(DM.wc.mei[,1]=="キャラ"),
                             which(DM.wc.mei[,1]=="パーソナリティ"),
                             which(DM.wc.mei[,1]=="個性")))
merg.mei[[35]] <- list(name="キャリア",
                       num=c(which(DM.wc.mei[,1]=="キャリア"),
                             which(DM.wc.mei[,1]=="キャリアウーマン")))
merg.mei[[35]] <- list(name="質.クオリティ",
                       num=c(which(DM.wc.mei[,1]=="クオリティ"),
                             which(DM.wc.mei[,1]=="質")))
merg.mei[[36]] <- list(name="クラウド",
                       num=c(which(DM.wc.mei[,1]=="クラウドソーシング"),
                             which(DM.wc.mei[,1]=="クラウドファンディング"),
                             which(DM.wc.mei[,1]=="クラウドファンディングサービス")))
merg.mei[[37]] <- list(name="クリエイティブ",
                       num=c(which(DM.wc.mei[,1]=="クリエイション"),
                             which(DM.wc.mei[,1]=="クリエイター"),
                             which(DM.wc.mei[,1]=="クリエイティブ"),
                             which(DM.wc.mei[,1]=="クリエティブディレクター")))
merg.mei[[38]] <- list(name="優勝",
                       num=c(which(DM.wc.mei[,1]=="優勝"),
                             which(DM.wc.mei[,1]=="グランプリ")))
merg.mei[[39]] <- list(name="団体.サークル.チーム",
                       num=c(which(DM.wc.mei[,1]=="団体"),
                             which(DM.wc.mei[,1]=="グループ"),
                             which(DM.wc.mei[,1]=="サークル"),
                             which(DM.wc.mei[,1]=="チーム")))
merg.mei[[40]] <- list(name="教育.コーチング",
                       num=c(which(DM.wc.mei[,1]=="コーチング"),
                             which(DM.wc.mei[,1]=="教育")))
merg.mei[[41]] <- list(name="人脈",
                       num=c(which(DM.wc.mei[,1]=="人脈"),
                             which(DM.wc.mei[,1]=="コネクション")))
merg.mei[[42]] <- list(name="コラボレーション",
                       num=c(which(DM.wc.mei[,1]=="コラボ"),
                             which(DM.wc.mei[,1]=="コラボレーション"),
                             which(DM.wc.mei[,1]=="コラボレート"),
                             which(DM.wc.mei[,1]=="タイアップ"),
                             which(DM.wc.mei[,1]=="デュオ"),
                             which(DM.wc.mei[,1]=="共演")))
merg.mei[[43]] <- list(name="コワーキングスペース",
                       num=c(which(DM.wc.mei[,1]=="コワーキング・スペース"),
                             which(DM.wc.mei[,1]=="コワーキングスペース")))
merg.mei[[44]] <- list(name="音楽",
                       num=c(which(DM.wc.mei[,1]=="コンサート"),
                             which(DM.wc.mei[,1]=="演奏"),
                             which(DM.wc.mei[,1]=="シンガー"),
                             which(DM.wc.mei[,1]=="シンガーソングライター"),
                             which(DM.wc.mei[,1]=="ソング"),
                             which(DM.wc.mei[,1]=="タワレコ"),
                             which(DM.wc.mei[,1]=="トロンボーン"),
                             which(DM.wc.mei[,1]=="ドリームボーカルオーディション"),
                             which(DM.wc.mei[,1]=="ハモネプ"),
                             which(DM.wc.mei[,1]=="バンド"),
                             which(DM.wc.mei[,1]=="ピアノ"),
                             which(DM.wc.mei[,1]=="ライブ"),
                             which(DM.wc.mei[,1]=="ライブハウス"),
                             which(DM.wc.mei[,1]=="歌"),
                             which(DM.wc.mei[,1]=="歌詞"),
                             which(DM.wc.mei[,1]=="歌手"),
                             which(DM.wc.mei[,1]=="歌姫"),
                             which(DM.wc.mei[,1]=="軽音楽")))
merg.mei[[45]] <- list(name="コンサルティング",
                       num=c(which(DM.wc.mei[,1]=="コンサルタント"),
                             which(DM.wc.mei[,1]=="コンサルティング")))
merg.mei[[46]] <- list(name="コンテンツ.中身",
                       num=c(which(DM.wc.mei[,1]=="中身"),
                             which(DM.wc.mei[,1]=="コンテンツ")))
merg.mei[[47]] <- list(name="コンピュータ",
                       num=c(which(DM.wc.mei[,1]=="コンピュータ"),
                             which(DM.wc.mei[,1]=="コンピューター"),
                             which(DM.wc.mei[,1]=="コンピューターグラフィックス"),
                             which(DM.wc.mei[,1]=="パソコン"),
                             which(DM.wc.mei[,1]=="プログラマー"),
                             which(DM.wc.mei[,1]=="プログラミング")))
merg.mei[[48]] <- list(name="劣等感",
                       num=c(which(DM.wc.mei[,1]=="劣等"),
                             which(DM.wc.mei[,1]=="コンプレックス")))
merg.mei[[49]] <- list(name="目標",
                       num=c(which(DM.wc.mei[,1]=="目標"),
                             which(DM.wc.mei[,1]=="ゴール"),
                             which(DM.wc.mei[,1]=="ビジョン"),
                             which(DM.wc.mei[,1]=="ミッション"),
                             which(DM.wc.mei[,1]=="目的")))
merg.mei[[50]] <- list(name="スポーツ",
                       num=c(which(DM.wc.mei[,1]=="野球"),
                             which(DM.wc.mei[,1]=="サッカー"),
                             which(DM.wc.mei[,1]=="サーフィン"),
                             which(DM.wc.mei[,1]=="スケート"),
                             which(DM.wc.mei[,1]=="スケボー"),
                             which(DM.wc.mei[,1]=="スポーツ"),
                             which(DM.wc.mei[,1]=="テニス"),
                             which(DM.wc.mei[,1]=="トランポリン"),
                             which(DM.wc.mei[,1]=="バスケ"),
                             which(DM.wc.mei[,1]=="バドミントン"),
                             which(DM.wc.mei[,1]=="バント"),
                             which(DM.wc.mei[,1]=="フットサル"),
                             which(DM.wc.mei[,1]=="フルマラソン"),
                             which(DM.wc.mei[,1]=="プロサッカーリーグ"),
                             which(DM.wc.mei[,1]=="ホッケー"),
                             which(DM.wc.mei[,1]=="マラソン"),
                             which(DM.wc.mei[,1]=="ラグビー"),
                             which(DM.wc.mei[,1]=="運動"),
                             which(DM.wc.mei[,1]=="英寿"),
                             which(DM.wc.mei[,1]=="イチロー"),
                             which(DM.wc.mei[,1]=="空手"),
                             which(DM.wc.mei[,1]=="剣道"),
                             which(DM.wc.mei[,1]=="新体操")))
merg.mei[[51]] <- list(name="支援.サポート",
                       num=c(which(DM.wc.mei[,1]=="サポート"),
                             which(DM.wc.mei[,1]=="支援"),
                             which(DM.wc.mei[,1]=="助け")))
merg.mei[[52]] <- list(name="参加",
                       num=c(which(DM.wc.mei[,1]=="参加"),
                             which(DM.wc.mei[,1]=="ジョイン")))
merg.mei[[53]] <- list(name="スキル.技能",
                       num=c(which(DM.wc.mei[,1]=="スキル"),
                             which(DM.wc.mei[,1]=="技術"),
                             which(DM.wc.mei[,1]=="技能"),
                             which(DM.wc.mei[,1]=="技")))
merg.mei[[54]] <- list(name="予定",
                       num=c(which(DM.wc.mei[,1]=="予定"),
                             which(DM.wc.mei[,1]=="スケジュール")))
merg.mei[[55]] <- list(name="旅行.ツアー",
                       num=c(which(DM.wc.mei[,1]=="旅"),
                             which(DM.wc.mei[,1]=="旅行"),
                             which(DM.wc.mei[,1]=="ツアー"),
                             which(DM.wc.mei[,1]=="スタディーツアー"),
                             which(DM.wc.mei[,1]=="バックパッカー"),
                             which(DM.wc.mei[,1]=="ヒッチハイク")))
merg.mei[[56]] <- list(name="スティーブジョブズ",
                       num=c(which(DM.wc.mei[,1]=="スティーブ"),
                             which(DM.wc.mei[,1]=="スティーブ・ジョブス"),
                             which(DM.wc.mei[,1]=="ジョブズ")))
merg.mei[[57]] <- list(name="ストーリー",
                       num=c(which(DM.wc.mei[,1]=="ストーリー"),
                             which(DM.wc.mei[,1]=="ストーリーテリング")))
merg.mei[[58]] <- list(name="ストレス.不安",
                       num=c(which(DM.wc.mei[,1]=="ストレス"),
                             which(DM.wc.mei[,1]=="不安"),
                             which(DM.wc.mei[,1]=="プレッシャー")))
merg.mei[[59]] <- list(name="スピーカー",
                       num=c(which(DM.wc.mei[,1]=="スピーカー"),
                             which(DM.wc.mei[,1]=="スピーチ"),
                             which(DM.wc.mei[,1]=="講演")))
merg.mei[[60]] <- list(name="スピード",
                       num=c(which(DM.wc.mei[,1]=="スピード"),
                             which(DM.wc.mei[,1]=="スピーディー")))
merg.mei[[61]] <- list(name="スラム",
                       num=c(which(DM.wc.mei[,1]=="スラム"),
                             which(DM.wc.mei[,1]=="ストリートチルドレン")))
merg.mei[[62]] <- list(name="ゼミ.セミナー",
                       num=c(which(DM.wc.mei[,1]=="ゼミ"),
                             which(DM.wc.mei[,1]=="ゼミナール"),
                             which(DM.wc.mei[,1]=="セミナー")))
merg.mei[[63]] <- list(name="ソーシャルビジネス",
                       num=c(which(DM.wc.mei[,1]=="ソーシャルゲームカンパニー"),
                             which(DM.wc.mei[,1]=="ソーシャルコマーステクノロジー"),
                             which(DM.wc.mei[,1]=="ソーシャルノートサービス"),
                             which(DM.wc.mei[,1]=="ソーシャルビジネス"),
                             which(DM.wc.mei[,1]=="ソーシャルファンディング"),
                             which(DM.wc.mei[,1]=="ソーシャルマーケティング"),
                             which(DM.wc.mei[,1]=="ソーシャルメディア")))
merg.mei[[64]] <- list(name="転機",
                       num=c(which(DM.wc.mei[,1]=="転機"),
                             which(DM.wc.mei[,1]=="ターニングポイント")))
merg.mei[[65]] <- list(name="無料.無償",
                       num=c(which(DM.wc.mei[,1]=="無料"),which(DM.wc.mei[,1]=="タダ"),
                             which(DM.wc.mei[,1]=="無償")))
merg.mei[[66]] <- list(name="カフェ",
                       num=c(which(DM.wc.mei[,1]=="スタバ"),
                             which(DM.wc.mei[,1]=="タリーズコーヒー"),
                             which(DM.wc.mei[,1]=="カフェ")))
merg.mei[[67]] <- list(name="変化",
                       num=c(which(DM.wc.mei[,1]=="変化"),
                             which(DM.wc.mei[,1]=="チェンジ")))
merg.mei[[68]] <- list(name="チャリティー.寄付",
                       num=c(which(DM.wc.mei[,1]=="チャリティ"),
                             which(DM.wc.mei[,1]=="チャリティー"),
                             which(DM.wc.mei[,1]=="ボランティア"),
                             which(DM.wc.mei[,1]=="寄付")))
merg.mei[[69]] <- list(name="挑戦",
                       num=c(which(DM.wc.mei[,1]=="チャレンジ"),
                             which(DM.wc.mei[,1]=="挑戦")))
merg.mei[[70]] <- list(name="チャンス",
                       num=c(which(DM.wc.mei[,1]=="チャンス"),
                             which(DM.wc.mei[,1]=="機会")))
merg.mei[[71]] <- list(name="チョコレート",
                       num=c(which(DM.wc.mei[,1]=="チョコ"),
                             which(DM.wc.mei[,1]=="チョコレート")))
merg.mei[[72]] <- list(name="テレビ",
                       num=c(which(DM.wc.mei[,1]=="フジテレビ"),
                             which(DM.wc.mei[,1]=="TBS"),
                             which(DM.wc.mei[,1]=="テレビ"),
                             which(DM.wc.mei[,1]=="ドキュメンタリー"),
                             which(DM.wc.mei[,1]=="ドラマ"),
                             which(DM.wc.mei[,1]=="バラエティ")))
merg.mei[[73]] <- list(name="デート",
                       num=c(which(DM.wc.mei[,1]=="デート"),
                             which(DM.wc.mei[,1]=="デートプラン")))
merg.mei[[74]] <- list(name="ディズニー",
                       num=c(which(DM.wc.mei[,1]=="ディズニー"),
                             which(DM.wc.mei[,1]=="ディズニーランド")))
merg.mei[[75]] <- list(name="デザイン",
                       num=c(which(DM.wc.mei[,1]=="デザイナー"),
                             which(DM.wc.mei[,1]=="デザイン"),
                             which(DM.wc.mei[,1]=="デザインコンサルティングファーム")))
merg.mei[[76]] <- list(name="トーク",
                       num=c(which(DM.wc.mei[,1]=="トーク"),
                             which(DM.wc.mei[,1]=="会話")))
merg.mei[[77]] <- list(name="夢",
                       num=c(which(DM.wc.mei[,1]=="ドリーム"),
                             which(DM.wc.mei[,1]=="夢")))
merg.mei[[78]] <- list(name="ノルマ.ハードル",
                       num=c(which(DM.wc.mei[,1]=="ノルマ"),
                             which(DM.wc.mei[,1]=="ハードル")))
merg.mei[[79]] <- list(name="漫画.アニメ",
                       num=c(which(DM.wc.mei[,1]=="スナフキン"),
                             which(DM.wc.mei[,1]=="ハイジ"),
                             which(DM.wc.mei[,1]=="ボーイズラブ"),
                             which(DM.wc.mei[,1]=="漫画"),
                             which(DM.wc.mei[,1]=="アニメ"),
                             which(DM.wc.mei[,1]=="ポケモンカード"),
                             which(DM.wc.mei[,1]=="マンガ")))
merg.mei[[80]] <- list(name="幸せ",
                       num=c(which(DM.wc.mei[,1]=="ハッピー"),
                             which(DM.wc.mei[,1]=="幸福"),
                             which(DM.wc.mei[,1]=="幸せ"),
                             which(DM.wc.mei[,1]=="ハッピーエンド"),
                             which(DM.wc.mei[,1]=="幸")))
merg.mei[[81]] <- list(name="誕生.誕生日",
                       num=c(which(DM.wc.mei[,1]=="誕生"),
                             which(DM.wc.mei[,1]=="バースデー")))
merg.mei[[82]] <- list(name="パーティー",
                       num=c(which(DM.wc.mei[,1]=="パーティ"),
                             which(DM.wc.mei[,1]=="パーティー")))
merg.mei[[83]] <- list(name="パソナ",
                       num=c(which(DM.wc.mei[,1]=="パソナ"),
                             which(DM.wc.mei[,1]=="パソナキャリアカンパニー")))
merg.mei[[84]] <- list(name="情熱",
                       num=c(which(DM.wc.mei[,1]=="情熱"),
                             which(DM.wc.mei[,1]=="パッション")))
merg.mei[[85]] <- list(name="決勝",
                       num=c(which(DM.wc.mei[,1]=="決勝"),
                             which(DM.wc.mei[,1]=="ファイナル"),
                             which(DM.wc.mei[,1]=="ファイナリスト")))
merg.mei[[86]] <- list(name="ファイナンス",
                       num=c(which(DM.wc.mei[,1]=="ファイナンシャルアカデミー"),
                             which(DM.wc.mei[,1]=="ファイナンシャルプランナー"),
                             which(DM.wc.mei[,1]=="ファイナンス")))
merg.mei[[87]] <- list(name="感覚.感情",
                       num=c(which(DM.wc.mei[,1]=="フィーリング"),
                             which(DM.wc.mei[,1]=="感覚"),
                             which(DM.wc.mei[,1]=="感情"),
                             which(DM.wc.mei[,1]=="感性"),
                             which(DM.wc.mei[,1]=="感じ"),
                             which(DM.wc.mei[,1]=="五感")))
merg.mei[[88]] <- list(name="祭",
                       num=c(which(DM.wc.mei[,1]=="フェスティバル"),
                             which(DM.wc.mei[,1]=="フェス"),
                             which(DM.wc.mei[,1]=="祭り"),
                             which(DM.wc.mei[,1]=="祭")))
merg.mei[[89]] <- list(name="ブレークスルー",
                       num=c(which(DM.wc.mei[,1]=="ブレイクスルー"),
                             which(DM.wc.mei[,1]=="ブレイクスルーキャンプ")))
merg.mei[[90]] <- list(name="プレゼンテーション",
                       num=c(which(DM.wc.mei[,1]=="プレゼン"),
                             which(DM.wc.mei[,1]=="プレゼンター"),
                             which(DM.wc.mei[,1]=="プレゼンテーション")))
merg.mei[[91]] <- list(name="プロデュース.監督",
                       num=c(which(DM.wc.mei[,1]=="プロデュース"),
                             which(DM.wc.mei[,1]=="プロデューサー"),
                             which(DM.wc.mei[,1]=="監督"),
                             which(DM.wc.mei[,1]=="監修")))
merg.mei[[92]] <- list(name="ホームステイ",
                       num=c(which(DM.wc.mei[,1]=="ホームステイ"),
                             which(DM.wc.mei[,1]=="ホストファミリー")))
merg.mei[[93]] <- list(name="本音",
                       num=c(which(DM.wc.mei[,1]=="ホンネ"),
                             which(DM.wc.mei[,1]=="本音")))
merg.mei[[94]] <- list(name="ポジティブ",
                       num=c(which(DM.wc.mei[,1]=="前向き"),
                             which(DM.wc.mei[,1]=="ポジティブ"),
                             which(DM.wc.mei[,1]=="楽観")))
merg.mei[[95]] <- list(name="ポリシー.信念",
                       num=c(which(DM.wc.mei[,1]=="ポリシー"),
                             which(DM.wc.mei[,1]=="信念"),
                             which(DM.wc.mei[,1]=="モットー"),
                             which(DM.wc.mei[,1]=="座右の銘")))
merg.mei[[96]] <- list(name="マスコミ",
                       num=c(which(DM.wc.mei[,1]=="マス"),
                             which(DM.wc.mei[,1]=="マスコミ")))
merg.mei[[97]] <- list(name="お金",
                       num=c(which(DM.wc.mei[,1]=="マネー"),
                             which(DM.wc.mei[,1]=="資金"),
                             which(DM.wc.mei[,1]=="お金"),
                             which(DM.wc.mei[,1]=="金銭"),
                             which(DM.wc.mei[,1]=="金額"),
                             which(DM.wc.mei[,1]=="金持ち"),
                             which(DM.wc.mei[,1]=="金儲け")))
merg.mei[[98]] <- list(name="メンタル.精神",
                       num=c(which(DM.wc.mei[,1]=="メンタル"),
                             which(DM.wc.mei[,1]=="メンタルケア"),
                             which(DM.wc.mei[,1]=="精神")))
merg.mei[[99]] <- list(name="リーダー",
                        num=c(which(DM.wc.mei[,1]=="リーダー"),
                              which(DM.wc.mei[,1]=="リーダーシップ")))
merg.mei[[100]] <- list(name="採用活動",
                        num=c(which(DM.wc.mei[,1]=="採用"),
                              which(DM.wc.mei[,1]=="リクルーティング")))
merg.mei[[101]] <- list(name="ルール.規律",
                        num=c(which(DM.wc.mei[,1]=="ルール"),
                              which(DM.wc.mei[,1]=="規則"),
                              which(DM.wc.mei[,1]=="規範"),
                              which(DM.wc.mei[,1]=="規律"),
                              which(DM.wc.mei[,1]=="校則")))
merg.mei[[102]] <- list(name="論理",
                        num=c(which(DM.wc.mei[,1]=="論理"),
                              which(DM.wc.mei[,1]=="ロジック")))
merg.mei[[103]] <- list(name="医療",
                        num=c(which(DM.wc.mei[,1]=="医者"),
                              which(DM.wc.mei[,1]=="医療"),
                              which(DM.wc.mei[,1]=="病院")))
merg.mei[[104]] <- list(name="下手",
                        num=c(which(DM.wc.mei[,1]=="下手"),
                              which(DM.wc.mei[,1]=="下手糞")))
merg.mei[[105]] <- list(name="願い",
                        num=c(which(DM.wc.mei[,1]=="願い"),
                              which(DM.wc.mei[,1]=="願望")))
merg.mei[[106]] <- list(name="企業.会社",
                        num=c(which(DM.wc.mei[,1]=="企業"),
                              which(DM.wc.mei[,1]=="会社")))
merg.mei[[107]] <- list(name="基本",
                        num=c(which(DM.wc.mei[,1]=="基本"),
                              which(DM.wc.mei[,1]=="基盤"),
                              which(DM.wc.mei[,1]=="基礎")))
merg.mei[[108]] <- list(name="元気.気力",
                        num=c(which(DM.wc.mei[,1]=="気合い"),
                              which(DM.wc.mei[,1]=="気力"),
                              which(DM.wc.mei[,1]=="元気")))
merg.mei[[109]] <- list(name="休み",
                        num=c(which(DM.wc.mei[,1]=="休み"),
                              which(DM.wc.mei[,1]=="休日"),
                              which(DM.wc.mei[,1]=="夏休み"),
                              which(DM.wc.mei[,1]=="春休み")))
merg.mei[[110]] <- list(name="ギャンブル",
                        num=c(which(DM.wc.mei[,1]=="ギャンブル"),
                              which(DM.wc.mei[,1]=="競艇"),
                              which(DM.wc.mei[,1]=="競輪"),
                              which(DM.wc.mei[,1]=="競馬")))
merg.mei[[111]] <- list(name="教員.教授",
                        num=c(which(DM.wc.mei[,1]=="教員"),
                              which(DM.wc.mei[,1]=="教授"),
                              which(DM.wc.mei[,1]=="教師"),
                              which(DM.wc.mei[,1]=="チューター")))
merg.mei[[112]] <- list(name="興味",
                        num=c(which(DM.wc.mei[,1]=="興味"),
                              which(DM.wc.mei[,1]=="興味津々")))
merg.mei[[113]] <- list(name="嫌い",
                        num=c(which(DM.wc.mei[,1]=="嫌い"),
                              which(DM.wc.mei[,1]=="嫌"),
                              which(DM.wc.mei[,1]=="嫌々")))
merg.mei[[114]] <- list(name="今",
                        num=c(which(DM.wc.mei[,1]=="現在"),
                              which(DM.wc.mei[,1]=="現時点"),
                              which(DM.wc.mei[,1]=="今")))
merg.mei[[115]] <- list(name="ネガティブ",
                        num=c(which(DM.wc.mei[,1]=="後ろ向き"),
                              which(DM.wc.mei[,1]=="ネガティブ"),
                              which(DM.wc.mei[,1]=="悲観")))
merg.mei[[116]] <- list(name="語学",
                        num=c(which(DM.wc.mei[,1]=="語学"),
                              which(DM.wc.mei[,1]=="フランス語"),
                              which(DM.wc.mei[,1]=="英語")))
merg.mei[[117]] <- list(name="自問",
                        num=c(which(DM.wc.mei[,1]=="自問"),
                              which(DM.wc.mei[,1]=="自問自答")))
merg.mei[[118]] <- list(name="小中学校.小中学生",
                        num=c(which(DM.wc.mei[,1]=="小３"),
                              which(DM.wc.mei[,1]=="小学"),
                              which(DM.wc.mei[,1]=="小学生"),
                              which(DM.wc.mei[,1]=="小学校"),
                              which(DM.wc.mei[,1]=="中学"),
                              which(DM.wc.mei[,1]=="中学生"),
                              which(DM.wc.mei[,1]=="中学校"),
                              which(DM.wc.mei[,1]=="小中学校"),
                              which(DM.wc.mei[,1]=="小中学生")))
merg.mei[[119]] <- list(name="高校.高校生",
                        num=c(which(DM.wc.mei[,1]=="高校"),
                              which(DM.wc.mei[,1]=="高校生"),
                              which(DM.wc.mei[,1]=="女子高"),
                              which(DM.wc.mei[,1]=="男子校")))
merg.mei[[120]] <- list(name="笑",
                        num=c(which(DM.wc.mei[,1]=="笑"),which(DM.wc.mei[,1]=="笑い")))
merg.mei[[121]] <- list(name="障害.障壁",
                        num=c(which(DM.wc.mei[,1]=="障害"),which(DM.wc.mei[,1]=="障壁")))
merg.mei[[122]] <- list(name="全力",
                        num=c(which(DM.wc.mei[,1]=="全力"),which(DM.wc.mei[,1]=="全身全霊")))
merg.mei[[123]] <- list(name="好き",
                        num=c(which(DM.wc.mei[,1]=="好き"),which(DM.wc.mei[,1]=="大好き")))
merg.mei[[124]] <- list(name="震災.被災",
                        num=c(which(DM.wc.mei[,1]=="震災"),which(DM.wc.mei[,1]=="大震災"),
                              which(DM.wc.mei[,1]=="被災")))
merg.mei[[125]] <- list(name="前提",
                        num=c(which(DM.wc.mei[,1]=="大前提"),which(DM.wc.mei[,1]=="前提")))
merg.mei[[126]] <- list(name="発見",
                        num=c(which(DM.wc.mei[,1]=="大発見"),which(DM.wc.mei[,1]=="発見")))
merg.mei[[127]] <- list(name="提言.提唱",
                        num=c(which(DM.wc.mei[,1]=="提言"),which(DM.wc.mei[,1]=="提唱")))
merg.mei[[128]] <- list(name="都市",
                        num=c(which(DM.wc.mei[,1]=="大都市"),which(DM.wc.mei[,1]=="都市"),
                              which(DM.wc.mei[,1]=="都内"),which(DM.wc.mei[,1]=="都会"),
                              which(DM.wc.mei[,1]=="都心")))
merg.mei[[129]] <- list(name="働き",
                        num=c(which(DM.wc.mei[,1]=="働き"),which(DM.wc.mei[,1]=="働")))
merg.mei[[130]] <- list(name="動き",
                        num=c(which(DM.wc.mei[,1]=="動き"),which(DM.wc.mei[,1]=="動")))
merg.mei[[131]] <- list(name="動画",
                        num=c(which(DM.wc.mei[,1]=="動画"),which(DM.wc.mei[,1]=="Ustream"),
                              which(DM.wc.mei[,1]=="YouTube")))
merg.mei[[132]] <- list(name="新聞",
                        num=c(which(DM.wc.mei[,1]=="新聞"),which(DM.wc.mei[,1]=="読売新聞")))
merg.mei[[133]] <- list(name="我慢.忍耐",
                        num=c(which(DM.wc.mei[,1]=="我慢"),which(DM.wc.mei[,1]=="忍耐")))
merg.mei[[134]] <- list(name="服",
                        num=c(which(DM.wc.mei[,1]=="服"),which(DM.wc.mei[,1]=="服飾"),
                              which(DM.wc.mei[,1]=="服装")))
merg.mei[[135]] <- list(name="介護.福祉",
                        num=c(which(DM.wc.mei[,1]=="福祉"),which(DM.wc.mei[,1]=="介護")))
merg.mei[[136]] <- list(name="無理",
                        num=c(which(DM.wc.mei[,1]=="無理"),which(DM.wc.mei[,1]=="無理矢理")))
merg.mei[[137]] <- list(name="お客",num=c(which(DM.wc.mei[,1]=="お客"),which(DM.wc.mei[,1]=="お客様"),
                                       which(DM.wc.mei[,1]=="顧客")),which(DM.wc.mei[,1]=="クライアント"))
merg.mei[[138]] <- list(name="母親",num=c(which(DM.wc.mei[,1]=="母親"),which(DM.wc.mei[,1]=="お母さん"),which(DM.wc.mei[,1]=="母")))
merg.mei[[139]] <- list(name="父親",num=c(which(DM.wc.mei[,1]=="父親"),which(DM.wc.mei[,1]=="お父さん"),which(DM.wc.mei[,1]=="父"),
                                       which(DM.wc.mei[,1]=="親父"),which(DM.wc.mei[,1]=="父さん")))
merg.mei[[140]] <- list(name="祖母",num=c(which(DM.wc.mei[,1]=="祖母"),which(DM.wc.mei[,1]=="ばあちゃん")))
merg.mei[[141]] <- list(name="祖父",num=c(which(DM.wc.mei[,1]=="祖父"),which(DM.wc.mei[,1]=="おじいちゃん")))
merg.mei[[142]] <- list(name="ぱみゅぱみゅ",num=c(which(DM.wc.mei[,1]=="ぱみゅぱみゅ"),which(DM.wc.mei[,1]=="ぱみゅぱみゅに")))
merg.mei[[143]] <- list(name="一緒",num=c(which(DM.wc.mei[,1]=="一緒"),which(DM.wc.mei[,1]=="いっしょ")))
merg.mei[[144]] <- list(name="瓦礫",num=c(which(DM.wc.mei[,1]=="瓦礫"),which(DM.wc.mei[,1]=="がれき")))
merg.mei[[145]] <- list(name="言葉",num=c(which(DM.wc.mei[,1]=="言葉"),which(DM.wc.mei[,1]=="ことば")))
merg.mei[[146]] <- list(name="子供",num=c(which(DM.wc.mei[,1]=="子供"),which(DM.wc.mei[,1]=="子ども"),which(DM.wc.mei[,1]=="こども")))
merg.mei[[147]] <- list(name="一人",num=c(which(DM.wc.mei[,1]=="一人"),which(DM.wc.mei[,1]=="ひとり")))
merg.mei[[148]] <- list(name="普通",num=c(which(DM.wc.mei[,1]=="普通"),which(DM.wc.mei[,1]=="ふつう")))
merg.mei[[149]] <- list(name="本当",num=c(which(DM.wc.mei[,1]=="本当"),which(DM.wc.mei[,1]=="ほんま")))
merg.mei[[150]] <- list(name="Girlpedia",num=c(which(DM.wc.mei[,1]=="Girlpedia"),which(DM.wc.mei[,1]=="ガルペディア")))
merg.mei[[151]] <- list(name="教科書.教材",num=c(which(DM.wc.mei[,1]=="教科書"),which(DM.wc.mei[,1]=="教本"),which(DM.wc.mei[,1]=="教材")))
merg.mei[[152]] <- list(name="最後",num=c(which(DM.wc.mei[,1]=="最後"),which(DM.wc.mei[,1]=="最終")))
merg.mei[[153]] <- list(name="叔父",num=c(which(DM.wc.mei[,1]=="叔父"),which(DM.wc.mei[,1]=="おじ")))
merg.mei[[154]] <- list(name="女性",num=c(which(DM.wc.mei[,1]=="女"),which(DM.wc.mei[,1]=="女の子"),
                                       which(DM.wc.mei[,1]=="女子"),which(DM.wc.mei[,1]=="女性"),
                                       which(DM.wc.mei[,1]=="ガール"),which(DM.wc.mei[,1]=="Girls")))
merg.mei[[155]] <- list(name="男性",num=c(which(DM.wc.mei[,1]=="男"),which(DM.wc.mei[,1]=="男の子"),which(DM.wc.mei[,1]=="男子"),which(DM.wc.mei[,1]=="男性")))
merg.mei[[156]] <- list(name="知人",num=c(which(DM.wc.mei[,1]=="知人"),which(DM.wc.mei[,1]=="知り合い")))
merg.mei[[157]] <- list(name="特徴",num=c(which(DM.wc.mei[,1]=="特徴"),which(DM.wc.mei[,1]=="特質")))
merg.mei[[158]] <- list(name="売上",num=c(which(DM.wc.mei[,1]=="売上"),which(DM.wc.mei[,1]=="売り上げ")))
merg.mei[[159]] <- list(name="名前",num=c(which(DM.wc.mei[,1]=="名前"),which(DM.wc.mei[,1]=="名")))
merg.mei[[160]] <- list(name="問い",num=c(which(DM.wc.mei[,1]=="問い"),which(DM.wc.mei[,1]=="問")))
merg.mei[[161]] <- list(name="友達",num=c(which(DM.wc.mei[,1]=="友達"),which(DM.wc.mei[,1]=="友だち"),
                                       which(DM.wc.mei[,1]=="友人"),which(DM.wc.mei[,1]=="クラスメイト"),
                                       which(DM.wc.mei[,1]=="チームメイト"),which(DM.wc.mei[,1]=="仲間")))
merg.mei[[162]] <- list(name="カ月",num=c(which(DM.wc.mei[,1]=="カ月"),which(DM.wc.mei[,1]=="ヵ月"),which(DM.wc.mei[,1]=="か月")))
merg.mei[[163]] <- list(name="私",num=c(which(DM.wc.mei[,1]=="おれ"),which(DM.wc.mei[,1]=="オレ"),which(DM.wc.mei[,1]=="俺"),
                                     which(DM.wc.mei[,1]=="ぼく"),which(DM.wc.mei[,1]=="僕"),
                                     which(DM.wc.mei[,1]=="わたし"),which(DM.wc.mei[,1]=="私")))
merg.mei[[164]] <- list(name="あなた",num=c(which(DM.wc.mei[,1]=="あなた"),which(DM.wc.mei[,1]=="お前")))
merg.mei[[165]] <- list(name="私達",num=c(which(DM.wc.mei[,1]=="僕たち"),which(DM.wc.mei[,1]=="僕達"),which(DM.wc.mei[,1]=="僕ら")))
merg.mei[[166]] <- list(name="皆",num=c(which(DM.wc.mei[,1]=="みなさん"),which(DM.wc.mei[,1]=="皆"),
                                      which(DM.wc.mei[,1]=="みんな"),which(DM.wc.mei[,1]=="みな"),
                                      which(DM.wc.mei[,1]=="皆さん"),which(DM.wc.mei[,1]=="老若男女")))
merg.mei[[167]] <- list(name="誰",num=c(which(DM.wc.mei[,1]=="だれ"),which(DM.wc.mei[,1]=="誰")))
merg.mei[[168]] <- list(name="奴",num=c(which(DM.wc.mei[,1]=="やつ"),which(DM.wc.mei[,1]=="奴"),which(DM.wc.mei[,1]=="あいつ")))
merg.mei[[169]] <- list(name="こちら",num=c(which(DM.wc.mei[,1]=="こっち"),which(DM.wc.mei[,1]=="こちら")))
merg.mei[[170]] <- list(name="どちら",num=c(which(DM.wc.mei[,1]=="どっち"),which(DM.wc.mei[,1]=="どちら")))
merg.mei[[171]] <- list(name="そちら",num=c(which(DM.wc.mei[,1]=="そっち"),which(DM.wc.mei[,1]=="そちら")))

merg.mei[[172]] <- list(name="今",num=c(which(DM.wc.mei[,1]=="いま"),which(DM.wc.mei[,1]=="今")))
merg.mei[[173]] <- list(name="全て",num=c(which(DM.wc.mei[,1]=="すべて"),which(DM.wc.mei[,1]=="全て")))
merg.mei[[174]] <- list(name="沢山",num=c(which(DM.wc.mei[,1]=="たくさん"),which(DM.wc.mei[,1]=="沢山")))
merg.mei[[175]] <- list(name="他",num=c(which(DM.wc.mei[,1]=="ほか"),which(DM.wc.mei[,1]=="他")))

#分断している語を抽出
merg.mei[[176]] <- list(name="TABLE FOR TWO",num=c(which(DM.wc.mei[,1]=="TABLE"),which(DM.wc.mei[,1]=="FOR"),
                                                  which(DM.wc.mei[,1]=="TWO")))

#エラーを招く文字を修正
merg.mei[[177]] <- list(name="TEDSendai",num=c(which(DM.wc.mei[,1]=="TED×Sendai")))
merg.mei[[178]] <- list(name="TEDUTokyo",num=c(which(DM.wc.mei[,1]=="TEDxUTokyo")))

#for(i in 1:length(merg.mei)) {
#  print(paste(i,length(merg.mei[[i]]$num)))
#}

add.wc <- NULL
for(i in 1:length(merg.mei)) {
  add.wc[[i]] <- c(merg.mei[[i]]$name,"名詞","追加",rep(0,ncol(DM.wc.mei)-3))
  delete4.num <- c(delete4.num,merg.mei[[i]]$num)
  for(j in merg.mei[[i]]$num) {
    add.wc[[i]][4:length(add.wc[[i]])] <- as.numeric(add.wc[[i]][4:length(add.wc[[i]])]) + DM.wc.mei[j,4:length(add.wc[[i]])]
  }
  DM.wc.mei <- rbind(DM.wc.mei,add.wc[[i]])
  print(paste("i =",i,"/",length(merg.mei),"is done"))
}

delete4.num <- sort(delete4.num)
delete4 <- DM.wc.mei[delete4.num,1]

#不要な語と重複している語を消す
DM.wc.mei <- DM.wc.mei[-delete4.num,]

####2.3 結果をまとめる####
DM.wc.new <- rbind(DM.wc.kei,DM.wc.dou,DM.wc.mei)

###最後に,タームが同じで重複している行をまとめる
#後の処理で重複は許されないため
dup.words <- NULL
dup.num <- length(names(table(DM.wc.new[,1]))[table(DM.wc.new[,1])>=2])

#重複している語の抽出
for(i in 1:dup.num) {
  name <- names(table(DM.wc.new[,1]))[table(DM.wc.new[,1])>=2][i]
  hinshi1 <- DM.wc.new[DM.wc.new[,1]==name,2]
  hinshi2 <- DM.wc.new[DM.wc.new[,1]==name,3]
  num <- which(DM.wc.new[,1]==name)
  dup.words[[i]] <- list(name=name,hinshi1=hinshi1,hinshi2=hinshi2,num=num)
  print(paste("i =",i,"/",dup.num,"is done"))
}

#抽出結果の確認
dup.words

#重複している行を合計した行を追加する
add.wc <- NULL
delete5.num <- NULL

for(i in 1:length(dup.words)) {
  add.wc[[i]] <- c(dup.words[[i]]$name,dup.words[[i]]$hinshi1[1],"追加",rep(0,ncol(DM.wc.mei)-3))
  delete5.num <- c(delete5.num,dup.words[[i]]$num)
  for(j in dup.words[[i]]$num) {
    add.wc[[i]][4:length(add.wc[[i]])] <- as.numeric(add.wc[[i]][4:length(add.wc[[i]])]) + DM.wc.new[j,4:length(add.wc[[i]])]
  }
  DM.wc.new <- rbind(DM.wc.new,add.wc[[i]])
  print(paste("i =",i,"/",length(dup.words),"is done"))
}

#重複している行を削除する
delete5 <- DM.wc.new[delete5.num,1]
DM.wc.new <- DM.wc.new[-delete5.num,]
dim(DM.wc.new) #5372x89の行列

####3.Wordcloudによる可視化####
#色の設定
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]

setwd("./2.1_Result/2.1.1_Wordcloud/")

#Wordcloudを出力
for(i in 4:ncol(DM.wc.new)) {
  png(file=paste(colnames(DM.wc.new)[i],".png",sep=""))
  wordcloud(words=DM.wc.new[,1],freq=DM.wc.new[,i],min.freq=2,
            max.words=Inf,random.order=TRUE,random.color=FALSE,
            rot.per=.0,colors=pal,main=colnames(DM.wc.new)[i])
  text(0.5,1,colnames(DM.wc.new)[i])
  dev.off()
  print(paste(colnames(DM.wc.new)[i],"is done"))
}
#windowsフォトビューアーで"2.1.1_Wordcloud"の結果を確認,頻度が高く(文字が大きく)興味の無い語を消す
delete6 <- c("する","いる","こと","られる","れる","せる",
             "ある","てる","はず","なる","これ","もの","なる",
             "これ","それ","そこ","思う","とき","ない","時","自分","私",
             "とれる","とこ","日","彼ら")
DM.wc.new <- DM.wc.new[!DM.wc.new[,1] %in% delete6,]

setwd("../")
setwd("./2.1.2_Wordcloud_revised/")

#再度Wordcloudを出力
for(i in 4:ncol(DM.wc.new)) {
  png(file=paste(colnames(DM.wc.new)[i],"2.png",sep=""))
  wordcloud(words=DM.wc.new[,1],freq=DM.wc.new[,i],min.freq=2,
            max.words=Inf,random.order=TRUE,random.color=FALSE,
            rot.per=.0,colors=pal,main=colnames(DM.wc.new)[i])
  text(0.5,1,paste(colnames(DM.wc.new)[i],2,sep=""))
  dev.off()
  print(paste(colnames(DM.wc.new)[i],"is done"))
}
#2.1.2_Wordcloud_revisedフォルダで出力結果を確認

setwd("../")
setwd("../")

#頻度をTF*IDF法で変換
IDF <- rep(0,nrow(DM.wc.new))
DM.new <- DM.wc.new

for(i in 1:nrow(DM.wc.new)) {
  IDF[i] <- log2(ncol(DM.wc.new)/length(which(DM.wc.new[i,4:ncol(DM.wc.new)]>0)))+1
  DM.new[i,4:ncol(DM.new)] <- DM.wc.new[i,4:ncol(DM.new)]*IDF[i]
  print(paste("i =",i,"/",nrow(DM.wc.new),"is done"))
}
#TF*IDF法の式は,「Rによるテキストマイニング入門」p.71

#正規化(各列の2乗和が1となるよう変換すること)
#(文書の長さが異なる影響を除去するため)
for(i in 4:ncol(DM.new)) {
  DM.new[,i] <- DM.new[,i]/sqrt(sum((DM.new[,i])^2))
  print(paste("i =",i,"/",ncol(DM.new),"is done"))
}

#結果を出力
write.table(DM.new,file="2.2_DM.csv",sep=",")
