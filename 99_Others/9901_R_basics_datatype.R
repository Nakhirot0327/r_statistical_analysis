##########1. データフレームの入力,行列の四則演算#############
####1.1 データフレームの入力####
x1 <- data.frame(hikki=c(68,85,50,54,66),mensetsu=c(65,80,95,70,75))
#ここでは,データフレームとは行列のことと考えれば十分です(厳密には異なる)
#上記はhikkiという列に68,85,50,54,66、mensetsuという列に65,80,95,70,75
#を代入し,行列化(データフレーム化)しています
#cはcombine(コンバイン,結合)の意味です

x2 <- data.frame(hikki=c(35,56,25,43,70),mensetsu=c(55,65,75,50,40))

####1.2 行に名前を付ける####
rownames(x1) <- c("A","B","C","D","E")
rownames(x2) <- c("A","B","C","D","E")

####1.3 入力結果の確認####
x1 #データ全体
x2 #データ全体
colnames(x1) #列名
rownames(x1) #行名
x1[2,5] #2行目・5列目の要素
#エクセルのように,データ全体を確認したり編集したいときは
#edit(x1)と入力すると編集画面が開く

####1.4 データフレームを行列化する####
x1 <- as.matrix(x1)
x2 <- as.matrix(x2)
#データフレームは,要素に文字型(character)や因子型(factor)が許される行列
#です。行列の演算を行う場合は,データフレームを行列に変換する必要が
#あります。もし,データフレームに文字型が存在する場合は,行列に
#変換することは出来ません。

####1.5 行列のサイズを確認する####
dim(x1) #行数と列数が表示される
dim(x2) #行数と列数が表示される
nrow(x1) #行数が表示される
ncol(x2) #列数が表示される 
length(x1) #要素数(行数×列数)が表示される

####1.6 要素ごとの四則演算####
x1 + x2 #x1とx2の各々の和が計算される
x1 - x2 #x1とx2の各々の差が計算される
x1 * x2 #x1とx2の各々の積が計算される
x1 / x2 #x1とx2の各々の商が計算される

####1.7 転置行列(行と列を逆転した行列)####
t(x1)

####1.8 行列の積の計算####
x1 %*% x2 #エラー(x1の列数とx2の行数が一致していないため)
t(x1) %*% x2 #2x5の行列と5x2の行列の積なので,結果は2x2

##########2.データの加工#############
#データの入力
A <- data.frame(ID=c("a","a","a","b","b","b","c","c","c"),mz=c(1:9))
A #列名は"ID","mz"
B <- data.frame(ID=c("b","c","d"),Risk=c(1:3))
B #列名は"ID","Risk"

####2.1 テーブルのマージ(VLOOKUP)####
table1 <- merge(B,A,by="ID")
table1
#エクセルで言うVLOOKUPに当たる処理が上記です。
#aというデータフレームとbというデータフレームを
#"ID"をkeyとして合併(merge)しています。
#bに対してaをmergeしていることに注意して下さい。
table2 <- merge(A,B,by="ID")
table2
#このようにaとbを逆にすれば,aに対してbをmergeしたことになります。

table1 <- merge(B,A,by="ID",all=TRUE)
table1
#今度は,merge()関数使用時にall=TRUEという指定をしました。
#A,Bの両方に含まれないID="a","d"の行が残っていることがわかります。
#このように引数allはTRUEにすると,両方に共通でない行も残すことが
#出来ます。何も付記しない場合はall=FALSEが前提です。

####2.2 不要な行/列を削除####
#RiskがNAである,8-10行目を削除してみましょう。
table1 <- table1[-(8:10),] #8-10行目を削除
table1
#table[x,y]でtableという行列のx行y列の要素を意味します。
#[]の中身にマイナス("-")を付して表現すると,該当する行/列を削除出来ます。
#なお,8:10は8,9,10という数列を意味します。

####2.3 欠損値(NA)を0で置き換える####
table1
#table1の7,3成分は"NA"です。
#このNAを0で置き換えてみましょう。
table1$mz <- replace(x=table1$mz,list=which(is.na(table1$mz)),values=0)
table1
#replace関数は次のように作用しています。
#x=table1$mz:NAがある列を指定
#list=which(is.na(table1$mz)):何番目の値を置換するのかを指定
#is.na(table1$mz):NAかどうかをTRUE/FALSEで返します
#which():()内にTRUEが何番目にあるかを示します
#values=0:listで抽出した値を何に置き換えるのかを示します
#上記は,欠損値(NA)の場合を扱いましたが,
#NULLやNanの場合もis.na()の代わりにis.null(),is.nan()を使えば同様に出来ます

####2.4 一部を抽出####
table1
#今度は"mz"が5以下の行のみ抽出してみましょう。
table1 <- subset(table1,table1$mz <= 5) #subsetで一部を抽出。A$BはAというデータのB列、という意味
table1

####2.5 降順,昇順の並び替え####
table2
#mzの列を基準にして降順,昇順に並び替えをしてみましょう。
table2 <- table2[order(-table2$mz),] #降順
table2
table2 <- table2[order(table2$mz),] #昇順
table2

####2.6 列/行の結合####
table2
#table2に新しい行や列を追加してみましょう。
#まずは行の追加
ad <- c("a",10,3)
table2 <- rbind(table2,ad)
table2
#adの要素は順に文字,数値,数値としています。
#tabl2の"ID","mz","Risk"が文字,数値,数値となっているからです。
#型が適合しない場合はエラーとなるので注意しましょう。

#次は列の追加
adc <- c(1,2,1,1,2,2,3)
table2 <- cbind(table2,adc)
#列が追加されました。

##########3.データの集計#############
library(vcd) #パッケージの呼び出し

#Rでは,R本体のインストールと同時に,様々なサンプルデータ集が
#無償で使用出来,自習する際に大いに役に立ちます。
#そのうちの1つを試しに使用してみましょう。

####3.1 サンプルデータの呼び出し####
data(DanishWelfare) #サンプルデータDanishWelfareを呼び出す

DanishWelfare #180行あり,上の方が隠れてしまい列名がわからない
head(DanishWelfare) #データの上位6行を表示
#Freqは頻度,Alcoholはアルコールの摂取量,Incomeは収入,
#Statusは未亡人(widow),未婚(Unmarried),既婚(married)の分類
#UrbanはCopenhagen,SubCopenhagen,LargeCity,Cityの分類

#データの型の確認
dim(DanishWelfare) #180行×5列のデータ
class(DanishWelfare) #データフレームとわかる

DanishWelfare$Freq #Freqの列だけ抽出
class(DanishWelfare$Freq) #数値型(numeric)とわかる

#他の列も同様に…
class(DanishWelfare$Alcohol) #因子型(factor)
class(DanishWelfare$Income) #因子型(factor)
class(DanishWelfare$Status) #因子型(factor)
class(DanishWelfare$Urban) #因子型(factor)

####3.2 簡単な集計####
#各列毎の集計を求める場合はsummary関数を使用
summary(DanishWelfare)
#Minは最小値です。1st Qu.は小さい方から数えて25%の点を指します。
#Medianは中央値(小さい方から数えて50%の点),Meanは平均値,
#3rd Qu.は小さい方から数えて75%の点,Max.は最大値です。

####3.3 テーブルデータの集計####
tabledata <- xtabs(Freq~Alcohol+Income,data=DanishWelfare) #集計データをテーブル形式に
tabledata
#上記では,xtabsによりFreq(頻度)をAlcoholとIncome毎に集計しています。
#Alchol,Incomeの部分をStatusやUrbanに変えて結果を確かめてみて下さい。

####3.4 ピボットテーブルの作成####
#エクセルのピボットテーブルのような集計も出来ます。
library(reshape2) #パッケージの呼び出し
acast(melt(DanishWelfare, id.var=c("Alcohol","Income","Status","Urban"),
           measure.var="Freq"),Urban + Status ~ Income, 
      sum, margins = FALSE)
#上記では,melt()関数で,DanishWelfareのデータ構造を分解しています。
#id.var=…で呼び出す列名を指定し,measure.var=…で集計する項目を指定しています。
#Urban+Status~Incomeはテーブルの構造を定義しています。
#結果を見ると,UrbanとStatus毎,Income毎に頻度(Freq)が集計されている
#ことがわかります。
#sumをmeanに変えると平均が計算されます。
#margins=TRUEとすると,小計が算出されます。

##########4.基本的な図示#############
#ここでは,別のサンプルデータを用いて,簡単な図示について学びます。
data(tips) #データtipsの呼び出し
head(tips)
#データtipsはあるレストランの売り上げについてのサンプルデータです。
#会計ごとに集計されており,
#total_billは合計,tipはチップ額,sexは性別,smokerは喫煙者か否か,
#dayは曜日,timeは時間帯,sizeは人数です。

dim(tips) #244行×7列

####4.1 ヒストグラム####
head(tips)
#total_bill,tip,sizeは数値データですからヒストグラムが
#描けるはずです。hist()関数を使って描いてみましょう。
hist(tips$total_bill)
hist(tips$tip)
hist(tips$size)

####4.2 箱ひげ図####
#分布をざっくりと確認したいときはboxplotが便利です。
boxplot(x=tips$total_bill)
boxplot(x=tips$tip)
boxplot(x=tips$size)
#箱から"ひげ"が伸びている形の図が表示されます。
#一番下の線は最小値(外れ値を除く)です。
#長方形の底辺は小さい方から数えて25%の点(Q1),
#長方形の中央の太線は小さい方から数えて50%の点,
#長方形の上辺は小さい方から数えて75%の点(Q3),
#一番上の線は最大値(外れ値を除く)です。
#○は外れ値を示します。外れ値の基準は
#Q1-(Q3-Q1)*1.5からQ1+(Q3-Q1)*1.5の間に入っているかどうか
#で判定されます。(設定によって変更が可能(今回は省略))

####4.2 円グラフ####
library(graphics)
head(tips)
#sex,smoker,day,timeは因子型(factor)データですから
#円グラフを描いて,内訳を集計してみましょう。
#table()関数で集計した後にpie()関数で図示します。
pie(table(tips$sex))
pie(table(tips$smoker))
pie(table(tips$day))
pie(table(tips$time))

####4.3 モザイクプロット####
head(tips)
#因子型(factor)データであるsex,smoker,day,timeは
#モザイクプロット関数で構造を一覧することが出来ます。
mosaicplot(~sex+smoker+day+time,data=tips,color=TRUE)

####4.5 プロット####
#total_bill,tip,sizeは数値データですからプロットを描くことが出来ます。
#2変数
plot(x=tips$total_bill,y=tips$tip)
#x,yを異なる変数に変えて試してみて下さい。

#多変数
#paris()関数を使用すると,多変数のプロットも確認できます。
#全体を俯瞰したいときは便利です。
pairs(tips)
