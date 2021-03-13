########################################################################
###1.3.1.国勢調査結果を用いたハンバーガーチェーンの出店傾向分析(準備)###
########################################################################
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/"
setwd(current_directory)

setwd("./1_GIS_data_analysis/")
library(spgwr)
library(maptools)
library(MASS)
library(RColorBrewer)
library(classInt)
#install.packages("gplots") #初回のみ
library(gplots)

####1.地図データの入力####
map.spdf <- readShapeSpatial("japan_ver71.shp",proj4string=CRS("+proj=longlat +datum=WGS84"))
str(map.spdf,max.level=2) #データ構造
head(map.spdf@data) #各市町村のデータ
length(map.spdf@polygons) #1902
dim(map.spdf@data) #1902x9

####2.チェーンの店舗データの入力・集計####
###内容は1.3.1_Shops_comparison.Rと概ね同じ(理解の容易さを優先し再掲)###
##A社のデータ整形
mc <- read.csv(file="1.1_ShopGeometry_mc.csv",header=TRUE)
mc[,2] <- as.numeric(as.character(mc[,2]))
mc[,3] <- as.numeric(as.character(mc[,3]))
mc <- subset(mc,!is.na(mc[,2])&!is.na(mc[,3])) #今回は欠損値を削除する
MC <- mc[,2:3] #緯度,経度を抽出
MC <- cbind(MC[,2],MC[,1]) #経度,緯度の順番にする
colnames(MC) <- c("longitude","latitude")
cnt <- rep(1,nrow(MC)) #店舗数のカウント(もし情報があれば,売上等でも良い)
MC <- cbind(MC,"CNT"=cnt) #cntとMCを列で統合
MC.spdf <- SpatialPointsDataFrame(SpatialPoints(MC[,1:2]),data=data.frame(MC[,3]),
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))

##B社のデータ整形
kfc <- read.csv(file="1.1_ShopGeometry_kfc.csv",header=FALSE,sep=",") #本番はTRUE
for(i in 1:ncol(kfc)) {
  print(class(kfc[,i]))
}
#edit(kfc) #下方までスクロールすると8行目にバグがあるとわかる
kfc <- kfc[,-8]
colnames(kfc) <- c("ID","longitude","latitude","S_name","Postal","Address","Tel")
head(kfc)
KFC <- cbind("longitude"=kfc[,3],"latitude"=kfc[,2]) #経度,緯度の順番にする
cnt <- rep(1,nrow(KFC)) #店舗数のカウント
KFC <- cbind(KFC,"CNT"=cnt) #cntとKFCを列で統合
KFC.spdf <- SpatialPointsDataFrame(SpatialPoints(KFC[,1:2]),data=data.frame(KFC[,3]),
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

##C社のデータ整形
mos <- read.csv(file="1.1_ShopGeometry_mos.csv",header=TRUE,dec=".")
dim(mos)
#edit(mos) #下方までスクロールすると,ずれている列があるとわかる
mos <- mos[-1,] #中島要確認

for(i in 1:ncol(mos)) {
  print(class(mos[,i]))
}

mos[,1] <- as.numeric(as.character(mos[,1]))
mos[,2] <- as.numeric(as.character(mos[,2]))

#edit(mos)
#editで確認した結果を踏まえ,データの整形
for(i in 2:(nrow(mos))) {
  if(is.na(mos[i,1])&is.na(mos[i,2])) {
    if(mos[i,3]==""&mos[i,4]=="") {
      mos[i,1] <- as.numeric(as.character(mos[i-1,5]))
      mos[i,2] <- as.numeric(as.character(rownames(mos)[i]))
    } else {
      mos[i,1] <- as.numeric(as.character(mos[i-1,5]))
      mos[i,2] <- as.numeric(as.character(rownames(mos)[i]))
      mos[i+1,1] <- as.numeric(as.character(mos[i,3]))
      mos[i+1,2] <- as.numeric(as.character(mos[i,4]))
    }
  }
}
#edit(mos) #経度・緯度は修正された
mos <- mos[,-5] #5列目は削除
head(mos,1)
colnames(mos) <- c("latitude","longitude","shopname","address")
MOS <- mos[,1:2] #緯度,経度を抽出
MOS <- cbind("longitude"=MOS[,2],"latitude"=MOS[,1]) #経度,緯度の順番にする
cnt <- rep(1,nrow(MOS)) #店舗数のカウント(もし情報があれば,売上等でも良い)
MOS <- cbind(MOS,"CNT"=cnt) #cntとMOSを列で統合
MOS.spdf <- SpatialPointsDataFrame(SpatialPoints(MOS[,1:2]),data=data.frame(MOS[,3]),
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

#第1引数:SpatialPointsDataFrame,第2引数:SpatialPolygonsDataFrame,第3引数:sum等の関数
overlay.integrate <- function(spdf,map,fn) {
  res <- overlay(spdf,map,fn=fn)
  id <- row.names(res)
  res <- cbind("CNT"=res[,1],"ID"=id)
  res <- as.data.frame(res)
  res[,"ID"] <- as.numeric(as.character(res[,"ID"])) #map.spdf@dataとIDを合わせる
  res[,"CNT"] <- as.numeric(as.character(res[,"CNT"]))
  
  #resは店舗が無い市区町村の行が不足しているため,追加
  ad <- matrix(rep(0,2),ncol=2)
  ad <- as.data.frame(ad)
  colnames(ad) <- c("CNT","ID")
  for(i in 1:(nrow(map@data))) {
    if(nrow(res[res[,"ID"]==i,]) == 0) {
      ad[1,2] <- i
      res <- rbind(res,ad)
    }
  }
  res <- res[order(res$ID),] #id順に並び替え
  row.names(res) <- res$ID #後ほどの処理に備え,行名を変更  
  #市区町村別データ(res)を地図のデータ(map@data)に統合  
  map@data <- cbind(map@data,"CNT"=res[,"CNT"])
  return(map)
}

#関数を使用
map.spdf <- overlay.integrate(spdf=MC.spdf,map=map.spdf,fn=sum) #位置が重複している店舗は警告が表示される
map.spdf <- overlay.integrate(spdf=KFC.spdf,map=map.spdf,fn=sum)#位置が重複している店舗は警告が表示される
map.spdf <- overlay.integrate(spdf=MOS.spdf,map=map.spdf,fn=sum)#位置が重複している店舗は警告が表示される
head(map.spdf@data)
colnames(map.spdf@data)[(ncol(map.spdf@data)-2):(ncol(map.spdf@data))] <- c("MC_CNT","KFC_CNT","MOS_CNT")

length(map.spdf@polygons) #1902
dim(map.spdf@data) #1902x12
#edit(map.spdf@data)

####3.統計データの入力・整形####
#平成22年国勢調査(総務省統計局)を使用
dat <- read.csv("1.3_MIC_population_stat.csv",header=TRUE,dec=".")

#データ型の確認
for(i in 1:ncol(dat)) {
  print(paste(i,colnames(dat)[i],class(dat[,i])))
}
#データの定義は:http://www.stat.go.jp/data/kokusei/2010/users-g/word.htm

#edit(dat) #データを確認(別ウィンドウが開きます)
#Rstudioの場合,文字化けするが正しく認識はされている

#39列目:外国人数をintegerに変換
#42列目:施設等の世帯数をintegerに変換
#48列目:男親と子供から成る世帯をintegerに変換
#53列目:3世代世帯数をintegerに変換
#61-64,66-68,71-73,79,81(産業別就業人口)をintegerに変換
ing <- c(39,42,48,53,61:64,66:68,71:73,79,81)

for(i in ing) {
  dat[,i] <- as.integer(as.character(dat[,i]))
  #factorをinteger型/numeric型に変換するときは,一度character型に変換してから
  #"-"がNAに変換される
}

dat[,81] #"-"が"NA"に変換されている

#"NA"を0に変換する
for(i in ing) {
  dat[,i] <- replace(dat[,i], which(is.na(dat[,i])), 0)
}

##1,2列目:0を補完
#まず,character型に変換
fct <- c(1,2)
for(i in fct) {
  dat[,i] <- as.character(dat[,i])
}

#0を補完
for(i in 1:nrow(dat)) {
  if(nchar(dat[i,1])==1) {
    dat[i,1] <- paste("0",dat[i,1],sep="")
  }
  if(nchar(dat[i,2])==4) {
    dat[i,2] <- paste("0",dat[i,2],sep="")
  }
}

head(dat[,1:2]) #結果を確認
#1行目のみ"00000"に修正
dat[1,2] <- "00000"

#1,2列目:文字コードをfactorに変換
for(i in fct) {
  dat[,i] <- as.factor(dat[,i])
}

head(dat[,1:2]) #結果を確認
class(dat[,1])
class(dat[,2])
dim(dat) #1969x83のdata.frame

#map.spdf@dataに統計データを突合する(行の順番を揃えるためfor文使用)
new.dat <- NULL
for(i in 1:nrow(map.spdf@data)) {
  new.dat <- rbind(new.dat,merge(x=map.spdf@data[i,],y=dat,by.x="JCODE",
                                 by.y="都道府県.市区町村コード",all.x=TRUE))
  print(paste("i =",i,"/",nrow(map.spdf@data),"is done"))
}
map.spdf@data <- new.dat
length(map.spdf@polygons) #1902のpolygon
dim(map.spdf@data) #1902x94のdata.frame

####4.探索####
#データ型の確認
for(i in 1:ncol(map.spdf@data)) {
  print(paste(i,colnames(map.spdf@data)[i],class(map.spdf@data[,i])))
}

##いくつかの変数について割合を計算する
#"50 外国人 numeric"から外国人比率を計算
PctF <- map.spdf@data[,"外国人"]/map.spdf@data[,"人口総数"]
PctFnames <- "外国人の割合"

#"56 うち核家族世帯 integer"
#"57 うち夫婦のみの世帯 integer"
#"58 うち夫婦と子供から成る世帯 integer"
#"59 うち男親と子供から成る世帯 numeric"
#"60 うち女親と子供から成る世帯 integer"
#"61 うち単独世帯 integer"
#"62 うち65歳以上の高齢単身者世帯 integer"
#"63 X.再掲.高齢夫婦世帯.夫65歳以上妻60歳以上の夫婦1組のみの一般世帯. integer"
#"64 X.再掲.３世代世帯 numeric"
#から,各世帯数の割合を計算
Pcthousehold <- map.spdf@data[,56:64]/map.spdf@data[,"一般世帯数"]
Pcthousehold.names <- paste(colnames(map.spdf@data[56:64]),"比率",sep="")

#72-92列目:産業別就業人口から産業別就業人口割合を算出
Pctindustry <- map.spdf@data[,72:92]/map.spdf@data[,"人口総数"]
Pctindustry.names <- paste(colnames(map.spdf@data[72:92]),"人口比率",sep="")

map.spdf@data <- cbind(map.spdf@data[,1:50],PctF,
                       map.spdf@data[,51:64],Pcthousehold,
                       map.spdf@data[,65:92],Pctindustry,
                       map.spdf@data[,93:94])

#列名を確認
for(i in 1:ncol(map.spdf@data)) {
  print(paste(i,colnames(map.spdf@data)[i],class(map.spdf@data[,i])))
}

colnames(map.spdf@data)[51] <- PctFnames
colnames(map.spdf@data)[66:74] <- Pcthousehold.names
colnames(map.spdf@data)[103:123] <- Pctindustry.names

#列目を再確認
for(i in 1:ncol(map.spdf@data)) {
  print(paste(i,colnames(map.spdf@data)[i],class(map.spdf@data[,i])))
}

##各データの分布確認
#10列～12列(チェーンの店舗数),20～125列(人口統計データ)のヒストグラムをpngで出力する
current_directory <- "./1.3_Result/"
setwd(current_directory)
colnum <- c(10:12,20:125)

for(i in colnum) {
  png(file=paste(i,"_",colnames(map.spdf@data)[i],".png",sep=""))
  par(mfrow=c(2,1))
  plot(sort(map.spdf@data[,i],na.last=TRUE),1:nrow(map.spdf@data),pch=16,
       main=colnames(map.spdf@data)[i],col="#ff00ff40",ylab="Index",xlab="")
  truehist(map.spdf@data[,i], prob=FALSE, col="black", 
           ylab="市区町村の数",xlab="")
  dev.off()
  print(paste(i,colnames(map.spdf@data)[i],"is done"))
}
par(mfrow=c(1,1))
#Windowsフォトビューアーで,1.3_Resultフォルダに出力されたpngファイルを確認する

##各説明変数について色分け地図をkmlで出力
#マックの場合エラーが出る可能性あり
current_directory <- "./1.3.1_KML_file/"
setwd(current_directory)
pal <- brewer.pal(8,"YlOrRd") #色付けに用いる色(9は色数,"YlOrRd"は色の種類)

for(i in colnum) {
  #色を分ける境界を抽出
  class <- classIntervals(map.spdf@data[,i],n=8,style="equal")
  brks <- class$brks
  cols <- pal[findInterval(map.spdf@data[,i],brks,,rightmost.closed=TRUE)]
  colcode <- findColours(class,cols)
  #ポリゴン毎に着色
  ShopNum.kml <- sapply(map.spdf@polygons,
                        function(x) {
                          kmlPolygon(x,col=pal[findInterval(
                            map.spdf@data[as.numeric(slot(x,"ID"))+1,i],brks,rightmost.closed=TRUE)],
                                     lwd=2,border="black",description=slot(x,"ID"))
                        }) #polygonのidは0から開始しているので,slot(x,"ID")とする
  #kmlを出力
  outfile <- file(paste(i,colnames(map.spdf@data)[i],".kml",paste=""),
                  "w",encoding="UTF-8")
  cat(kmlPolygon()$header,sep="\n",file=outfile) #concatenating
  cat(unlist(ShopNum.kml["style",]),sep="\n",file=outfile)
  cat(unlist(ShopNum.kml["content",]),sep="\n",file=outfile)
  cat(kmlPolygon()$footer,sep="\n",file=outfile)
  close(outfile)
  #凡例をpngで出力
  png(file=paste(i,"_",colnames(map.spdf@data)[i],"_legend.png",sep=""))
  plot(1,1,axes=F,type="n",xlab="",ylab="",main=colnames(map.spdf@data)[i])
  legend("center",legend=names(attr(colcode,"table")),fill=pal,cex=2,bty="n")
  print(paste(i,colnames(map.spdf@data)[i],"/",,"is done"))
  graphics.off()
}
setwd("../")
setwd("../")
warnings() #警告を確認(NAが無視されたことがわかる)
#GoogleEarthで1.3.1_KML_fileフォルダに出力されたkmlファイルを確認する
#凡例としてpngファイルを用いる
#分布がlong tailである変数(人口比率等)のBrksはquantileにした方がよい

##説明変数間の相関関係を確認
cor.check <- function(X,q) { #X:データ,q:閾値
  val <- NULL
  COR <- cor(X,use="na.or.complete")
  k = 1
  for (i in 1:nrow(COR)) {
    for (j in 1:ncol(COR)) {
      if (abs(COR[i,j]) >= q && i > j)　{
        #オブジェクトを格納する場合に[[]]を使用する
        val <- rbind(val,c(colnames(X)[i],colnames(X)[j],COR[i,j],abs(COR[i,j])))
        k = k + 1
      }
    }
  }
  val
}

#相関係数が0.7以上または-0.7以下の変数を調べる
check <- cor.check(map.spdf@data[,colnum],0.7)
colnames(check) <- c("Variable1","Variable2","correlationC","ABS")
write.table(check,"1.3.2_cor.check.csv",sep=",")
#1_GIS_data_analysisフォルダにある
#"1.3.2_cor.check.csv"をexcel等で開き,相関が強い変数を確認する
#相関が極めて強い:0.9以上または-0.9以下
#相関が強い:0.7以上0.9未満または-0.9より大きく0.7以下
#これらの相関関係を示す変数間には因果関係が存在する可能性が高く,
#2つを同時に説明変数に採用するのは不適当であると考える

##[ここで問題]##
#以上の情報から,人口統計データからハンバーガーチェーン3社の出店傾向を探る
#ための分析の方針を考えて下さい。
