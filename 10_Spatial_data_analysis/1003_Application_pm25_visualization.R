######################################
###1.2.1 属性値をもつ点データの図示###
######################################
#R-2.15.3以下で実行
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/"
setwd(current_directory)

setwd("./1_GIS_data_analysis/")
#install.packages("GeoXp")
library(GeoXp)
#install.packages("SparseM")
library(SparseM)
#install.packages("spdep")
library(spdep)
#install.packages("maptools")
library(maptools)
#install.packages("classInt")
library(classInt)
#install.packages("RColorBrewer")
library(RColorBrewer)

####1.pm2.5関連データの入力・整形####
station <- read.csv("1.2_sokuchi_2011.csv",header=TRUE,dec=".") #decは小数点の表記の指定
head(station) #長い
colnames(station) #列名の確認
#1,3-5,7-12,14-16,21,24,26,28列以降は今回は削除
delete1 <- c(1,3:5,7:12,14:16,21,24,26,28:ncol(station))
station <- station[,-delete1]
colnames(station) #列名の確認
colnames(station)[which(colnames(station)=="国環研局番")] <- "SCODE" #突合のため

pmdata <- read.csv("1.2_pm2.5_2011.csv",header=TRUE,dec=".")
#read.csvについては
#http://www.okada.jp.org/RWiki/?R%A4%C7%A5%C7%A1%BC%A5%BF%A5%AF%A5%EA%A1%BC%A5%CB%A5%F3%A5%B0
#が参考になる
colnames(pmdata)
#1-5,8,11,14-17,19,28列は今回は削除
delete2 <- c(1:5,8,11,14:17,19,28)
pmdata <- pmdata[,-delete2]
colnames(pmdata) #列名の確認
colnames(pmdata)[which(colnames(pmdata)=="測定局コード")] <- "SCODE" #突合のため

pm <- merge(pmdata,station,by="SCODE")
colnames(pm)

#両データ(pmdata,station)で重複している項目を確認,削除
table(pm[,"測定局名.x"] == pm[,"測定局名.y"]) #表記が異なる
table(pm[,"都道府県コード.x"] == pm[,"都道府県コード.y"]) #一致
table(pm[,"都道府県名.x"] == pm[,"都道府県名.y"]) #表記が異なる
table(pm[,"市区町村コード.x"] == pm[,"市区町村コード.y"]) #一致
table(pm[,"市区町村名.x"] == pm[,"市区町村名.y"]) #表記が異なる
delete3 <- c("測定局名.y","都道府県コード.y","都道府県名.y",
             "市区町村コード.y","市区町村名.y")
pm <- pm[,!colnames(pm) %in% delete3] #delete3に含まれない列のみ抽出
colnames(pm) #重複している列が消えた

#列の並び替え
pm <- cbind("SCODE"=pm[,1],"測定局名"=pm[,"測定局名.x"],
            pm[,2:5],"用途地域名"=pm[,7],
            "経度"=pm[,"経度"],"緯度"=pm[,"緯度"],
            "住所"=pm[,"住所"],"建物名等"=pm[,"建物名等"],
            pm[,8:63])

#データ型の確認
for(i in 1:ncol(pm)) {
  print(paste(i,colnames(pm)[i],class(pm[,i])))
}

#列番号を記録
year.avg <- 13
month.avg.column <- 32:43
max.column <- 44:55
month.avg.day.over.35μg.column <- 56:67
#[参考]:日本のPM2.5の環境基準の行政上の目標値は日平均値35μg/m3以下
#参照:http://www.pref.saitama.lg.jp/page/pm25.html

#地図の読みこみと統合
map.spdf <- readShapeSpatial("japan_ver71.shp",proj4string=CRS("+proj=longlat +datum=WGS84"))
map.union <- unionSpatialPolygons(map.spdf,map.spdf$KEN) #地図を統合

####2.図示時の色の設定####
#色
colnum <- 8 #色の段階数
mycols <- brewer.pal(colnum,"YlOrRd")
col.st <- seq(20,90,by=10)

for.col1 <- NULL
for.col2 <- NULL
for.col3 <- NULL

#月平均値の数値を1つにまとめ,全数値を調べ8等分する区間を抽出
for(i in month.avg.column) {
  for.col1 <- c(for.col1,pm[,i])
}
class1 <- classIntervals(for.col1,n=colnum) #欠損値あり
brks1 <- classIntervals(for.col1,n=colnum)$brks #欠損値あり

#日平均値の最高値の数値を1つにまとめ,全数値を調べ8等分する区間を抽出
for(i in max.column) {
  for.col2 <- c(for.col2,pm[,i])
}
class2 <- classIntervals(for.col2,n=colnum) #欠損値あり
brks2 <- classIntervals(for.col2,n=colnum)$brks #欠損値あり

#日平均値が35μg/m3を超えた日数の数値を1つにまとめ,全数値を調べ8等分する区間を抽出
for(i in month.avg.day.over.35μg.column) {
  for.col3 <- c(for.col3,pm[,i])
}
class3 <- classIntervals(for.col3,n=colnum) #欠損値あり
brks3 <- classIntervals(for.col3,n=colnum)$brks #欠損値あり

####3.図示####
current_directory <- "./1.2_Result/"
setwd(current_directory)

pm.spdf <- SpatialPointsDataFrame(pm[,8:9],pm,
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
str(pm.spdf,max.level=2)
#dataスロット(@data)にpmの経度緯度以外のデータが格納され,
#coordsスロット(@coords)にpmの経度緯度のデータが格納されているとわかる
#bboxスロットには経度緯度の最小値,最大値が格納されている

#Google Earthに出力するための準備
pm.sg <- Sobj_SpatialGrid(pm.spdf)$SG
pm.gesg <- GE_SpatialGrid(pm.sg)

##重複を避けるため,kmlを出力する関数を定義
#kmlを出力する関数
outkml <- function(filename,data,spdf,gesg,colnum,style="quantile",
                   pch=16,cex1=0.5,cex2=1,col,class,brks) {
  png(file=paste(filename,".forkml.png",sep=""),width=gesg$width,height=gesg$height,bg="transparent")
  par(mar=c(0,0,0,0),xaxs="i",yaxs="i")
  if(missing(brks)) {
    cols <- col[findInterval(data,
                             classIntervals(data,n=colnum, #[参考]を確認
                                            style=style)$brks,
                             all.inside=TRUE)]    
  } else {
    cols <- col[findInterval(data,brks,all.inside=TRUE)]
  }
  if(missing(class)) {
    class <- classIntervals(data,n=colnum,style=style) #[参考]を確認 
  }  
  colcode <- findColours(class,cols)
  plot(spdf,pch=pch,col=cols,cex=cex1,
       xlim=gesg$xlim,ylim=gesg$ylim,setParUsrBB=TRUE) #○にならない
  legend("bottomright",legend=names(attr(colcode,"table")),
         fill=col,cex=cex2,bty="n")
  dev.off()
  kmlOverlay(gesg,paste(filename,".kml",sep=""),paste(filename,".forkml.png",sep=""))
}
#[参考]:色分けクラスの区間を均等にしたいのであればstyle="equal"とする
#他にも"kmeans","hclust","fisher"のオプションが可能

#pngを出力する関数
outpng <- function(filename,data,spdf,gesg,colnum,map,title,style="quantile",
                   pch=16,cex1=0.5,cex2=1,col,class,brks) {
  png(file=paste(filename,".png",sep=""),width=gesg$width,height=gesg$height)
  par(mar=c(0,0,2,0),xaxs="i",yaxs="i")
  if(missing(brks)) {
    cols <- col[findInterval(data,
                             classIntervals(data,n=colnum, #[参考]を確認
                                            style=style)$brks,
                             all.inside=TRUE)]    
  } else {
    cols <- col[findInterval(data,brks,all.inside=TRUE)]    
  }
  if(missing(class)) {
    class <- classIntervals(data,n=colnum,style=style) #[参考]を確認 
  }  
  colcode <- findColours(class,cols)
  plot(map)
  plot(spdf,pch=pch,col=cols,cex=cex1,
       xlim=gesg$xlim,ylim=gesg$ylim,setParUsrBB=TRUE,add=TRUE) #○にならない
  legend("bottomright",legend=names(attr(colcode,"table")),
         fill=col,cex=cex2,bty="n")
  title(title)
  dev.off()
}

##年平均値
outkml(filename="1.2.1_pm.year.avg",data=pm[,year.avg],spdf=pm.spdf,
       gesg=pm.gesg,colnum=colnum,col=mycols)
setwd("./1.2.1_Result/")
outpng(filename="1.2.1_pm.year.avg",data=pm[,year.avg],spdf=pm.spdf,
       gesg=pm.gesg,colnum=colnum,col=mycols,
       map=map.union,title="2011年度 pm2.5年平均値(μg/m3)")
setwd("../")

##月平均値
month <- c(4:12,1:3)
k <- 1
month.avg.column <- 32:43

for(i in month.avg.column) {
  year.month <- if(month[k]<=3) {paste(2012,".",month[k],sep="")}
  else {paste(2011,".",month[k],sep="")}
  outkml(filename=paste("1.2.1_pm.month.avg",year.month,sep=""),
         data=pm[,i][!is.na(pm[,i])],spdf=pm.spdf,
         gesg=pm.gesg,colnum=colnum,col=mycols,class=class1,brks=brks1)
  setwd("./1.2.1_Result/")
  outpng(filename=paste("1.2.1_pm.month.avg",year.month,sep=""),
         data=pm[,i][!is.na(pm[,i])],
         spdf=pm.spdf,gesg=pm.gesg,colnum=colnum,
         col=mycols,map=map.union,class=class1,brks=brks1,
         title=paste("pm2.5 月平均値(μg/m3)",
                     if(month[k]<=3) {paste(2012,"年",month[k],"月",sep="")}
                     else {paste(2011,"年",month[k],"月",sep="")},sep=""))
  setwd("../")
  print(paste("k = " ,k," is done",sep=""))
  k <- k + 1
}

##月別日平均値の最高値
k <- 1
max.column <- 44:55

for(i in max.column) {
  year.month <- if(month[k]<=3) {paste(2012,".",month[k],sep="")}
  else {paste(2011,".",month[k],sep="")}
  outkml(filename=paste("1.2.1_pm.max",year.month,sep=""),
         data=pm[,i][!is.na(pm[,i])],
         spdf=pm.spdf,
         gesg=pm.gesg,colnum=colnum,
         col=mycols,class=class2,brks=brks2)
  setwd("./1.2.1_Result/")
  outpng(filename=paste("1.2.1_pm.max",year.month,sep=""),
         data=pm[,i][!is.na(pm[,i])],spdf=pm.spdf,
         gesg=pm.gesg,colnum=colnum,col=mycols,map=map.union,
         class=class2,brks=brks2,
         title=paste("pm2.5 月最高値(μg/m3)",
                     if(month[k]<=3) {paste(2012,"年",month[k],"月",sep="")}
                     else {paste(2011,"年",month[k],"月",sep="")},sep=""))
  setwd("../")
  print(paste("k = " ,k," is done",sep=""))
  k <- k + 1
}

##日平均値が35μg/m3を超えた日数
k <- 1
month.avg.day.over.35μg.column <- 56:67

for(i in month.avg.day.over.35μg.column) {
  year.month <- if(month[k]<=3) {paste(2012,".",month[k],sep="")}
  else {paste(2011,".",month[k],sep="")}
  outkml(filename=paste("1.2.1_pm.days.over.35μg",year.month,sep=""),
         data=pm[,i][!is.na(pm[,i])],
         spdf=pm.spdf,
         gesg=pm.gesg,colnum=colnum,
         col=mycols,class=class3,brks=brks3)
  setwd("./1.2.1_Result/")
  outpng(filename=paste("1.2.1_pm.days.over.35μg",year.month,sep=""),
         data=pm[,i][!is.na(pm[,i])],spdf=pm.spdf,
         gesg=pm.gesg,colnum=colnum,col=mycols,map=map.union,
         class=class3,brks=brks3,
         title=paste("pm2.5 日平均値が35μg/m3を超えた日数(日)",
                     if(month[k]<=3) {paste(2012,"年",month[k],"月",sep="")}
                     else {paste(2011,"年",month[k],"月",sep="")},sep=""))
  setwd("../")
  print(paste("k = " ,k," is done",sep=""))
  k <- k + 1
}

#1.2.1_Resultフォルダに出力された結果を確認する
