##########################
###1.1.1 点データの図示###
##########################
#Tools > Global-options > text encoding をSJISに変更
#R-2.15.3以下で実行
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/"
setwd(current_directory)

setwd("./1_GIS_data_analysis/")
#install.packages("rgdal") #初回のみ実行
#install.packages("XML") #初回のみ実行
library(rgdal)
library(XML)

####1.A社の店舗のプロット####
#文字コードはUTF-8
#Rstudioの場合,データは正しく読み込まれているが表示は文字化けする
mc <- read.csv(file="1.1_ShopGeometry_mc.csv",header=FALSE)
#マックの場合,"マルチバイト"のエラーが表示されるので次を実行
#mc <- read.csv(file="1.1_ShopGeometry_mc.csv",header=FALSE,fileENcoding='Shift-JIS')
head(mc) #データ確認

#データ型を確認
for(i in 1:ncol(mc)) {
  print(paste(colnames(mc)[i],class(mc[,i])))
}
colnames(mc) <- c("ID","latitude","longitude","S_name","Postalcode","Address","Tel")

#データのクラスを変換
mc[,2] <- as.numeric(as.character(mc[,2])) #factorをnumericに変換
mc[,3] <- as.numeric(as.character(mc[,3])) #factorをnumericに変換
mc <- subset(mc,!is.na(mc[,2])&!is.na(mc[,3])) #今回は欠損値を削除する
dim(mc)
MC <- mc[,2:3] #緯度,経度を抽出

#SpatialPointsクラスを作成
MC <- cbind(MC[,2],MC[,1]) #経度,緯度の順番にする
colnames(MC) <- c("longitude","latitude")
cs <- CRS("+proj=longlat +datum=WGS84") #座標系指定
MC.sp <- SpatialPoints(MC,proj4string=cs) #dataframeをspクラスに変換
#各点のデータ(ID,郵便番号,住所)を作成
select <- c(1,4,5,6,7)
mc.dat <- mc[,select]
#SpatialPOintsとデータを合わせて,SpatialPointsDataFrameを作成
MC.spdf <- SpatialPointsDataFrame(MC.sp,data=mc.dat)
#SpatialPointsDataFrameをkml形式で出力
setwd("./1.1_Result/")
head(MC.spdf@data)
writeOGR(MC.spdf,dsn="1.1_ShopGeometry_MC.kml",layer="MC",driver="KML",
         overwrite_layer=TRUE,verbose=TRUE)
setwd("../")

####2.B社の店舗のプロット####
kfc <- read.csv(file="1.1_ShopGeometry_kfc.csv",header=FALSE) #本番はTRUE
for(i in 1:ncol(kfc)) {
  print(class(kfc[,i]))
}

#edit(kfc) #下方までスクロールすると8行目にバグがあるとわかる
kfc <- kfc[,-8]
colnames(kfc) <- c("ID","longitude","latitude","S_name","Postal","Address","Tel")
KFC <- cbind("longitude"=kfc[,3],"latitude"=kfc[,2]) #経度,緯度の順番にする
KFC.sp <- SpatialPoints(KFC,proj4string=cs) #dataframeをspクラスに変換

#各点のデータ(ID,店舗名,郵便番号,住所,電話番号)を作成
head(kfc,2)
select <- c(1,4,5,6,7)
kfc.dat <- kfc[,select]
#SpatialPOintsとデータを合わせて,SpatialPointsDataFrameを作成
KFC.spdf <- SpatialPointsDataFrame(KFC.sp,data=kfc.dat)
#SpatialPointsDataFrameをkml形式で出力
setwd("./1.1_Result/")
writeOGR(KFC.spdf,dsn="1.1_ShopGeometry_KFC.kml",layer="KFC",driver="KML",
         overwrite_layer=TRUE,verbose=TRUE)
setwd("../")
head(KFC.spdf@data)

####3.モスバーガーのプロット####
mos <- read.csv(file="1.1_ShopGeometry_mos.csv",header=TRUE,dec=".")
dim(mos)
#edit(mos) #下方までスクロールすると,ずれている列があるとわかる
mos <- mos[-1,]
####[要確認]1行目にバグあり####

for(i in 1:ncol(mos)) {
  print(class(mos[,i]))
}

mos[,1] <- as.numeric(as.character(mos[,1])) #factorをnumericに変換
mos[,2] <- as.numeric(as.character(mos[,2])) #factorをnumericに変換

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
MOS.sp <- SpatialPoints(MOS,proj4string=cs) #dataframeをspクラスに変換

#各点のデータ(ID,店舗名,郵便番号,住所,電話番号)を作成
head(mos,2)
select <- c(3,4)
mos.dat <- mos[,select]
#SpatialPOintsとデータを合わせて,SpatialPointsDataFrameを作成
MOS.spdf <- SpatialPointsDataFrame(MOS.sp,data=mos.dat)
#SpatialPointsDataFrameをkml形式で出力
setwd("./1.1_Result/")
writeOGR(MOS.spdf,dsn="1.1_ShopGeometry_MOS.kml",layer="MOS",driver="KML",
         overwrite_layer=TRUE,verbose=TRUE)
setwd("../")

####4.Google Earth上で出力を確認####
#1.1_ResultフォルダにあるGoogle Earthで"1.1_ShopGeometry_MC.kml"等
#を開くとプロットが表示される
#Google Earthで"場所"の該当フォルダ上で右クリック->"プロパティ"
#->"スタイル・色"->"スタイルを共有"を選択し,window右上のピンマークをクリック
#その上で各URLを張り付けるとピンを変えることが出来る
#http://img.mcdonalds.co.jp/index/graphic/logo_01.png
#https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSxQ7fYqjaRhwyLjS4_mgBWvyvH726EdLK0-iNYjIW_lEAS-QZy
#http://articleimage.nicoblomaga.jp/image/16/2011/a/8/a87c028ea9486d173d76187f28af22922c591d051324526459.gif
#等を使うと現実味がある