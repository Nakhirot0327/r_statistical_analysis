######################################
###1.1.2 点データのカーネル密度推定###
######################################
#R-2.15.3以下で実行
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_Statistical_analysis/101_R/140216_Seminer Data/"
setwd(current_directory)

setwd("./1_GIS_data_analysis/")
#install.packages("rgdal") #初回のみ
library(rgdal)
#install.packages("RColorBrewer") #初回のみ
library(RColorBrewer)
#install.packages("maps") #初回のみ
library(maps)
#install.packages("XML") #初回のみ
library(XML)
#install.packages("gpclib")
library(gpclib) #3.0.2には対応していない(2.15以下でのみ動作)
#install.packages("maptools")
library(maptools)
#install.packages("Splancs") #初回のみ
library(splancs)
#install.packages("MASS") #初回のみ
library(MASS)
#unionSpatialPolygons()関数のライブラリのライセンス関係で実行が必要
gpclibPermit() #警告が表示される

####1.地図の読込・加工####
#時間の都合上,今回は東京都のA社に限定
#マック使用者は,documents-export-2014-02-15.zipを解凍し,1_GIS_data_analysisフォルダに展開してから実行
map.spdf <- readShapeSpatial("japan_ver71.shp",proj4string=CRS("+proj=longlat +datum=WGS84"))

tokyo.spdf <- map.spdf[map.spdf$KEN=="東京都",]
head(tokyo.spdf@data) #属性値の確認
tokyo.spdf@data[,3] #3列目のSICHOを見ると,離島は支庁の有無で識別出来るとわかる
tokyo.spdf <- tokyo.spdf[is.na(tokyo.spdf$SICHO),] #離島を除く
tokyo.union <- unionSpatialPolygons(tokyo.spdf,tokyo.spdf$KEN) #地図を統合
plot(tokyo.union) #統合されていることを確認
str(tokyo.union,max.level=5) #データ構造の確認

Polynum <- length(tokyo.union@polygons[[1]]@Polygons) #ポリゴンの数
CRDS <- NULL

for(i in 1:Polynum) {
  CRDS <- rbind(CRDS,tokyo.union@polygons[[1]]@Polygons[[i]]@coords)
}

##図示するためのパラメータの設定
#変域
xmin <- min(CRDS[,1])
xmax <- max(CRDS[,1])
ymin <- min(CRDS[,2])
ymax <- max(CRDS[,2])

#色
mycols <- NULL
col.st <- seq(14,90,by=4)
for (i in 1:20) {
  mycols <- c(mycols,paste("#ff00ff",col.st[i],sep="")) #mycolsにはカラーコードが格納される
}

####2.店舗データの読込・加工####
mc <- read.csv(file="1.1_ShopGeometry_mc.csv",header=TRUE)
mc[,2] <- as.numeric(as.character(mc[,2])) #factorをnumericに変換
mc[,3] <- as.numeric(as.character(mc[,3])) #factorをnumericに変換
mc <- subset(mc,!is.na(mc[,2])&!is.na(mc[,3])) #今回は欠損値を削除する
MC <- mc[,2:3] #緯度,経度を抽出
MC <- cbind(MC[,2],MC[,1]) #経度,緯度の順番にする

####3.カーネル密度推定####
MC.tokyo <- subset(MC,subset=
                    MC[,1]<xmax & MC[,1]>xmin &
                    MC[,2]<ymax & MC[,2]>ymin)
MC.d <- kde2d(MC.tokyo[,1],MC.tokyo[,2],
              c(bandwidth.nrd(MC.tokyo[,1]),bandwidth.nrd(MC.tokyo[,2])),
              n=1000)

op <- par(no.readonly=TRUE) # 現在のグラフィックスパラメータ値をopに退避する
par(mar=c(1,1,1,1),ps=10) #余白幅の調整。順に底辺,左,上,右の順。
plot(tokyo.union,xlim=c(xmin,xmax),ylim=c(ymin-0.05,ymax+0.05),axes=FALSE)
image(MC.d,xlab="latitude",ylab="longitude",add=TRUE,col=mycols)
contour(MC.d,xlab="latitude",ylab="longitude",add=TRUE,col=2)
par(op)

####4.Google Earth上への出力####
#SpatialPOintsとデータを合わせて,SpatialPointsDataFrameを作成
str(MC.d)
tokyo.grid <- expand.grid(MC.d$x,MC.d$y)
MC.d.z <- NULL

for(i in 1:ncol(MC.d$z)) {
  MC.d.z <- c(MC.d.z,MC.d$z[,i])
}
MC.d.z <- as.data.frame(MC.d.z)

tokyo.MC.d.spdf <- SpatialPointsDataFrame(tokyo.grid,data=MC.d.z,
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))

setwd("./1.1_Result/")
#pngデータを作成し,kmlで出力
tokyo.sg <- Sobj_SpatialGrid(tokyo.MC.d.spdf)$SG
tokyo.gesg <- GE_SpatialGrid(tokyo.sg)
png(file="1.1_Tokyo_MC.png",width=tokyo.gesg$width,
    height=tokyo.gesg$height,bg="transparent")
par(mar=c(0,0,0,0),xaxs="i",yaxs="i")
image(MC.d,xlab="latitude",ylab="longitude",col=mycols)
contour(MC.d,xlab="latitude",ylab="longitude",add=TRUE,col=2,lwd=0.1)
dev.off()
par(op)

kmlOverlay(tokyo.gesg,"1.1_Tokyo_MC.kml","1.1_Tokyo_MC.png")
setwd("../")

####[補足1]rで使用できる主要カラーパレット####
demo.pal <- function(n, border = if (n<32) "light gray" else NA,
                     main = paste("color palettes;  n=",n),
                     ch.col = c("rainbow(n, start=.7, end=.1)", "heat.colors(n)",
                                "terrain.colors(n)", "topo.colors(n)", "cm.colors(n)"))
{
  nt <- length(ch.col)
  i <- 1:n; j <- n / nt; d <- j/6; dy <- 2*d
  plot(i,i+d, type="n", yaxt="n", ylab="", main=main)
  for (k in 1:nt) {
    rect(i-.5, (k-1)*j+ dy, i+.4, k*j,
         col = eval(parse(text=ch.col[k])), border = border)
    text(2*j,  k * j +dy/4, ch.col[k])
  }
}
n <- if(.Device == "postscript") 64 else 16
# Since for screen, larger n may give color allocation problem
demo.pal(n)