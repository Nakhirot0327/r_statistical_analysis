########################################################################
###1.3.2.国勢調査結果を用いたハンバーガーチェーンの出店傾向分析(実行)###
########################################################################
#1.3.1を実行後にこのファイルを実行すること
#各自のホームディレクトリを入力
current_directory <- "C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/140215_Seminer Data/"
setwd(current_directory)

setwd("./1_GIS_data_analysis/")
#install.packages("spgwr") #初回のみ
library(spgwr)

####1.目的の設定####
#まず単に人口と店舗数で単回帰分析
MC.lm <- lm(MC_CNT~人口総数,data=map.spdf@data)
KFC.lm <- lm(KFC_CNT~人口総数,data=map.spdf@data)
MOS.lm <- lm(MOS_CNT~人口総数,data=map.spdf@data)

par(mfrow=c(1,3))
plot(map.spdf@data[,"人口総数"],map.spdf@data[,"MC_CNT"],
     xlab="市区町村別人口",ylab="市区町村別店舗数",main="A社")
abline(a=MC.lm$coefficients[1],b=MC.lm$coefficients[2])
text(7e+05,1,paste("R^2 =",round(summary(MC.lm)$r.squared,2)))

plot(map.spdf@data[,"人口総数"],map.spdf@data[,"KFC_CNT"],
     xlab="市区町村別人口",ylab="市区町村別店舗数",main="B社")
abline(a=KFC.lm$coefficients[1],b=KFC.lm$coefficients[2])
text(7e+05,1,paste("R^2 =",round(summary(KFC.lm)$r.squared,2)))

plot(map.spdf@data[,"人口総数"],map.spdf@data[,"MOS_CNT"],
     xlab="市区町村別人口",ylab="市区町村別店舗数",main="C社")
abline(a=MOS.lm$coefficients[1],b=MOS.lm$coefficients[2])
text(7e+05,1,paste("R^2 =",round(summary(MOS.lm)$r.squared,2)))
par(mfrow=c(1,1))
#人が多い都市に店舗が大きいことは当然である。
#この演習では,残りの要因を地域差を考慮して分析する。

####2.説明変数の選択####
#データ列名を再確認
for(i in 1:ncol(map.spdf@data)) {
  print(paste(i,colnames(map.spdf@data)[i],class(map.spdf@data[,i])))
}
#従属変数:店舗数
#説明変数:人口,人口増減率,平均年齢,人口性比(男性の数÷女性の数),
#単身世帯比率,昼間人口比率
#以上の変数にて地理的加重回帰を実行する。

#"1.3.2_cor.check.csv"を見る限り,上記の間に0.9を超える極めて強い相関がある
#変数は少ない。また,店舗を出店する動機として上記は根拠になりそう

#上記の説明変数間にはいずれも強い相関はないことが"1.3.2_cor.check.csv"でわかる
#(強い相関とは,ここでは0.7以上または-0.7以下)
#したがって,多重共線性が起こる可能性は低い。

exp.val <- c(20,23,26,48,71,125)
#103列目,105-123列目(産業別人口比率)を含めても良いが時間がかかる

####3.地図データの整形####
##データの無い地域(北方領土など)を消す
map.spdf.select <- map.spdf
delete <- NULL
#edit(map.spdf.select@data) 北方領土などは1列目にJCODEが無い

#データを消す
for(i in 1:nrow(map.spdf.select@data)) {
  if(is.na(map.spdf.select@data[i,1])) { #1列目(JCODE)がNAの列の番号を抽出
    delete <- c(delete,i)
  }
}

map.spdf.select@data <- map.spdf.select@data[-delete,]
polygons.delete <- as.character(delete-1) #消去すべきポリゴンのID

#ポリゴンを消す
k <- 1
for(i in 1:length(map.spdf@polygons)) {
  if(!map.spdf.select@polygons[[i]]@ID %in% polygons.delete) {
    map.spdf.select@polygons[[k]] <- map.spdf@polygons[[i]]
    k <- k + 1
    print(paste("i =",i,"/",length(map.spdf@polygons),"polygons were replaced"))
  }
}

length(map.spdf.select@polygons) #1902個のポリゴン
nrow(map.spdf.select@data) #1894行のデータ

#1895番目-1902番目のポリゴンを消す
for(i in length(map.spdf@polygons):nrow(map.spdf.select@data)+1) { #1902から1895番目の順番で消す
  map.spdf.select@polygons[[i]] <- NULL
}
length(map.spdf.select@polygons)

#ポリゴンにIDを振りなおす
for(i in 1:length(map.spdf.select@polygons)) {
  map.spdf.select@polygons[[i]]@ID <- as.character(i)
  print(paste("i =",i,"/",length(map.spdf.select@polygons),"polygons were renamed"))
}

####4.GMRの実行####
###距離低減関数の作成
##gweightオプションで距離低減関数を指定する
#gweight=gwr.Gauss:Brunsdon(1996)のガウス型の距離低減関数
#Wi=√exp(-(di/θ)^2)
#θ:距離低減関数パラメータ(ハンド幅(bandwidth))
#di:観測地点iと周辺サンプルにおけるその他の観測地点までの距離
g.Gauss1.MC <- gwr.sel(MC_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
                       +うち単独世帯比率+昼夜間人口比率,
                       gweight=gwr.Gauss,data=map.spdf.select,
                       adapt=TRUE,method="cv") #2分
g.Gauss1.KFC <- gwr.sel(KFC_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
                       +うち単独世帯比率+昼夜間人口比率,
                       gweight=gwr.Gauss,data=map.spdf.select,
                       adapt=TRUE,method="cv") #2分
g.Gauss1.MOS <- gwr.sel(MOS_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
                       +うち単独世帯比率+昼夜間人口比率,
                       gweight=gwr.Gauss,data=map.spdf.select,
                       adapt=TRUE,method="cv") #2分
#adapt=TRUE:でエッジ効果を補正する
#cv score:sum of squared CV errorsのこと
#method="aic"でAICによる推定も可能
#Adaptive q:θの推定値

#gweight=gwr.tricube:McMillenとMcDonaldが1998年に提唱した(LeSage,1999)tri-cube関数
#Wi=(1-(di/qi)^3)^3 I(di<qi)
#qi:観測地点iのq番目に近い近傍観測地点までの距離
#関数I(・):与えられた式が真の場合は1,偽の場合は0を返す。
#したがって,Wiによる重みは半径qiの内部に限定される。
#di:観測地点iからその他の観測地点までの距離を要素とするベクトル
#今回は時間がないのでtricubeは省略:
#g.tric1.MC <- gwr.sel(MC_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
#                       +うち単独世帯比率+昼夜間人口比率,
#                       gweight=gwr.tricube,data=map.spdf.select,
#                       adapt=TRUE,method="cv") #2分
#g.tric1.KFC <- gwr.sel(KFC_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
#                       +うち単独世帯比率+昼夜間人口比率,
#                       gweight=gwr.tricube,data=map.spdf.select,
#                       adapt=TRUE,method="cv") #2分
#g.tric1.MOS <- gwr.sel(MOS_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
#                       +うち単独世帯比率+昼夜間人口比率,
#                       gweight=gwr.tricube,data=map.spdf.select,
#                       adapt=TRUE,method="cv") #2分

###GWRモデルの作成
res.gauss.MC <- gwr(MC_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
               +うち単独世帯比率+昼夜間人口比率,
               data=map.spdf.select,adapt=g.Gauss1.MC,gweight=gwr.Gauss)
res.gauss.KFC <- gwr(KFC_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
                    +うち単独世帯比率+昼夜間人口比率,
                    data=map.spdf.select,adapt=g.Gauss1.KFC,gweight=gwr.Gauss)
res.gauss.MOS <- gwr(MOS_CNT~人口総数+平成17年.22年の人口増減率+平均年齢+人口性比
                     +うち単独世帯比率+昼夜間人口比率,
                     data=map.spdf.select,adapt=g.Gauss1.MOS,gweight=gwr.Gauss)
#結果の表示
#各説明変数の係数の概略が示される。"Global"は重回帰モデルの係数
res.gauss.MC
res.gauss.KFC
res.gauss.MOS

###地域毎の回帰係数をkmlで出力
setwd("./1.3_Result/1.3.2_GWR_file/")
GWR.res <- NULL
GWR.res[[1]] <- res.gauss.MC
GWR.res[[2]] <- res.gauss.KFC
GWR.res[[3]] <- res.gauss.MOS
exp.val.name <- colnames(map.spdf.select@data)[exp.val]
GWR.res.name <- c("GWR.MC","GWR.KFC","GWR.MOS")
pal <- brewer.pal(8,"YlOrRd") #色付けに用いる色(9は色数,"YlOrRd"は色の種類)
exp.val.num <- NULL

for(i in 1:ncol(res.gauss.MC$SDF@data)) {
  if(colnames(res.gauss.MC$SDF@data)[i] %in% exp.val.name) {
    exp.val.num <- c(exp.val.num,i)
  }
}

for(j in 1:length(GWR.res)) {
  for(i in exp.val.num) {
    #カラーパレットの用意
    class <- classIntervals(GWR.res[[j]]$SDF@data[,i],n=8,style="equal")
    brks <- class$brks
    cols <- pal[findInterval(GWR.res[[j]]$SDF@data[,i],brks,,rightmost.closed=TRUE)]
    colcode <- findColours(class,cols)
    
    #ポリゴン毎に着色
    ShopNum.kml <- sapply(GWR.res[[j]]$SDF@polygons,
                          function(x) {
                            kmlPolygon(x,col=pal[findInterval(GWR.res[[j]]$SDF@data[as.numeric(slot(x,"ID")),i],
                                                              brks,rightmost.closed=TRUE)],
                                       lwd=1,border="black",
                                       description=slot(x,"ID"))
                          })
    #kmlを出力
    outfile <- file(paste(colnames(GWR.res[[j]]$SDF@data)[i],
                          GWR.res.name[j],".kml",sep=""),
                    "w",encoding="UTF-8")
    cat(kmlPolygon()$header,sep="\n",file=outfile)
    cat(unlist(ShopNum.kml["style",]),sep="\n",file=outfile)
    cat(unlist(ShopNum.kml["content",]),sep="\n",file=outfile)
    cat(kmlPolygon()$footer,sep="\n",file=outfile)
    close(outfile)
    
    #凡例をpngで出力
    png(file=paste(paste(colnames(GWR.res[[j]]$SDF@data)[i],GWR.res.name[j],
                         ".legend.png",sep="")))
    plot(1,1,axes=F,type="n",xlab="",ylab="",
         main=paste(colnames(GWR.res[[j]]$SDF@data)[i],GWR.res.name[j],sep=""))
    legend("center",legend=names(attr(colcode,"table")),fill=pal,cex=2,bty="n")
    graphics.off()
    
    print(paste(GWR.res.name[j],colnames(GWR.res[[j]]$SDF@data)[i]," is done",sep=""))
  }
} #GWRの結果(地域毎の回帰係数)を地図上に出力

setwd("../")
setwd("../")

####5.GWRの結果の検証####
###重回帰モデルとGWRモデルの比較
##決定係数R^2
par(mfrow=c(1,3))
plot(GWR.res[[1]]$SDF@data$localR2,type="l",main="A社",
     ylab="R squared")
abline(h=cor(map.spdf.select$MC_CNT,GWR.res[[1]]$lm$fitted.values)^2,col=2)
legend("topleft",legend=c("GWR","MLM"),lty=c(1,1),col=c(1,2),bty="n")
#MLMは重回帰モデルの決定係数。地域差を考慮したGWRの方が決定係数が高いことがわかる

plot(GWR.res[[2]]$SDF@data$localR2,type="l",main="B社",
     ylab="R squared")
abline(h=cor(map.spdf.select$KFC_CNT,GWR.res[[2]]$lm$fitted.values)^2,col=2)
legend("topleft",legend=c("GWR","MLM"),lty=c(1,1),col=c(1,2),bty="n")
#B社の場合,単純な重回帰の方がR squaredが高く,GWRは失敗と考えられる

plot(GWR.res[[3]]$SDF@data$localR2,type="l",main="C社",
     ylab="R squared")
abline(h=cor(map.spdf.select$MOS_CNT,GWR.res[[3]]$lm$fitted.values)^2,col=2)
legend("topleft",legend=c("GWR","MLM"),lty=c(1,1),col=c(1,2),bty="n")
par(mfrow=c(1,1))

##残差に地域の偏りがないことを確認する
#残差の計算
GWR.residual.MC <- map.spdf.select$MC_CNT - GWR.res[[1]]$SDF$pred
GWR.residual.KFC <- map.spdf.select$KFC_CNT - GWR.res[[2]]$SDF$pred
GWR.residual.MOS <- map.spdf.select$MOS_CNT - GWR.res[[3]]$SDF$pred
ymin <- min(c(GWR.residual.MC,GWR.residual.KFC,GWR.residual.MOS))
ymax <- max(c(GWR.residual.MC,GWR.residual.KFC,GWR.residual.MOS))

par(mfrow=c(1,3))
plot(GWR.residual.MC,type="l",main="マクドナルド",ylab="GWRモデルの残差",
     ylim=c(ymin,ymax),xlab="市区町村ID")
plot(GWR.residual.KFC,type="l",main="KFC",ylim=c(ymin,ymax),xlab="市区町村ID")
plot(GWR.residual.MOS,type="l",main="モスバーガー",ylim=c(ymin,ymax),xlab="市区町村ID")
par(mfrow=c(1,1))
#一部に残差が大きく出る傾向があるが,特定の地域に偏っている現象ではない。
