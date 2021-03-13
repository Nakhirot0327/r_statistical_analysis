##############5.状態空間モデルによるWTI石油価格の予測##############
#wdをファイル格納場所に変更
getwd()
setwd("C:/Users/Nakhirot0327/Documents/100_多変量解析/101_R/0804_Seminer Data")
#↑アドレスを各自変更して下さい

Oil <- read.csv("5_WTI_Oilprice.csv",header=TRUE)
head(Oil)
plot(Oil[,2],main="WTI Oil price",type="l",xlab="Month(08Jan=1)",ylab="USD/Barrel")

#時系列データの整形(2009年以降を抽出する)
y <- Oil[13:nrow(Oil),2]
plot(y,main="WTI Oil price",type="l",xlab="Month(09Jan=1)",ylab="USD/Barrel")
#分散のオーダー(桁)を大体把握する必要がある。
#Oilの場合はだいたい標準偏差が10～20前後なので分散のオーダーは100=1e2程度。

#東京卸売市場年次たまねぎ入荷数(トン)
#※時間があれば試してみて下さい
#Workspaceの変数をすべて消去後、
#1-15行目を実行する代わりに20-32行目を実行し、以下順に実行していけば結果が得られます
onion <- read.csv("5_onion.csv",header=TRUE,row.names=1)
onion_dat <- rep(0,nrow(onion)*ncol(onion))
k <- 1
for (i in 1:nrow(onion)){
  for (j in 1:ncol(onion)) {
    onion_dat[k] <- onion[i,j]
    k <- k + 1
  }
}
y <- onion_dat
plot(y,main="東京卸売市場たまねぎ入荷量(トン)",xlab="Month(1=89Jan)",
     type="l")

N <- length(y)
range(y)

#オプションの設定
limy <- 100000    #計測値の上限(range(y)を見て設定) ※Oilなら1000,onionなら100000
MJ <- 6         　#Cycle成分:次数の最大値
p <- 12           #Seasonal成分:季節変動の周期(月次データなので12)
k <- 2            #Trend成分:次数(1次関数のトレンドを想定)
MM <- 5　　       #Cycle成分:係数推定のための反復計算の回数

####Preparation for maximizing likelihood and optimization(Defining functions)#####

#Cycle成分:PARCORからAR係数を計算する関数(係数決定のためのアルゴリズム)
#詳しくは「ベイズ統計データ解析」p.148-149を参照のこと
ARCOEF <- function(par,q) {
  aa <- numeric(50)
  al <- numeric(q)
  if (q == 1) {
    al <- par
  } else {
    for (II in 1:q) {
      al[II] <- par[II]
      aa[II] <- par[II]
      if (II > 1) {
        for (J in 1:(II-1)) {
          al[J] <- aa[J] - par[II]*aa[II-J]
        }
        if (II < q) {
          for (J in 1:(II-1)) {
            aa[J] <- al[J]
          }          
        }
      }
    }
  }
  return(al)
}

#全体:状態空間モデルの行列設定のための関数(資料に掲載した数式に対応)
FGHset2 <- function(al,k,p,q) {
  m <- k + p + q - 1
  if (q > 0) {G <- matrix(0,m,3)} else {G <- matrix(0,m,2)}
  F <- matrix(0,m,m)
  H <- matrix(0,1,m)
  G[1,1] <- 1
  H[1,1] <- 1
  if (k == 1) {F[1,1] <- 1}
  if (k == 2) {F[1,1] <- 2;F[1,2] <- -1;F[2,1] <- 1}
  if (k == 3) {F[1,1] <- 3;F[1,2] <- -3;F[1,3] <- 1;F[2,1] <- 1;F[3,2] <- 1}
  LS <- k
  NS <- 2
  G[LS+1,NS] <- 1
  H[1,LS+1] <- 1
  for (i in 1:(p-1)) {F[LS+1,LS+i] <- -1}
  for (i in 1:(p-2)) {F[LS+i+1,LS+i] <- 1}
  LS <- LS + p - 1
  if (q > 0) {
    NS <- NS + 1
    G[LS+1,NS] <- 1
    H[1,LS+1] <-1
    for (i in 1:q) {F[LS+1,LS+i] <- al[i]}
    if (q > 1) {
      for (i in 1:(q-1)){F[LS+i+1,LS+i] <- 1}
    }
  }
  return(list(m=m,MatF=F,MatG=G,MatH=H))
}

#状態空間モデルにおける行列Qの設定(資料に掲載した数式に対応)
Qset <- function(TAU12,TAU22,TAU32,k,p,q) {
  NS <- 0
  if (k > 0) {NS <- NS + 1}
  if (p > 0) {NS <- NS + 1}
  if (q > 0) {NS <- NS + 1}
  Q <- diag(NS)
  NS <- 0
  if (k > 0) {NS <- NS + 1; Q[NS,NS] <- TAU12}
  if (p > 0) {NS <- NS + 1; Q[NS,NS] <- TAU22}
  if (q > 0) {NS <- NS + 1; Q[NS,NS] <- TAU32}
  return(Q)
}

#カルマンフィルタの関数(資料に掲載した数式に対応)
#"%*%"は行列の積を表す記号
KF <- function(y,XFO,VFO,F,H,G,Q,R,limy,ISW,OSW,m,N) {
  if(OSW == 1) {
    XPS <- matrix(0,m,N)       #平均xn|n-1を格納する行列
    XFS <- matrix(0,m,N)       #平均xn|nを格納する行列
    VPS <- array(dim=c(m,m,N)) #分散共分散行列vn|n-1を格納する配列
    VFS <- array(dim=c(m,m,N)) #分散共分散行列vn|nを格納する配列
  }
  XF <- XFO #x0|0
  VF <- VFO #v0|0
  NSUM <- 0 #フィルタの実行回数カウンター
  SIG2 <- 0 #分散係数の最尤推定値(の初期値)
  LDET <- 0 #最大対数尤度の第2項(の初期値)
  for(n in 1:N) {
    #1期先予測
    XP <- F %*% XF                             #xn-1|n-1からxn|n-1を求める
    VP <- F %*% VF %*% t(F) + G %*% Q %*% t(G) #Vn-1|n-1からVn|n-1を求める
    #フィルタ
    if(y[n] < limy) { #y[i]が上限値(limy)を超えていない場合
      NSUM <- NSUM + 1　　　　　　      #フィルタの実行回数のカウント
      B <- H %*% VP %*% t(H) + R　      #ynの分散共分散行列
      B1 <- solve(B)　　　　　　　      #Bの逆行列を求める
      K <- VP %*% t(H) %*% B1           #Kはカルマンゲイン
      e <- y[n] - H %*% XP　　　　      #ynの予測誤差
      XF <- XP + K %*% e　　　　　      #xn|n-1とynの予測誤差からxn|nを求める
      VF <- VP - K %*% H %*% VP　       #Vn|n-1からVn|nを求める
      SIG2 <- SIG2 + t(e) %*% B1 %*% e　#SIG2の最尤推定値
      LDET <- LDET + log(det(B))        #最大対数尤度の第2項の計算
    } else { #y[i]が上限値(limy)を超えている場合
      XF <- XP　                        #xn|n=xn|n-1とする
      VF <- VP　                        #vn|n=vn|n-1とする
    }
    if (OSW == 1) {　#OSW==1:状態フィルタ分布を計算する場合
      XPS[,n] <- XP  #xn|n-1を格納
      XFS[,n] <- XF  #xn|nを格納
      VPS[,,n] <- VP #vn|n-1を格納
      VFS[,,n] <- VF #vn|nを格納
    }
  }
  SIG2 <- SIG2/NSUM
  if (ISW == 0) {  
    FF <- -0.5 * (NSUM * (log(2*pi*SIG2)+1)+LDET) #SIG2を未知パラメータとして最尤推定する場合の最大対数尤度
  } else {
    FF <- -0.5 * (NSUM * (log(2*pi)+SIG2)+LDET)   #SIG2を所与の値とした場合の最大対数尤度
  }
  if (OSW == 0) {　#OSW==0:最大対数尤度を計算する場合
    return(list(LLF=FF,Ovar=SIG2))
  }
  if (OSW == 1) {　#OSW==1:状態フィルタ分布を計算する場合
    return(list(XPS=XPS,XFS=XFS,VPS=VPS,VFS=VFS,Ovar=SIG2))
  }
}

#平滑化の関数(資料に掲載した数式に対応)
SMO <- function(XPS,XFS,VPS,VFS,F,GSIG2,k,p,q,m,N) {
  XSS <- matrix(0,m,N) 　　　　#平滑化の結果のうち,Xn|Nを格納する入れ物
  VSS <- array(dim=c(m,m,N))　 #平滑化の結果のうち,Vn|Nを格納する入れ物
  XS1 <- XFS[,N]  #xN|NをXS1に代入
  VS1 <- VFS[,,N] #VN|NをVS1に代入
  XSS[,N] <- XS1
  VSS[,,N] <- VS1
  for (n1 in 1:(N-1)) {
    n <- N - n1      #N-n
    XP <- XPS[,n+1]  #XN-n+1|N
    XF <- XFS[,n]    #XN-n|N
    VP <- VPS[,,n+1] #VN-n+1|N
    VF <- VFS[,,n]   #VN-n|N
    VPI <- solve(VP) #VN-n+1|Nの逆行列
    A <- VF %*% t(F) %*% VPI              #AN = Vn|n * F^t_n+1 * F^-1n+1|nに対応
    XS2 <- XF + A %*% (XS1 - XP)          #Xn|N = xn|n + An (xn+1|N - xn+1|n)に対応
    VS2 <- VF + A %*% (VS1 - VP) %*% t(A) #Vn|N = Vn|n + An (Vn+1|N - Vn+1|n)*A^t_nに対応
    XS1 <- XS2 #次の回に備えてXS1を作っておく
    VS1 <- VS2 #次の回に備えてVS1を作っておく
    XSS[,n] <- XS1　　#Xn|Nを格納
    VSS[,,n] <- VS1　 #Vn|Nを格納
  }
  t <- numeric(N)     #tnの係数の平均値の収束値を格納するスペース
  s <- numeric(N)　　 #snの係数の平均値の収束値を格納するスペース
  r <- numeric(N)     #rnの係数の平均値の習得値を格納するスペース
  tv <- numeric(N)    #tnの係数の分散の収束値を格納するスペース
  sv <- numeric(N)　　#snの係数の分散の収束値を格納するスペース
  rv <- numeric(N)    #rnの係数の分散の収束値を格納するスペース
  if (q == 0) {
    for (n in 1:N) {
      t[n] <- XSS[1,n]
      s[n] <- XSS[k+1,n]
      tv[n] <- GSIG2 * VSS[1,1,n]
      sv[n] <- GSIG2 * VSS[k+1,k+1,n]
    }
  } else {
    for (n in 1:N) {
      t[n] <- XSS[1,n]
      s[n] <- XSS[k+1,n]
      r[n] <- XSS[k+p,n]
      tv[n] <- GSIG2 * VSS[1,1,n]
      sv[n] <- GSIG2 * VSS[k+1,k+1,n]
      rv[n] <- GSIG2 * VSS[k+p,k+p,n]
    }
  }
  return(list(trd=t,sea=s,arc=r,trv=tv,sev=sv,arv=rv))
}

#カルマンフィルタおよび平滑化の関連計算
TSRest <- function(y,TAU12,TAU22,TAU32,al,OSW,limy,k,p,q,N) {
  MAT <- FGHset2(al,k,p,q)
  m <- MAT$m
  F <- MAT$MatF
  G <- MAT$MatG
  H <- MAT$MatH
  ISW <- 0
  Q <- Qset(TAU12,TAU22,TAU32,k,p,q)
  R <- diag(1)
  XFO <- numeric(m)
  VFO <- 100*diag(m)
  for (i in 1:k) {XFO[i] <- y[1]}
  if (q > 0) {
    for (i in 1:q) {
      VFO[k+p+i-1,k+p+i-1] <- 10
    }
  }
  LLF <- KF(y,XFO,VFO,F,H,G,Q,R,limy,ISW,OSW,m,N)
  if (OSW == 1) {
    XPS <- LLF$XPS
    XFS <- LLF$XFS
    VPS <- LLF$VPS
    VFS <- LLF$VFS
    SIG2 <- LLF$Ovar
    LL - LLF$LLF
    XVS <- SMO(XPS,XFS,VPS,VFS,F,SIG2,k,p,q,m,N)
    xt <- XVS$trd
    xs <- XVS$sea
    xr <- XVS$arc
    return(list(LLF=LL,xt=xt,xs=xs,xr=xr,SIG2=SIG2,F=F,G=G,H=H,Q=Q,R=R,
                XPS=XPS,XFS=XFS,VPS=VPS,VFS=VFS))
  } else {
    LL <- LLF$LLF
    SIG2 <- LLF$Ovar
  }
  return(list(LLF=LL,SIG2=SIG2))
}

#Trend成分:TAU1^2(分散)の尤度関数を最大化する関数
#計算を容易にするため対数をとっています
LogLI1 <- function(theta,y,limy,k,p,N) {
  TAU12 <- theta
  #TAU2^2に関する最適化
  #13,14行目で確認したオーダーを踏まえて、upperを指定
  LLF <- optimize(LogLI2,lower=1e-2,upper=1e2,maximum=TRUE,y=y, #Onionならupper=1e3,Oilならupper=1e2
                  TAU12=TAU12,limy=limy,k=k,p=p,N=N)
  LL <- LLF$objective
  return(LL)
}

#Seasonal成分:TAU2^2(分散)の尤度関数を最大化する関数
#上記でTAU1^2が決定した後にこの関数を用います
LogLI2 <- function(theta,y,TAU12,limy,k,p,N) {
  TAU22 <- theta
  LLF <- TSRest(y,TAU12,TAU22,0,0,0,limy,k,p,0,N)
  LL <- LLF$LLF
  return(LL)
}

#Cycle成分:TAU3^2(分散)の尤度関数を最大化する関数
#詳しくは「ベイズ統計データ解析」p.148-149を参照のこと
#上記でTAU1^2,TAU^2が決定した後にこの関数を用います
LogL1 <- function(theta,y,TAU12,TAU22,Spar,limy,k,p,q,N) {
  TAU32 <- theta
  #PARCOR(偏自己相関係数)に関する最適化
  if (q == 1) {
    LLF <- optimize(LogL2,lower=-0.95,upper=0.95,maximum=TRUE,y=y,
                    TAU12=TAU12,TAU22=TAU22,TAU32=TAU32,limy=limy,k=k,p=p,N=N)
  } else {
    LLF <- optimize(LogL3,lower=-0.95,upper=0.95,maximum=TRUE,y=y,
                    TAU12=TAU12,TAU22=TAU22,TAU32=TAU32,Spar=Spar,limy=limy,
                    k=k,p=p,q=q,N=N)
  }
  LL <- LLF$objective
  return(LL)
}

#Cycle成分:ARモデルの係数αを決定するためのアルゴリズム
#上記でTAU1^2,TAU2^2,TAU3^2が決定した後にこの関数を用います
#アルゴリズムの詳細は「ベイズ統計解析」p.148-149を参照のこと
LogL2 <- function(theta,y,TAU12,TAU22,TAU32,limy,k,p,N) {
  al <- numeric(1)
  al[1] <- theta
  LLF <- TSRest(y,TAU12,TAU22,TAU32,al,0,limy,k,p,1,N)
  LL <- LLF$LLF
  return(LL)
}

#Cycle成分:ARモデルの係数αを決定するためのアルゴリズム
#アルゴリズムの詳細は「ベイズ統計解析」p.148-149を参照のこと
#(q > 1)次以上のPARCORの対数尤度
#(q-1)次までのPARCOR,TAU1^2,TAU2^2,TAU3^2は所与
LogL3 <- function(theta,y,TAU12,TAU22,TAU32,Spar,limy,k,p,q,N) {
  par <- numeric(q)
  par[1:(q-1)] <- Spar[1:(q-1)]
  par[q] <- theta
  al <- ARCOEF(par,q)
  LLF <- TSRest(y,TAU12,TAU22,TAU32,al,0,limy,k,p,q,N)
  LL <- LLF$LLF
  return(LL)
}

#Cycle成分:ARモデルの係数αを決定するためのアルゴリズム
#アルゴリズムの詳細は「ベイズ統計解析」p.148-149を参照のこと
#TAU1^2の対数尤度(al,TAU3^2は所与)
LogL4 <- function(theta,y,TAU32,al,limy,k,p,q,N) {
  TAU12 <- theta
  #13,14行目で確認したオーダーを踏まえて、upperを指定
  LLF <- optimize(LogL5,lower=1e-2,upper=1e2,maximum=TRUE,y=y, #Onionならupper=1e3,Oilならupper=1e2
                  TAU12=TAU12,TAU32=TAU32,al=al,limy=limy,k=k,p=p,q=q,N=N)
  LL <- LLF$objective
  return(LL)
}

#Cycle成分:ARモデルの係数αを決定するためのアルゴリズム
#アルゴリズムの詳細は「ベイズ統計解析」p.148-149を参照のこと
#TAU2^2の対数尤度(TAU1^2,TAU3^2は所与)
LogL5 <- function(theta,y,TAU12,TAU32,al,limy,k,p,q,N) {
  TAU22 <- theta
  LLF <- TSRest(y,TAU12,TAU22,TAU32,al,0,limy,k,p,q,N)
  LL <- LLF$LLF
  return(LL)
}

####Executing algorithm maximizing likelihood and optimization#####

#Trend成分:TAU1^2の最尤推定
#13,14行目で確認したオーダーを踏まえて、upperを指定
LLF1 <- optimize(LogLI1,lower=1e-2,upper=1e2,maximum=TRUE,y=y,limy=limy,k=k,p=p,N=N) #Onionならupper=1e3,Oilならupper=1e2
ITAU12 <- LLF1$maximum

#Sesonal成分:TAU2^2の最尤推定
#13,14行目で確認したオーダーを踏まえて、upperを指定
LLF2 <- optimize(LogLI2,lower=1e-2,upper=1e2,maximum=TRUE,y=y,TAU12=ITAU12,limy=limy,k=k,p=p,N=N) #Onionならupper=1e3,Oilならupper=1e2

ITAU22 <- LLF2$maximum

STAU12 <- numeric(MJ)     #トレンド成分モデルの誤差分散の保存スペース
STAU22 <- numeric(MJ)     #季節成分モデルの誤差分散の保存スペース
STAU32 <- numeric(MJ)     #AR成分モデルの誤差分散の保存スペース
SSIG2 <- numeric(MJ)      #観測ノイズの分散の保存スペース
SLL <- numeric(MJ)        #最大対数尤度の保存スペース
SAIC <- numeric(MJ)       #AICの保存スペース
Sal <- matrix(0,MJ,MJ)    #AR成分モデルの係数の保存スペース(各行毎)
Spar <- numeric(MJ)       #PARCORの保存スペース
MAIC <- 1e10              

#Cycle成分:ARモデルの次数選択,係数αの最尤推定
for (J in 1:MJ) {
  q <- J
  PAR <- numeric(q)
  for (II in 1:MM) {
    if ((q == 1)&&(II == 1)) {TAU12 <- ITAU12; TAU22 <- ITAU22}
    #TAU3^2の最尤推定
    #13,14行目で確認したオーダーを踏まえて、upperを指定
    LLF3 <- optimize(LogL1,lower=1e-4,upper=1e2,maximum=TRUE,y=y, #Onionならupper=1e3,Oilならupper=1e2
                     TAU12=TAU12,TAU22=TAU22,Spar=Spar,limy=limy,
                     k=k,p=p,q=q,N=N)
    TAU32 <- LLF3$maximum
    #PARCORに関する最適化(係数αの最尤推定)
    if (q == 1) {
      LLF4 <- optimize(LogL2,lower=-0.95,upper=0.95,maximum=TRUE,y=y,
                       TAU12=TAU12,TAU22=TAU22,TAU32=TAU32,limy=limy,
                       k=k,p=p,N=N)
      par <- LLF4$maximum
      PAR[1] <- par
      al <- numeric(1)
      al[1] <- par
    } else {
      LLF4 <- optimize(LogL3,lower=-0.95,upper=0.95,maximum=TRUE,y=y,
                       TAU12=TAU12,TAU22=TAU22,TAU32=TAU32,Spar=Spar,
                       limy=limy,k=k,p=p,q=q,N=N)
      par <- LLF4$maximum
      PAR[1:(q-1)] <- Spar[1:(q-1)]
      PAR[q] <- par
      al <- ARCOEF(PAR,q)
    }
    #TAU1^2に関する最適化
    #13,14行目で確認したオーダーを踏まえて、upperを指定
    LLF5 <- optimize(LogL4,lower=1e-2,upper=1e2,maximum=TRUE,y=y, #Onionならupper=1e3,Oilならupper=1e2
                     TAU32=TAU32,al=al,limy=limy,k=k,p=p,q=q,N=N)
    TAU12 <- LLF5$maximum
    #TAU2^2に関する最適化
    #13,14行目で確認したオーダーを踏まえて、upperを指定
    LLF6 <- optimize(LogL5,lower=1e-2,upper=1e2,maximum=TRUE,y=y, #Onionならupper=1e3,Oilならupper=1e2
                     TAU12=TAU12,TAU32=TAU32,al=al,limy=limy,k=k,p=p,q=q,N=N)
    TAU22 <- LLF6$maximum
    print(paste("J =",J,",II =",II,"done"))
  }
  LLF7 <- TSRest(y,TAU12,TAU22,TAU32,al,0,limy,k,p,q,N)
  LL <- LLF7$LLF
  SIG2 <- LLF7$SIG2
  AIC <- -2*LL + 2*(q+4)
  if (AIC < MAIC) {
    MAIC <- AIC     #AICの最小値を更新
    Mq <- q　　　　 #最小のAICを与える次数qを更新
  }
  Sal[q,1:q] <- al
  Spar[q] <- PAR[q]
  STAU12[q] <- TAU12
  STAU22[q] <- TAU22
  STAU32[q] <- TAU32
  SSIG2[q] <- SIG2
  SLL[q] <- LL
  SAIC[q] <- AIC
  print(paste("J =",J,"done"))
} #JはCycle成分の次数,IIはTAU最尤推定に関するアルゴリズムの実行回数

TAU12 <- STAU12[Mq]   #Trend成分:最小のAICを与えるTAU1^2
TAU22 <- STAU22[Mq]   #Seasonal成分:最小のAICを与えるTAU2^2
TAU32 <- STAU32[Mq]   #Cycle成分:最小のAICを与えるTAU3^2
al <- Sal[Mq,1:Mq]    #Cycle成分:最小のAICを与えるARモデルの係数
LLF8 <- TSRest(y,TAU12,TAU22,TAU32,al,1,limy,k,p,Mq,N)
xt <- LLF8$xt         #Trend成分
xs <- LLF8$xs         #Seasonal成分
xr <- LLF8$xr         #Cycle成分
w <- y - xt - xs - xr #観測ノイズ(residuals)
yad <- y - xs         #季節調整済み系列

#モデル推定の結果表示
print(SLL)　　 #モデル全体:最大対数尤度(左から順にCycle成分の次数が1,2,…,6の場合)
print(SAIC)    #モデル全体:AIC(左から順にCycle成分の次数が1,2,…,6の場合)
print(STAU12)  #Trend成分:ノイズ項の分散(左から順にCycle成分の次数が1,2,…,6の場合)
print(STAU22)  #Sesonal成分:ノイズ項の分散(左から順にCycle成分の次数が1,2,…,6の場合)
print(STAU32)  #Cycle成分:ノイズ項の分散(左から順にCycle成分の次数が1,2,…,6の場合)
print(SSIG2)   #Residual成分:分散(左から順にCycle成分の次数が1,2,…,6の場合)
print(Sal)     #Cycle成分:係数(1行目から順にCycle成分の次数が1,2,…,6の場合)
print(Spar)    #Cycle成分:係数推定のために用いた偏自己相関係数(左から順にCycle成分の次数が1,2,…,6の場合)
print(Mq)      #Cycle成分の次数の最終結果(AICが最も小さい次数が選択される)

#トレンドと時系列データの折れ線グラフの作成
t <- c(1:N)
par(mfrow=c(2,2))
plot(t,xt,xlim=range(t),ylim=range(y),type="l",  #Trendと元データ
     xlab="time",ylab="Data and Trend",lwd=2)
lines(t,y)
plot(t,xs,xlim=range(t),ylim=range(xs),type="l",　#Seasonal
     xlab="time",ylab="Seasonal",lwd=1)
plot(t,xr,xlim=range(t),ylim=range(xr),type="l",  #Cycle
     xlab="time",ylab="Cycle",lwd=1)
plot(t,w,xlim=range(t),ylim=range(w),type="l",    #Residuals
     xlab="time",ylab="Residuals",lwd=1)
par(mfrow=c(1,1))

#####Prediction#####
#予測を行う関数
KFP <- function(XFO,VFO,F,H,G,Q,R,m,N) {
  XPSP <- matrix(0,m,N)       #平均xn|n-1を格納する行列
  XFSP <- matrix(0,m,N)       #平均xn|nを格納する行列
  VPSP <- array(dim=c(m,m,N)) #分散共分散行列vn|n-1を格納する配列
  VFSP <- array(dim=c(m,m,N)) #分散共分散行列vn|nを格納する配列
  XF <- XFO                   #予測時の初期値(xN|N)
  VF <- VFO                   #予測時の初期値(vN|N)
  ypred <- rep(0,N)           #yの予測値を格納するスペース
  ysig2 <- rep(0,N)           #yの予測値を格納するスペース
  for(n in 1:N) {
    XP <- F %*% XF                             #xn-1|n-1からxn|n-1を求める
    VP <- F %*% VF %*% t(F) + G %*% Q %*% t(G) #Vn-1|n-1からVn|n-1を求める
    ypred[n] <- H %*% XP                       #xn|n-1からynを求める
    ysig2[n] <- H %*% VP %*% t(H) + R  　　    #Vn|n-1からUn|n-1を求める
    XF <- XP                                   #xn|n=xn|n-1とする
    VF <- VP                                   #vn|n=vn|n-1とする
    XPSP[,n] <- XP
    XFSP[,n] <- XF
    VPSP[,,n] <- VP
    VFSP[,,n] <- VF
  }
  return(list(XPSP=XPSP,XFSP=XFSP,VPSP=VPSP,VFSP=VFSP,ypred=ypred,ysig2=ysig2))
}

#半年分(6カ月分)を予測する
nmonth <- 6
Pred <- KFP(XFO=LLF8$XFS[,length(y)],VFO=LLF8$VFS[,,length(y)],
            F=LLF8$F,H=LLF8$H,G=LLF8$G,Q=LLF8$Q,R=LLF8$R,m=k+p+Mq-1,N=nmonth)
yhat <- Pred$ypred
sig <- sqrt(Pred$ysig2 + SSIG2[Mq]) #Residuals成分の分散も合わせている(中原案なので使用上は吟味すること)
yhatl <- Pred$ypred + 2*sig
yhatu <- Pred$ypred - 2*sig
yhatl2 <- Pred$ypred + sig
yhatu2 <- Pred$ypred - sig
t <- c(1:length(y))
t_p <- c((length(y)+1):(length(y)+nmonth))
plot(t,y,xlim=c(1,t_p[nmonth]),ylim=c(min(yhatu,y),max(yhatl,y)),
     type="l",xlab="Month(09Jan=1)",ylab="USD/Barrel",lwd=1,main="WTI Oil Price")　#玉ねぎの場合はmain="東京卸売市場たまねぎ入荷量(トン)"とする
lines(t_p,yhat,col=2)
lines(t_p,yhatl2,lty=1,col=3)
lines(t_p,yhatu2,lty=1,col=3)
lines(t_p,yhatl,lty=1,col=4)
lines(t_p,yhatu,lty=1,col=4)
legend("topleft",legend=c("Prediction","70%CI","95%CI"),col=c(2,3,4),lty=c(1)) #たまねぎの場合は"topleft"を"topright"に変更