##############6.時変ARモデルによる株価予測##############
######データの取り込みと整形,図示######
stock <- read.csv("http://k-db.com/site/jikeiretsu.aspx?c=7201-T&hyouji=&download=csv",skip=1,header=TRUE)

stockday <- matrix(stock[,1],ncol=1)
stockdaynum <- matrix(seq(1,nrow(stock),by=1),ncol=1)
stockindex <- cbind(stockday,stockdaynum)

delete <- c(2,3,4,6,7)
stock <- stock[,-delete]
stock <- ts(stock,start=1,freq=1)
stock <- stock[order(stock[,1]),]

stocksub <- matrix(rep(0,(nrow(stock)-1)*2),ncol=2)
for (i in 1:(nrow(stock)-1)) {
  stocksub[i,2] <- stock[i+1,2]-stock[i,2]
  stocksub[i,1] <- i
}

stocksub <- ts(stocksub,start=1,freq=1)

par(mfrow=c(2,1),ps=15)
ts.plot(stock[,2],type="l",col=1,lty=1,
        main="日産自動車(7201)の株価(終値)120730-130802",
        ylab="株価(円)")
ts.plot(stocksub[,2],type="l",col=1,lty=1,
        main="日産自動車(7201)の株価(終値)120730-130802(階差1)",
        ylab="階差(円)")
par(mfrow=c(1,1))

#データ数Nを求める
ND <- length(stocksub[,2])
range(stocksub[,2])

#以下の分析対象を階差系列に限定するため、変数の入れ換え
stock <- stocksub[,2]

####Preparation for maximizing likelihood and optimization(Defining functions)#####
#オプションの設定
limy <- 1e3
MJ <- 10　　　    #設定したいARモデルの次数の最大値
NF <- 30          #時変スペクトルの周波数の階級数
Mn <- 20          #時変共分散関数の最大ラグ
IT <- 5           #いくつかの原数値の平均値を解析対象とする場合に、平均の計算に用いる原数値の個数
N <- trunc(ND/IT) #周期数
NO <- ND - IT*N   #余り

if (NO > 0) {
  N <- N + 1
}

y <- numeric(N)       #周期毎の平均の格納スペース
NN <- 0

for (i in 1:(N-1)) {  #周期毎の平均を計算し、格納
  c <- 0
  for (j in 1:IT) {
    NN <- NN + 1
    c <- c + stock[NN]
  }
  y[i] <- c/IT
}
c <- 0

for (j in 1:(ND-(N-1)*IT)) { #最後(第N周期)の平均を計算し、格納
  NN <- NN + 1
  c <- c + stock[NN]
}
y[N] <- c/(ND-(N-1)*IT)

#行列の設定
FGset <- function(q,k) {
  if (k == 1) {
    m <- q
    F <- diag(m)
    G <- diag(m)
  }
  if (k == 2) {
    m <- 2*q
    F1 <- cbind(2*diag(q),-diag(q))
    F2 <- cbind(diag(q),0*diag(q))
    F <- rbind(F1,F2)
    G <- rbind(diag(q),0*diag(q))
  }
  return(list(m=m,MatF=F,MatG=G))
}

#AR係数から自己共分散関数を計算する
AUTCOR <- function(al,SIG2,q,Mn) {
  n <- q + 1
  M <- max(q,Mn) + 1
  scv <- numeric(M)
  c <- numeric(n)
  A <- matrix(0,n,n)
  c[1] <- SIG2
  A[1,2:n] <- al
  A[2,1:q] <- al
  B <- diag(n)
  if (n > 2) {
    for (i in 3:n) {
      A[1,1:(n-i+1)] <- al[(i-1):q]
      for (j in 2:(i-1)) {
        B[i, j] <- -al[i-j]
      }
    }
  }
  B <- B - A
  scv[1:n] <- solve(B,c)
  if (Mn > q) {
    for (j in 1:(Mn-q)) {
      a <- 0
      for (i in 1:q) {
        a <- a + scv[n+j-i]*al[i]
      }
      scv[n+j] <- a
    }
  }
  return(scv)
}

#AR係数からPARCORを計算する
PARCOR <- function(al,q) {
  if (q == 1) {
    par <- al
  } else {
    par <- al
    g <- numeric(50)
    for (ii in 1:(q-1)) {
      II <- q - ii
      s <- 1 - par[II+1]^2
      for (I in 1:II) {
        g[I] <- (par[I] + par[II+1]*par[II-I+1])/s
      }
      I2 <- (II + 1)/2
      MII <- II - 2*trunc(II/2)
      if (MII == 1) {
        g[I2] <- par[I2]/(1-par[II+1])
      }
      for (I in 1:II) {
        par[I] <- g[I]
      }
    }
  }
  return(par)
}

#離散フーリエ変換(Goertzel法)
FOURIE <- function(x,N,NF) {
  FC <- numeric(NF)
  FS <- numeric(NF)
  W <- pi/(NF-1)
  for (i in 1:NF) {
    CI <- cos((i-1)*W)
    SI <- sin((i-1)*W)
    T1 <- 0
    T2 <- 0
    for (j in 2:N) {
      jj <- N - j + 2
      T0 <- 2*CI*T1 - T2 + x[jj]
      T2 <- T1
      T1 <- T0
    }
    FC[i] <- CI*T1 - T2 + x[1]
    FS[i] <- SI*T1
  }
  return(list(FC=FC,FS=FS))
}

#ARモデルのスペクトル
ARSP <- function(TAU2,al,q,NF) {
  h <- numeric(50)
  h[1] <- 1
  for (i in 1:q) {
    h[i+1] <- -al[i]
  }
  Fal <- FOURIE(h,q+1,NF)
  FR <- Fal$FC
  FI <- Fal$FS
  SP <- TAU2/(FR^2 + FI^2)
  LSP <- log(SP)
  return(LSP)
}

#カルマンフィルタの関数
KF1 <- function(y,XFO,VFO,F,G,Q,R,limy,ISW,OSW,m,q,N) {
  if(OSW == 1) {
    XPS <- matrix(0,m,N)  #平均xn|n-1を格納する行列
    XFS <- matrix(0,m,N)  #平均xn|nを格納する行列
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
    XP <- F %*% XF #xn-1|n-1からxn|n-1を求める
    VP <- F %*% VF %*% t(F) + G %*% Q %*% t(G) #Vn-1|n-1からVn|n-1を求める
    #フィルタ
    if(y[n] < limy) { #y[i]が上限値(limy)を超えていない場合
      H <- matrix(0,1,m)
      for (i in 1:q) { #yn-1,yn-2,…,yn-mの値をHに格納する
        n0 <- n-i
        if (n0 > 0) {
          if (y[n0] < limy) {
            H[1,i] <- y[n0]
          }
        }
      }
      NSUM <- NSUM + 1　　　　　　#フィルタの実行回数のカウント
      B <- H %*% VP %*% t(H) + R　#ynの分散共分散行列
      B1 <- solve(B)　　　　　　　#Bの逆行列を求める
      K <- VP %*% t(H) %*% B1     #Kはカルマンゲイン
      e <- y[n] - H %*% XP　　　　#ynの予測誤差
      XF <- XP + K %*% e　　　　　#xn|n-1とynの予測誤差からxn|nを求める
      VF <- VP - K %*% H %*% VP　 #Vn|n-1からVn|nを求める
      SIG2 <- SIG2 + t(e) %*% B1 %*% e　#SIG2の最尤推定値
      LDET <- LDET + log(det(B))        #最大対数尤度の第2項の計算
    } else { #y[i]が上限値(limy)を超えている場合
      XF <- XP　#xn|n=xn|n-1とする
      VF <- VP　#vn|n=vn|n-1とする
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
    FF <- -0.5 * (NSUM * (log(2*pi)+SIG2)+LDET) #SIG2を所与の値とした場合の最大対数尤度
  }
  if (OSW == 0) {　#OSW==0:最大対数尤度を計算する場合
    return(list(LLF=FF,Ovar=SIG2))
  }
  if (OSW == 1) {　#OSW==1:状態フィルタ分布を計算する場合
    return(list(XPS=XPS,XFS=XFS,VPS=VPS,VFS=VFS,Ovar=SIG2))
  }
}

#平滑化の関数
SMO1 <- function(XPS,XFS,VPS,VFS,F,GSIG2,m,q,N) {
  XSS <- matrix(0,m,N) 　　　　#平滑化の結果のうち,Xn|Nを格納するスペース
  VSS <- array(dim=c(m,m,N))　 #平滑化の結果のうち,Vn|Nを格納するスペース
  XS1 <- XFS[,N]  　　　　　　 #xN|NをXS1に代入
  VS1 <- VFS[,,N]              #VN|NをVS1に代入
  XSS[,N] <- XS1
  VSS[,,N] <- VS1
  for (n1 in 1:(N-1)) {
    n <- N - n1                #N-n
    XP <- XPS[,n+1]            #XN-n+1|N
    XF <- XFS[,n]              #XN-n|N
    VP <- VPS[,,n+1]           #VN-n+1|N
    VF <- VFS[,,n]             #VN-n|N
    VPI <- solve(VP)           #VN-n+1|Nの逆行列
    A <- VF %*% t(F) %*% VPI              #AN = Vn|n * F^t_n+1 * F^-1n+1|nに対応
    XS2 <- XF + A %*% (XS1 - XP)          #Xn|N = xn|n + An (xn+1|N - xn+1|n)に対応
    VS2 <- VF + A %*% (VS1 - VP) %*% t(A) #Vn|N = Vn|n + An (Vn+1|N - Vn+1|n)*A^t_nに対応
    XS1 <- XS2                 #次の回に備えてXS1を作っておく
    VS1 <- VS2                 #次の回に備えてVS1を作っておく
    XSS[,n] <- XS1　　         #Xn|Nを格納
    VSS[,,n] <- VS1　          #Vn|Nを格納
  }
  arc <- matrix(0,q,N)　　　　 #Xn|Nを格納する入れ物
  arv <- array(dim=c(q,q,N))   #Vn|Nを格納する入れ物
  for (n in 1:N) {
    arc[,n] <- XSS[1:q,n]      #Xn|N(n=1,2,…,N-1)の結果を格納
    arv[,,n] <- GSIG2 * VSS[1:q,1:q,n]  #Vn|N(n=1,2,…,N-1)の結果を格納
  }
  return(list(arc=arc, arv=arv))
}

#TAU2の対数尤度関数の定義
LogL <- function(theta,y,limy,q,k,N) {
  TAU2 <- theta
  MAT <- FGset(q,k)
  m <- MAT$m
  F <- MAT$MatF
  G <- MAT$MatG
  OSW <- 0
  ISW <- 0
  Q <- TAU2*diag(q)
  R <- diag(1)
  XFO <- numeric(m)
  VFO <- 150 * diag(m)
  LL <- KF1(y,XFO,VFO,F,G,Q,R,limy,ISW,OSW,m,q,N)
  LL0 <- LL$LLF
  return(LL0)
}

STAU2 <- matrix(0,2,MJ)
SLL <- matrix(0,2,MJ)
MLL <- -1e10

for (I in 1:2) {
  k <- I
  for (J in 1:MJ) {
    q <- j
    LLF1 <- optimize(LogL,lower=1e-6,upper=1e2,maximum=TRUE,
                     y=y,limy=limy,q=q,k=k,N=N)
    TAU2 <- LLF1$maximum
    LL <- LLF1$objective
    STAU2[I,J] <- TAU2
    SLL[I,J] <- LL
    if (LL > MLL) {
      MLL <- LL
      Mq <- q
      Mk <- k
    }
  }
}

q <- Mq
k <- Mk
TAU2 <- STAU2[k,q]
MAT <- FGset(q,k)
m <- MAT$m
F <- MAT$MatF
G <- MAT$MatG
OSW <- 1
ISW <- 0
Q <- TAU2*diag(q)
R <- diag(1)
XFO <- numeric(m)
VFO <- 150 * diag(m)
LLF2 <- KF1(y, XFO, VFO, F, G, Q, R, limy, ISW, OSW, m, q, N)
XPS <- LLF2$XPS
XFS <- LLF2$XFS
VPS <- LLF2$VPS
VFS <- LLF2$VFS
SIG2 <- LLF2$Ovar
GSIG2 <- 1
XVS <- SMO1(XPS,XFS,VPS,VFS,F,GSIG2,m,q,N)
arc <- XVS$arc
print(SLL)
print(STAU2)
print(k)
print(q)

#時変PARCORの計算
PAR <- matrix(0,q,N)
for (n in 1:N) {
  al <- arc[,n]
  PAR[,n] <- PARCOR(al,q)
} 

#時変PARCORのグラフ作成
t <- c(1:N)
xt <- PAR[1,]
par(mfrow=c(1,3),oma=c(0,0,0,0)+0.1,mar=c(4,4,2,2)+0.1)
plot(t,xt,xlim=range(t),ylim=c(-1,1),type="l",main="(a)",
     xlab="time",ylab="PARCOR",lwd=2)

xt <- PAR[2,]
plot(t,xt,xlim=range(t),ylim=c(-1,1),type="l",main="(b)",
     xlab="time",ylab="PARCOR",lwd=2)

xt <- PAR[3,]
plot(t,xt,xlim=range(t),ylim=c(-1,1),type="l",main="(c)",
     xlab="time",ylab="PARCOR",lwd=2)

#時変スペクトルの計算##########
N1 <- N/6
TVSP <- matrix(0,NF,N1)
for (n1 in 1:N1) {
  n <- 6*n1
  al < arc[,n]
  TVSP[,n1] <- ARSP(SIG2,al,q,NF)
}

#時変スペクトルのグラフ作成
t <- 6*c(1:N1)
fr <- 0.5*c(1:NF)/NF

par(mfrow=c(1,2),oma=c(0,0,0,0)+0.1,mar=c(4,4,2,2)+0.1)
library(lattice)
z <- TVSP
persp(fr,t,z,theta=45,phi=30,xlab="frequency",ylab="time",
      zlab="p(f)",main="(a)")
contour(fr,t,z,main="(b)",xlab="frequency",ylab="time")

#時変自己共分散関数の計算
N1 <- N/6
M <- max(q,Mn) + 1
AUC <- matrix(0,M,N1)
for (n1 in 1:N1) {
  n <- 6*n1
  al <- arc[,n]
  AUC[,n1] <- AUTCOR(al,SIG2,q,Mn)
}

#時変自己共分散関数のグラフ作成
library(lattice)
par(mfrow=c(1,2),oma=c(0,0,0,0)+0.1,mar=c(4,4,3,2)+0.1)
t <- 6*c(1:N1)
L <- c(1:M)
z <- AUC
persp(L,t,z,theta=45,phi=30,xlim=range(L),ylim=range(t),
      zlim=range(z),xlab="Lag",ylab="time",zlab="C",main="(a)")
contour(L,t,z,main="(b)",xlab="lag",ylab="time")
