#############1. STOCK PRICE PREDICTION USING EXPONENTIAL SMOOTHNG#############
#SET WORKING DIRECTORY
setwd("/work/R/141025/")
getwd()

#READ DATA FROM WEB SITE
stock <- read.csv("http://k-db.com/stocks/7201-T?download=csv",skip=1,header=TRUE)
#FOR READING FROM CSV FILE
#stock <- read.csv("7201-T.csv" ,header=TRUE)
#NISSAN COMPANY NUMBER=7201
#TRY OTHRE COMPANY CODE IN URL
#E.X. http://k-db.com/stocks/2503-T?download=csv


stockday <- matrix(stock[,1],ncol=1)
stockdaynum <- matrix(seq(1,nrow(stock),by=1),ncol=1) #NO USE
stockindex <- cbind(stockday,stockdaynum)　　　　　　 #NO USE
delete <- c(2,3,4,6,7)
stock <- stock[,-delete]　　　　　 #DELETE COLUMNS #2,3,4,6,7 
stock <- ts(stock,start=1,freq=1)　#CHANGE DATE TO SEQUENTIAL NUMBER
stock <- stock[order(stock[,1]),]　#SORT BY DATE
head(stock)

#POT GRAPH (Time=250 IS YESTERDAY'S CLOSING PRICE)
par(mfrow=c(1,1), ps=15)
ts.plot(stock[,2], type="l", col=1, lty=1,
        main="NISSAN(7201) CLOSING PRICE",ylab="STOCK PRICE(YEN)")

#PREDICT WITH SOME DATA USING EXPONENATIAL SMOOTHING
lag <- c(2:10)
coef <- rev(c(-5:10)*0.1)

for (p in lag) {
  for (b in coef) {
    stocksub <- stock[1:(nrow(stock)-p),]
    stocksub <- ts(stocksub[,2],start=1,freq=1)
    n <- length(stocksub)
    x <- rep(0,p)
    (w <- (1-b)*(cumprod(c(1,rep(b,p-1))))) #WEIGHT COEFFICIENT
    
    datINT <- rev(stocksub[(n-p+1):n]) #SORT IN REVERSE ORDER FOR PREDICT INITIAL VALUE
    Pre <- filter(x,filter=w,method="recursive",init=datINT) #method="recursive": EXPONENTIAL SMOOTHING
    Pre2 <- ts(Pre,start=n+1,frequency=1)
    title <- paste("NISSAN CLOSING PRICE PREDICTION WITH EXPONENTIAL SMOOTHING","lag = ",p,",beta = ",b,sep="")
    ts.plot(stocksub,Pre2,type="l",lty=c(1,1),main=title,col=c(1:2),lwd=c(1,4),ylab="STOCK PRICE(YEN)")
    Sys.sleep(1)
    lines(stock[,2],type="l")
    legend("topleft",legend=c("Actual","Prediction"),lty=c(1,1),col=c(1,2),lwd=c(1,4))
    Sys.sleep(1)
  }
}

###############1-Additional STOCK PRICE PREDICT USING RECURRENCE FORULA###############
#Y_predictionT+1 = (1-β)YT + βY_predictionT　...(A)

coef <- rev(c(-5:10)*0.1)

for (b in coef) {
  w <- b    #'b' MEANS 'β' IN ABOVE RECURRENCE FORMULA
  x <- (1-b)*stock[,2]　#'x' IS 20% OF RAW DATA
  datHAT <- filter(x,filter=w,method="recursive",init=stock[1,2])
  #IN ABOVE FORMULA (A), x:(1-β)YT, w：β, init:Y1
  #CALCULATION LOGIC IS BELOW
  #datHAT[1] == stocksub[1]*0.8 + x[1]
  #datHAT[2] == datHAT[1]*0.8 + x[2]
  dathat <- ts(datHAT,start=1,frequency=1)
  title <- paste("NISSAN CLOSING PRICE PREDICTION WITH RECURRENCE FORMULA",",beta = ",b,sep="")
  ts.plot(stock[,2],dathat,type="l",lty=c(1,1),col=c(1,2),main=title,ylab="STOCK PRICE(YEN)")
  Sys.sleep(1)  
}
