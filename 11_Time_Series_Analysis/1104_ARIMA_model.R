##############4. STOCK PRICE PREDICTION WITH ARIMA MODEL##############
#####STEP0: PREPARING####
install.packages("urca")

#####STEP1: READ DATA AND PLOT (TIME SERIES AND AUTOCORRELATION COEFFICIENT)#####
#READ DATA FROM WEB SITE
pred <- read.csv("stock_prediction.csv")
stock <- read.csv("http://k-db.com/stocks/9984-T?download=csv",skip=1,header=TRUE)

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
acf(stock[,2],main="AUTOCORRELATION COEFFICIENT")
par(mfrow=c(1,1))

#####STEP2:######
#BEYOND THE SCOPE OF THIS CHAPTER

#####STEP3:単位根検定#####
library(urca)
#START FROM 'type="trend", lag=4'
(stockACF <- summary(ur.df(stock[,2],type="trend",lag=4)))

#'z.diff.lag' IS NOT SIGNIFICANT, CALCULATE AT 'lag=3'
(stockACF <- summary(ur.df(stock[,2],type="trend",lag=3)))

#'z.diff.lag' IS NOT SIGNIFICANT, CALCULATE AT 'lag=2'
(stockACF <- summary(ur.df(stock[,2],type="trend",lag=2)))

#'z.diff.lag' IS NOT SIGNIFICANT, CALCULATE AT 'lag=1'
(stockACF <- summary(ur.df(stock[,2],type="trend",lag=1)))

#'z.diff.lag' IS NOT SIGNIFICANT, CALCULATE AT 'lag=0'
(stockACF <- summary(ur.df(stock[,2],type="trend",lag=0)))

#P-VALUE OF 'z.lag1(=π)' IS UNDER 0.05, SO π=0 IS NOT TRUE(=ROOT UNIT IS NOT EXISTED)
#IF UNIT ROOT IS EXISTED, CALCULATE SERIES DIFFERENE AND RE-RUN STEP2 UNTIL UNIT ROOT IS NOT EXISTED

#####STEP4: DECIDE ARIMA MODEL COEFFICIENT REGARDING TREND#####
#(LINEAR EXPRESSION TREND)+ARMA(p,q) 
#EXPLAIN LATER HOW TO DECIDE 'p,q'
x <- seq(1,length(stock[,2]),by=1)　 #Time
stock_lm <- lm(stock[,2]~x)  　　　　#LINEAR REGRESSION
summary(stock_lm)                    #CONFIRM REGRESSION RESULT
a <- stock_lm$coef[1] 　　　　　　　 #CONSTANT TERM OF REGRESSION
b <- stock_lm$coef[2]                #SLOPE OF REGRESSION
trend <- a + b*x　　　　　　　　　　 #TREND SERIES
res <- stock[,2] - trend             #REMOVE TREND

#CALCULATE ARIMA MODEL EXHAUSTIVELY WITH 'p=0～4, q=0～4'
arima <- NULL
k <- 1

dim <- matrix(0,5*5,2)  #MATRIX TO STORE DEGREE (1ST COLUMN:p, 2ND COLUMN:q)
colnames(dim) <- c("AR(p)","MA(q)")
AIC <- rep(0,5*5)       #VECTOR TO PLUG IN AIC
results <- rep(0,5*5)   #VACTOR TO PLUG IN T-TEST RESULT

TF <- NULL              #VARIABLE TO STORE T-TEST RESULT(TRUE/FALSE)
cof <- NULL　　　　　　 #VARIABLE TO STORE COEFFICIENT

for (p in 0:4) {
  for (q in 0:4) {
    dim[k,] <- c(p,q)
    arima[[k]] <- arima(res,order=c(p,0,q),transform.pars=FALSE)  #PLUG IN ARIMA MODEL
    AIC[k] <- arima[[k]]$aic       #AIC
    cof[[k]] <- numeric(p+q)       #PLUG IN COEFFICIENT
    cof[[k]] <- arima[[k]]$coef　  #PLUG IN COEFFICIENT
    V <- arima[[k]]$var.coef　     #PLUG IN STANDART ERROR
    t <- numeric(p+q)              #PLUG IN T-VALUE
    TF[[k]] <- rep(0,p+q)          #PLUG IN T-TEST RESULT

    for (j in 1:(p+q)) {
      t[j] <- cof[[k]][j]/sqrt(V[j,j])   #CALCULATE T-VALUE ON EACH COEFFICIENT
    }
    #JUDGE SIGNIFICANT OF COEFFICIENT (T-TEST WITH SIGNIFICANCE LEVEL:5%)
    TF[[k]] <- ((t<0)&(pnorm(t)<0.05))|((t>=0)&(pnorm(t)>0.95))
    results[k] <- if(sum(((t<0)&(pnorm(t)<0.05))|((t>=0)&(pnorm(t)>0.95)))==p+q) {
      "OK"
    } else {
      "NG"
    }
    k <- k + 1
    print(paste("p = ",p,",q = ",q,",done",sep=""))
  }
}

#####STEP5: CONFIRM SIGNIFICANT OF COEFFICIENT#####
success <- rep(0,5*5)  #VECTOR TO PLUG IN RESULT
cons <- rep(0,5*5)     #VECTOR TO CHECK STATIONARITY IN STEP6
k <- 1

for (k in 1:(5*5)) {
  if (results[k] == "OK") {
    success[k] <- k
    if (dim[k,1] > 0) {
      cons[k] <- k      
    }
  }
}

success <- subset(success,success>0)
cons <- subset(cons,cons>0)
(dim_success <- dim[success,])   #CONFIRM 'p,q' OF MODEL WHOSE COEFFICIENT IS SIGNIFICANT

#####STEP6: CHECK STATIONARITY (MA(q) MODEL IS STATIONARITY, SO CHECK ONLY AR(p))#####
(cons_check <- dim[cons,])  #CONFIRM 'p,q' OF MODEL TO CHECK STATIONARITY
hantei <- rep(0,5*5)        #VECTOR TO PLUG IN CHECK RESULT
success2 <- rep(0,5*5)      #VECTOR TO PLUG IN CHECK RESULT

for (l in cons) {
  testp <- dim[l,1]
    switch(testp,
           "1" = hantei[l] <- if(sum(cof[[l]][1] < 1)==1) {"OK"} else {"NG"},
           "2" = hantei[l] <- if(sum((abs(polyroot(c(-cof[[l]][2],-cof[[l]][1],1))) < 1))==2) {"OK"} else {"NG"},
           "3" = hantei[l] <- if(sum((abs(polyroot(c(-cof[[l]][3],-cof[[l]][2],-cof[[l]][1],1))) < 1))==3) {"OK"} else {"NG"},
           "4" = hantei[l] <- if(sum(abs(polyroot(c(-cof[[l]][4],-cof[[l]][3],-cof[[l]][2],-cof[[l]][1],1))) < 1)==4) {"OK"} else {"NG"})
    if (hantei[l] == "OK") {
      success2[l] <- l
    }
}

write.table(stock,file="test.csv",sep=",")

success2 <- subset(success2,success2>0)

#####STEP7: Ljung-Box TEST (CHECK EQUALITY OF VARIANCE)#####
success3 <- rep(0,5*5)       #VECTOR TO PLUG IN CHECK RESULT

for (m in success2) {
  ahat <- arima[[m]]$resid  #RESIDUAL ERROR
  p.value <- numeric(20)    #VARIABLE TO PLUG IN P-VALUE

  for(i in 1:20) {  #JUDGE RESIDUAL IS WHITE NOISE OR NOT ON EACH 'lag1～20'
    result <- Box.test(ahat,lag=i,type="Ljung-Box")
    p.value[i] <- result$p.value
  }
  if(sum(p.value>0.05) == 20) {
    success3[m] <- m
  }
}
  
success3 <- subset(success3,success3>0)

#####STEP8: SELECT SMALLEST AIC MODEL#####
candidate <- numeric(1)

for (n in success3) {
  if (AIC[n] < candidate) {
    candidate <- AIC[n]
    success4 <- n
  }
}

(dim[n,])  #BEST MODEL
final_arima <- arima[[n]]
final_arima$coef

#####STEP9: PREDICTION USING MODEL#####

#PREDICT FOR TEN DAYS
nday <- 10
arima.pred <- predict(final_arima,n.ahead=nday)
x <- seq(1,length(stock[,2])+nday,by=1)　   #TIME
trend <- a + b*x　　　　　　　　　　        #TREND SERIES
y <- stock[,2]

write.table(stock,file="actual.csv",sep=",")



yhat <- arima.pred$pred + trend[(length(trend)-nday+1):length(trend)]
sig <- arima.pred$sek
yhatl <- yhat-2*sig                  #95% PREDICTION INTERVAL
yhatu <- yhat+2*sig                  #95% PREDICTION INTERVAL
yhatl2 <- yhat-1*sig                 #70% PREDICTION INTERVAL
yhatu2 <- yhat+1*sig                 #70% PREDICTION INTERVAL
xl <- c(1,length(stock[,2])+nday) 　 #USING 250 DAYS + PREDICTION 10 DAYS
yl <- c(min(y,yhatl),max(y,yhatu))
title <- "NISSAN(7201) CLOSING PRICE"
plot(y,type="l",main=title,xlim=xl,ylim=yl)
lines(yhat,lty=1,col=2)
lines(yhatl,lty=1,col=4)
lines(yhatu,lty=1,col=4)
lines(yhatl2,lty=1,col=3)
lines(yhatu2,lty=1,col=3)
legend("topleft",legend=c("Prediction","95%CI","70%CI"),col=c(2,4,3),lty=c(1))
