##############3. CONFIRM NEGATIVE EFFECT OF UNIT ROOT PROCESS##############
#CREATE PARAMETERS
n <- 100      #SAMPLE NUMBER
d <- 0　　　　#DEGREE OF ARIMA MODEL
sig2 <- 1.2   #VARIANCE
mu <- 0　　　 #MEAN
beta <- 0.07　#1ST COEFFICIENT OF TREND
phi <- c(0.5,0.3)　　　#COEFFICIENT OF AR
p <- length(phi)       #DEGREE OF AR
theta <-  c(0.3,0.2)   #COEFFICIENT OF MA
q <- length(theta)     #DEGREE OF MA
time <- 1:n            #TIME
trend <- mu+beta*time  #LINEAR EXPRESSION OF TREND

#CREATE SERIES OF LINEAR EXPRESSION OF TREND AND ARIMA(p,d,q) AS 'y1'
pdq <- list(order=c(p,d,q),ar=phi,ma=theta)
wt <- arima.sim(n,model=pdq,sd=sqrt(sig2))
y1 <- trend + wt

#CREATE UNIT ROOT PROCESS AS 'y2'
y2 <- cumsum(beta+rnorm(n,sd=sqrt(sig2))) #'cumsum' IS FUNCTION TO CALCULATE CUMULATIVE SUM

#PLOT GRAPH
yl <- c(min(y1,y2),max(y1,y2))
ttl <- "ARMA WITH TREND AND UNIT ROOT PROCESS"
plot(y1,type="l",main=ttl,ylim=yl,col=3)
lines(y2,col=2)
hanrei <- c("LINEAR EXPRESSION TREND + ARMA","UNIT ROOT PROCESS")
xx <- 0
yy <- max(y1,y2)
legend(xx,yy,legend=hanrei,lty=c(1,1),box.lty=0,col=c(3,2))
