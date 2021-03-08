#############2. AR MODEL AND MA MODEL TEST#############
#CREATE A FUNCTION TO MAKE AR(1) MODEL SERIES 
AR1 <- function(n,b,a=0,sd=1,y0=0) {
  c <- rnorm(n,sd=sd) #CREATE RANDOM NUMBER WITH NORMAL DISTRIBUTION (SAMPLE NUMBER:n, STANDARD ERROR:sd)
  y <- rep(0,n)　　　 #CREATE VECTOR TO SET SAMPLE VALUES
  y[1] <- y0　　　　　#PLUG IN INITIAL VALUE(DEFAULT VALUE=0)
  
  for(j in 1:(n-1)) {
    y[j+1] <- a + b*y[j] + c[j+1]  #CREATE AR(1) MODEL SERIES 
  }
  
  m <- round(mean(y),digits=2)   #SAMPLE MEAN OF SERIES
  e_y <- round(m/(1-b),digits=2) #POPULATION MEAN OF SERIES
  
  title <- paste("AR Model y_t = a + b*y_t-1 + c_t, a=",a,
                 ",b =",b,",sd =",sd,sep="")
  yl <- c(min(y),max(y))
  plot(y,type="l",main=title)
  abline(h=e_y,col=2) 
  Sys.sleep(1.5)
  #y_ts <- ts(y,start=1,freq=1)
  #acf(y_ts,main="Autocorrelation coefficient")
}

#PLOT GRAPHS OF VARIABLE COEFFICIANTS OF AR(1) MODEL (FIXED NOISE DISTRIBUTION AND MEAN OF SERIES)
b_seq <- seq(0,1.5,by=0.1) #'b' IS AR(1) COEFFICIENT OF y_t-1
for (i in b_seq) {
  AR1(n=200,b=i,a=0)
}

#CREATE A FUNCTION TO MAKE AR(2) MODEL SERIES
AR2 <- function(n,b1,b2,a=0,sd=1,y0=0) {
  c <- rnorm(n,sd=sd)
  y <- rep(0,n)
  y[1] <- y0
  
  for(j in 1:(n-2)) {
    y[j+2] <- a + b1*y[j+1] + b2*y[j] + c[j+2] 
  }
  
  m <- round(mean(y),digits=2)
  e_y <- round(m/(1-b1-b2),digits=2)
  
  title <- paste("AR Model y_t=a + b1*y_t-1 + b2*y_t-2 + c_t, 
                 a=",a,",b1=",b1,",b2=",b2,",sd=",sd,sep="")
  yl <- c(min(y),max(y))
  plot(y,type="l",main=title)
  abline(h=e_y,col=2)
  
  #y_ts <- ts(y,start=1,freq=1)
  #acf(y_ts,main="Autocorrelation coefficient")
  Sys.sleep(1)
}

#PLOT GRAPHS OF VARIABLE COEFFICIANTS OF AR(2) MODEL (FIXED NOISE DISTRIBUTION AND MEAN OF SERIES)
b1_seq <- seq(0,1.4,by=0.2) #'b1' IS AR(2) COEFFICIENT OF y_t-1
b2_seq <- seq(0,1.4,by=0.1) #'b2' IS AR(2) COEFFICIENT OF y_t-2

for (i in b1_seq) {
  for (j in b2_seq)
    AR2(n=200,b1=i,b2=j)
}

#CREATE A FUNCTION TO MAKE MA1 MODEL SERIES
MA1 <- function(n,si,a=0,sd=1,y0=0) {
  c <- rnorm(n,sd=sd)
  y <- rep(0,n)
  y[1] <- y0
  
  for(j in 1:(n-1)) {
    y[j+1] <- a + c[j+1] + si*c[j]
  }
  
  m <- round(mean(y),digits=2)
  e_y <- round(m,digits=2)
  
  title <- paste("MA Model y_t = a + c_t + c_t-1,a=",a,
                 ",θ =",si,",sd =",sd,sep="")
  yl <- c(min(y),max(y))
  plot(y,type="l",main=title)
  abline(h=e_y,col=2)
  Sys.sleep(1.5)
  
  #y_ts <- ts(y,start=1,freq=1)
  #acf(y_ts,main="Autocorrelation coefficient")
}

#PLOT GRAPHS OF VARIABLE COEFFICIANTS OF MA1 MODEL
si_seq <- seq(0,1.5,by=0.1) #'si' IS MA(1) COEFFICIENT OF c_t-1(NOISE OF BEFORE TIME)

for (i in si_seq){
  MA1(n=200,si=i)
}