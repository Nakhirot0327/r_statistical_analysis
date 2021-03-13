## PRACTICE1 Sakura Forecast ###

## 1_0 PREPARING ##
#INSTALL PACKAGE

#SET CURRENT DIRECTORY
setwd("/work/R/140823")
getwd()

## 1_1 READ AND CHECK DATA ## 
dat <- read.csv("sakura_temperature.csv", header=TRUE)
head(dat)
#DATA DESCRIPTION (unit or category example is shown in paratheses)
#year:              YEAR
#temp_L4~temp_L12:  MONTHLY AVERAGE TEMPERATURE IN "LAST" DECEMBER
#temp_1~temp_3:     MONTHLY AVERAGE TEMPERATURE IN THIS MONTH
#open_day:          DAYS FROM MARCH 1ST TO SAKURA BLOOM DAY

dat <- dat[,-1] #DELETE YEAR COLUMN
dim(dat)  #CONFIRM MATRIX SIZE
#CONFIRM DATA TYPE
for (i in 1:ncol(dat)) {
    print(c(names(dat[i]), class(dat[,i])))
}

#MONTHLY TEMPERATURE HISTGRAM 
par(mfrow=c(2,3), mar=c(5,3,3,3), ps=15)　# LAYOUT
for (i in 1:(ncol(dat)-1)) {
    hist(dat[,i], xlab="temperature(degree)", main=names(dat[i]))
}

#DAYS TO OPEN HISTGRAM 
par(mfrow=c(1,1), mar=c(5,3,3,3))
hist(dat[,ncol(dat)], xlab="days to open from March 1st", main=names(dat[ncol(dat)]))

#CHECK CORRELATION BETWEEN TEMPERATURE AND DAYS TO OPEN
par(mfrow=c(3,4), mar=c(5,3,3,3))
plot(open_day~., data=dat)
cor(dat)
#dat <- dat[,c(-1:-8)]   #DELETE tempL4 ~ tempL11
head(dat)

## 1_2 LINEAR REGRESSION ##
lm_a <- lm(open_day~., data=dat)
summary(lm_a)

#(a)VARIABLE SELECTION
lm_b <- step(lm_a) #VARIABLE SELECTION WITH AIC (DEFAULT VALUE:backward)
summary(lm_b) #CHECK R-square

lm_b$coefficients

#(b)EVALUATE MODEL
#(b-1):RESIDUAL ANALYSIS
par(mfrow=c(2,2), mar=c(4,4,4,4), ps=15)
plot(lm_b)  #RESIDUAL ANALYSIS 
#NUMBERS IN CANVAS ARE LINE NUMBERS OF OUTLINER
par(mfrow=c(1,1), mar=c(3,3,3,3)) 
#RESIDUAL ANALYSIS:
#Residuals vs Fitted values:SPC: 
#           DISPLAY RESIDUAL VALUES WITH OBJECTIVE VARIABLES ALONG Y-AXIS 
#NormalQ-Q: X-Y PLOT AFTER STANDARIZATION
#           X-AXIS: EXPECTATION VALUES ASSUMED THAT THEY ARE DISTRIBUTED 
#                   WITH NORMAL DISTRIBUTION
#           Y-AXIS: RESIDUAL VALUES
#Scale-Location: 
#           Residual vs Fitted values (TAKE THE SQUARE ROOT ALONG Y-AXIS)
#Residuals vs Leverage:
#           Leverage IS PREDICTIVE VARIABLE RATE OF EACH SAMPLE 
#           IN REGRESSION ANALYSIS WHEN OBJECTIVE VARIABLE(Y) INCRESES ONLY "1" 
#           AND EXPLANARY VALUE(X) IS FIXED 
#cook's distance: DISTANCE FOR TEST 

#REGRESSION ANALYSIS AGAIN WITHOUT OUTLINERS
#delete <- c(17,16,20,38) #OUTLINERS CHECKED IN CANVAS
delete <- c(17,20,49) #OUTLINERS CHECKED IN CANVAS
dat_rv <- dat[-delete,]
lm_c <- step(lm(open_day~., data=dat_rv))
sl <- summary(lm_c) #CHECK R-square
lm_c$coefficients
(sl_c <- sl$coefficients)   #RESULT T-TEST ON EACH FACTOR

#CONFIRM INTERCEPT AND COEFFICIENTS 95% CONFIDENCE INTERVAL
max_v <- as.vector(0)   #DEFINE MAXIMUM 95% CONFIDENCE INTERVAL
min_v <- as.vector(0)   #DEFINE MINIMUM 95% CONFIDENCE INTERVAL
for (i in 1:nrow(sl_c)) {
    max_v[i] <- sl_c[i,1] + sl_c[i,2] * 1.96
    min_v[i] <- sl_c[i,1] - sl_c[i,2] * 1.96
}
data.frame(rownames(sl_c), max_v, min_v)


#SET OBJECTIVE AND EXPLANARY VARIABLE
exps <- names(lm_c$coefficients)
exps <- exps[-1]   #DELETE 1ST COLUMN (Intercept)
exps
obj="open_day"

#CALCULATE AVERAGE MONTHLY TEMPERATURE
m_ave_temp <- as.vector("")
for (i in 1:length(exps)) {
    m_ave_temp[i] <- (sum(dat_rv[exps[i]])) / nrow(dat_rv[exps[i]])
}

#CALCULATE INTERCEPT(a) AND COEFFICIENT(b)
a <- as.vector(0)  #Intercept
b <- as.vector(0)  #coefficient

#EXTRACT COEFFICIENT VALUES ON EACH FACTOR
lm_c_c <- lm_c$coefficients[-1]

#CALCULATE INTERCEPT AND COEFFICIENT ON EACH FACTOR
for (i in 1:length(exps)) {
    a[i] <- as.numeric(lm_c$coefficients[1])
    b[i] <- as.numeric(lm_c_c[i])
    for (j in 1:length(exps)) {
        if (j != i) {a[i] <- a[i] + as.numeric(lm_c_c[j]) * as.numeric(m_ave_temp[j])}
    }
}

#PLOT OBJECTIVE AND EXPLANATORY VARIABLE
par(mfrow=c(1,1), mar=c(6,6,5,5))

for (k in 1:length(exps)) {
    plot(dat_rv[,obj]~dat_rv[,exps[k]], xlab=exps[k], ylab=obj, 
         main="linear regression and abline",
         xlim=c(min(dat_rv[,exps[k]]), max(dat_rv[,exps[k]])), 
         ylim=c(min(dat_rv[,obj]), max(dat_rv[,obj])),
         col=NULL, bg=rgb(0,1,1, alpha=0.4), pch=21)
    abline(a=a[k], b=b[k], col="green")
    par(new=T)
    
    
    #(b-2):95% CONFIDENCE INTERVAL
    lm_con <- predict(lm_c, interval="confidence", level=0.95)
    
    #SORT DATA TO DISPLAY
    con_lwr <- data.frame(dat_rv[,exps[k]], lm_con[,"lwr"])
    con_lwr <- con_lwr[order(con_lwr[,1]),]
    con_upr <- data.frame(dat_rv[,exps[k]], lm_con[,"upr"])
    con_upr <- con_upr[order(con_upr[,1]),]
    
    #ADD 95% CONFIDENCE INTERVAL LINES
    par(ps=20) #FONT SIZE
    lines(con_lwr,type="l",
          xlim=c(min(dat[,exps[k]]), max(dat[,exps[k]])), 
          ylim=c(min(dat[,obj]), max(dat[,obj])),
          ylab="",xlab="",col="blue",lty=1)
    par(new=T)
    lines(con_upr,type="l",
          xlim=c(min(dat[,exps[k]]), max(dat[,exps[k]])), 
          ylim=c(min(dat[,obj]), max(dat[,obj])),
          ylab="",xlab="",col="blue",lty=1)
    par(new=T)
    
    #(b-3):95% PREDICTION INTERVAL
    lm_prd <- predict(lm_c, interval="prediction", level=0.95)
    
    prd_lwr <- data.frame(dat_rv[,exps[k]], lm_prd[,"lwr"])
    prd_lwr <- prd_lwr[order(prd_lwr[,1]),]
    prd_upr <- data.frame(dat_rv[,exps[k]], lm_prd[,"upr"])
    prd_upr <- prd_upr[order(prd_upr[,1]),]
    
    #ADD 95% PREDICTION INTERVAL LINES
    lines(prd_lwr,type="l",
          xlim=c(min(dat[,exps[k]]), max(dat[,exps[k]])), 
          ylim=c(min(dat[,obj]), max(dat[,obj])),
          ylab="",xlab="",col="red",lty=1)
    par(new=T)
    lines(prd_upr,type="l",
          xlim=c(min(dat[,exps[k]]), max(dat[,exps[k]])), 
          ylim=c(min(dat[,obj]), max(dat[,obj])),
          ylab="",xlab="",col="red",lty=1)
    par(new=T)
    
    par(ps=15) #FONT SIZE
    #ADD LEGEND
    legend("topleft",legend=c("95% CONFIDENCE INTERVAL","95% PREDICTION INTERVAL"),
           col=c("blue","red"),lty=c(1,1),bty="n") #bty="n": NO BOX
}
