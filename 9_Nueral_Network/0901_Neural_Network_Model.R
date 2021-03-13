#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160807_M_HighQualityModel/Data")

####0. Package Install####
if("nnet" %in% rownames(installed.packages()) == FALSE) {install.packages("nnet")}
library(nnet)

####1. Data Preparation####
#Read csv data
dat <- read.csv("2_Bank_Customer.csv",header=TRUE,row.names=1)

#Confirm column names - please see the meaning of columns in the material
colnames(dat)

#Confirm data type
sapply(dat,class)
#"AGE","CRED","INCOME","MILEAGE","DELINQ","NUMTR" are numeric/integer variables
#"resp" should be "factor"
dat[,"resp"] <- as.factor(dat[,"resp"])
sapply(dat,class)

#Reject NA rows - how to handle NAs is to be lectured in future class
dat <- na.omit(dat)
dim(dat) #29,871 rows x 13 columns

##Confirm Histgram for Quantitative Variables
sapply(dat,class)

#Objective variable
ggplot(dat,aes(x = CRED)) + geom_histogram(position = "identity")

#Explainatory variables
ggplot(dat,aes(x = AGE)) + geom_histogram(position = "identity")
ggplot(dat,aes(x = INCOME)) + geom_histogram(position = "identity")
ggplot(dat,aes(x = MILEAGE)) + geom_histogram(position = "identity")
ggplot(dat,aes(x = DELINQ)) + geom_histogram(position = "identity")
ggplot(dat,aes(x = NUMTR)) + geom_histogram(position = "identity")

##Reject strange data in "CRED" and "INCOME" columns
#"CRED"
dat[,"CRED"][order(dat[,"CRED"],decreasing = TRUE)][1:500]
reject <- which(dat[,"CRED"] > 1000)
dat <- dat[-reject,]

#"INCOME"
dat[,"INCOME"][order(dat[,"INCOME"],decreasing = TRUE)][1:500]
reject <- which(dat[,"INCOME"] > 70)
dat <- dat[-reject,]

dat[,"MILEAGE"][order(dat[,"MILEAGE"],decreasing = TRUE)][1:500]
#There is no strange outliner
dim(dat) #29,370 rows x 13 columns

####2. Regression by General Linear Model####
##Sepearate train data and test data
set.seed(10)
trainNO <- sample(x = nrow(dat),size = nrow(dat)*0.7,replace = TRUE)
train.dat <- dat[trainNO,]
test.dat <- dat[-trainNO,]

##Create neuralNet model
m.CRED <- max(train.dat[,"CRED"])
nn <- nnet(CRED/m.CRED~.,data = train.dat, size = 40)
str(nn)
summary(nn) #Coefficients
nn$coefnames

##Calculate fitted value
nn.p <- predict(nn)*m.CRED

####3. Evaluation####
##R-squared
(R2.nn <- 1 - sum((train.dat[,"CRED"] - nn.p)^2) / sum((train.dat[,"CRED"] - mean(train.dat[,"CRED"]))^2))

##R-squared Comparison
#Please execute after finishig "M23_GEneralLinearModel"
xlab <- c("Linear\nReg.","Poisson", "Negative\nBinominal","Gamma","Neural\nNet")
barplot(c(R_sq,R_sq.pois,R_sq.nb,R_sq.gamma,R2.nn),main = "R-squared",names.arg = xlab, las = 3)

##Prediction vis actual values
plot(y = train.dat[,"CRED"], x = nn.p,
     main = "Neural Network predictions vs actual",
     ylab = "Actual value",xlab = "Predictive value", pch = 19, col = "#0000ff20")
abline(0,1)
