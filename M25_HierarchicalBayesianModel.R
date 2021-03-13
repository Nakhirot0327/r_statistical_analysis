####DSL medium class 2 - 4. HierarchicalBaysianModel (HBR)####
#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160807_M_HighQualityModel/Data")

####0. Package Install####
if("dummies" %in% rownames(installed.packages()) == FALSE) {install.packages("dummies")}
library(dummies)
if("fmsb" %in% rownames(installed.packages()) == FALSE) {install.packages("fmsb")}
library(fmsb)
if("LearnBayes" %in% rownames(installed.packages()) == FALSE) {install.packages("LearnBayes")}
library(LearnBayes)
if("bayesm" %in% rownames(installed.packages()) == FALSE) {install.packages("bayesm")}
library(bayesm)
if("lattice" %in% rownames(installed.packages()) == FALSE) {install.packages("lattice")}
library(lattice)
if("mlmRev" %in% rownames(installed.packages()) == FALSE) {install.packages("mlmRev")}
library(mlmRev)
if("DAAG" %in% rownames(installed.packages()) == FALSE) {install.packages("DAAG")}
library(DAAG)

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
dim(dat) #29,871 rows x 24 columns

#Confirm Histgram for Quantitative Variables
#sapply(dat,class)
#select <- c("AGE","CRED","INCOME","MILEAGE","DELINQ","NUMTR")
#ggplot(dat,aes(x = AGE)) + geom_histogram(alpha = 0.5, position = "identity")
#ggplot(dat,aes(x = CRED)) + geom_histogram(alpha = 0.5, position = "identity")
#ggplot(dat,aes(x = INCOME)) + geom_histogram(alpha = 0.5, position = "identity")
#ggplot(dat,aes(x = MILEAGE)) + geom_histogram(alpha = 0.5, position = "identity")
#ggplot(dat,aes(x = DELINQ)) + geom_histogram(alpha = 0.5, position = "identity")
#ggplot(dat,aes(x = NUMTR)) + geom_histogram(alpha = 0.5, position = "identity")

#Reject strange data in "CRED" and "INCOME" columns
dat[,"CRED"][order(dat[,"CRED"],decreasing = TRUE)][1:500]
reject <- which(dat[,"CRED"] > 1000)
dat <- dat[-reject,]

dat[,"INCOME"][order(dat[,"INCOME"],decreasing = TRUE)][1:500]
reject <- which(dat[,"INCOME"] > 70)
dat <- dat[-reject,]

dat[,"MILEAGE"][order(dat[,"MILEAGE"],decreasing = TRUE)][1:500]
#There is no strange outliner
dim(dat) #29,370 rows x 13 columns

#Change qualitative data to quantitive data (dummy variables)
q <- c("MS","DEPC","MOB","RESTYPE","GENDER","EMP_STA","resp")
dat_q <- dat[,q]
dat_q <- dummy.data.frame(dat_q) #Make dummy variables
sapply(dat_q,class) #Confirm data type

#Combine dummy variables and origial quantitative variables
reject <- which(colnames(dat) %in% q)
dat <- dat[,-reject]
dat <- cbind(dat,dat_q)

#Confirm data type - all variables are quantitative
sapply(dat,class)
dim(dat) #29,370 rows x 24 columns
head(dat)
rm(dat_q)

####3. Hierarchical Baysian Model####
set.seed(10)
trainNO <- sample(x = nrow(dat),size = nrow(dat)*0.7,replace = TRUE)
train.dat <- dat[trainNO,]
test.dat <- dat[-trainNO,]

###Reject redundant variables for HBM
head(train.dat)
reject <- c("MSX","DEPCN","MOBN","RESTYPECONDO","GENDERM","EMP_STA0","resp0")
reject <- which(colnames(train.dat) %in% reject)
train.dat <- train.dat[,-reject]
head(train.dat)

###HierarchicalBaysianModel(HBM)
##Develop input data for HBM
ex.v <- c(1,3:(ncol(train.dat)))
regdata <- NULL

#Need 1 min
for(i in 1:nrow(train.dat)){
  X <- as.matrix(cbind(1,train.dat[i,ex.v]))
  y <- train.dat$CRED[i]
  regdata[[i]] <- list(X=X,y=y)
  print(paste("Developing iput data..., i =",i,"/",nrow(train.dat),"is done"))
}
Data <- list(regdata=regdata)

##Setting parameters for MCMC
R <- 500 #Number of iterations - Ideally 10,000 but in this time 500 due to computer resource
Mcmc <- list(R=R)
gc();gc() #Release redundant memroy capacity
memory.size()

##Estimate parameters by MCMC
out <- rhierLinearModel(Data=Data,Mcmc=Mcmc) #need 5 mins

#Confirm beta distribution
gc();gc() #Release redundant memroy capacity
memory.size()
plot(out$betadraw)
#plot(out$Vbetadraw)

##Calculate mean of beta for each variable - need 3 mins
beta <- data.frame()
for (i in 1:nrow(train.dat)){
  tmp <- rowMeans(out$betadraw[i,,seq(100,500)]) #Burn-in and total num of iteration
  beta <- rbind(beta,tmp)
  print(paste("Extract mean of each coefficients... i =",i,"/",nrow(train.dat),"is done"))
}
colnames(beta) <- c("I",colnames(train.dat)[ex.v]) #"I" means "interval"

#Mean of beta for each variable
apply(beta,2,mean)

##Apply the model to data - need 2 mins
pred.blm <- c()
for(i in 1:nrow(train.dat)){
  pred.blm <- c(pred.blm,sum(beta[i,] * cbind(1,train.dat[i,ex.v])))
  print(paste("Calculating prediction... i =",i,"/",nrow(train.dat),"is done"))
}

##Comparison between prediction and actual value
xt <- c(min(train.dat[,"CRED"]),max(train.dat[,"CRED"])) #x interval
yt <- c(min(pred.blm),max(pred.blm)) #y interval

plot(train.dat[,"CRED"],pred.blm,xlim=xt,ylim=yt,
     xlab="Actual",ylab="Predictive",pch = 19, col = "#0000ff20",
     main="Comparison of Actual and Predictive values")

##R-squared comparison
R2.blm <- 1 - sum((train.dat[,"CRED"] - pred.blm)^2) / sum((train.dat[,"CRED"] - mean(train.dat[,"CRED"]))^2)
print(paste("R-squared - HBM:",round(R2.blm,3)))

####4. Result analysis of HBM####
###(Re)Read csv data and Reconfirm the data
dat <- read.csv("2_Bank_Customer.csv",header=TRUE,row.names=1)
colnames(dat)
dat[,"resp"] <- as.factor(dat[,"resp"])
dat <- na.omit(dat)

dat[,"CRED"][order(dat[,"CRED"],decreasing = TRUE)][1:500]
reject <- which(dat[,"CRED"] > 1000)
dat <- dat[-reject,]

dat[,"INCOME"][order(dat[,"INCOME"],decreasing = TRUE)][1:500]
reject <- which(dat[,"INCOME"] > 70)
dat <- dat[-reject,]

train.dat <- dat[trainNO,]
dim(train.dat) #20,559 rows x 13 columns

###Define analysis scope
str(train.dat)
colnames(beta) <- paste0(colnames(beta),"_beta")
ana.dat <- cbind(train.dat, beta)
colnames(ana.dat)

##Analyze coefficients distribution by each variable
plot(ana.dat$AGE, ana.dat$AGE_beta, xlab="AGE",ylab="AGE coefficients in HBM",
     pch = 19, col = "#0000ff10") #AGE
histogram(~ AGE_beta | INCOME, data = ana.dat) #INCOME
  histogram(~ MSM_beta | MS, data = ana.dat) #MS
histogram(~ INCOME_beta | INCOME, data = ana.dat) #INCOME
histogram(~ DEPCY_beta | DEPC, data = ana.dat) #DEPC
histogram(~ MOBY_beta | MOB, data = ana.dat) #MOB
plot(ana.dat$MILEAGE, ana.dat$MILEAGE_beta, xlab="MILEAGE",ylab="MILEAGE coefficients in HBM",
     pch = 19, col = "#0000ff20") #MILEAGE
histogram(~ GENDERF_beta | GENDER, data = ana.dat) #GENDER
plot(ana.dat$EMP_STA, ana.dat[,28], xlab="EMP_STA",ylab="Employee Status(1 or 2) coefficients in HBM",
     pch = 19, col = "#0000ff20") #EMP_STA1 or 2
plot(ana.dat$EMP_STA, ana.dat[,29], xlab="EMP_STA",ylab="Employee Status(3+) coefficients in HBM",
     pch = 19, col = "#0000ff20") #EMP_STA3+
plot(ana.dat$DELINQ, ana.dat$DELINQ_beta, xlab="DELINQ",ylab="DELINQ coefficients in HBM",
     pch = 19, col = "#0000ff20") #DELINQ
plot(ana.dat$NUMTR, ana.dat$NUMTR_beta, xlab="NUMTR",ylab="NUMTR coefficients in HBM",
     pch = 19, col = "#0000ff20") #NUMTR
histogram(~ resp1_beta | resp, data = ana.dat) #resp