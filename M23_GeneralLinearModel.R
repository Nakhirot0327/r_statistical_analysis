####DSL medium class 2 - 3. General Liner Model (GLM)####
#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160807_M_HighQualityModel/Data")

####0. Package Install####
if("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
library(MASS)
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(ggplot2)

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
fom <- "CRED~." #Formula

###(a) Liner regression
res <- step(lm(fom,data = dat))
(res.sum <- summary(res))
R_sq <- res.sum$r.squared
AIC <- res$anova$AIC[length(res$anova$AIC)]

###(b) Poisson distribution x log function
res.pois <- step(glm(fom, data = dat, family = poisson(link = log)))
(res.pois.sum <- summary(res.pois))
pois.p.y <- res.pois$fitted.values
R_sq.pois <- 1 - sum((dat[,"CRED"] - pois.p.y)^2) / sum((dat[,"CRED"] - mean(dat[,"CRED"]))^2)
AIC.pois <- res.pois$aic

###(c) Negative binomial distribution x log function
res.nb <- step(glm.nb(fom,data = dat))
(res.nb.sum <- summary(res.nb))
nb.p.y <- res.nb$fitted.values
R_sq.nb <- 1 - sum((dat[,"CRED"] - nb.p.y)^2) / sum((dat[,"CRED"] - mean(dat[,"CRED"]))^2)
AIC.nb <- res.nb.sum$aic

###(d) Gamma distribution x log function
res.gamma <- step(glm(fom, data = dat, family = Gamma(link = log)))
(res.gamma.sum <- summary(res.gamma))
gamma.p.y <- res.gamma$fitted.values
R_sq.gamma <- 1 - sum((dat[,"CRED"] - gamma.p.y)^2) / sum((dat[,"CRED"] - mean(dat[,"CRED"]))^2)
AIC.gamma <- res.gamma.sum$aic

####3. Comparison of R-squared and AIC####
xlab <- c("Linear\nReg.","Poisson", "Negative\nBinominal","Gamma")

par(mfrow=c(2,1))
barplot(c(R_sq,R_sq.pois,R_sq.nb,R_sq.gamma),main = "R-squared",names.arg = "")
barplot(c(AIC,AIC.pois,AIC.nb,AIC.pois),main = "AIC",names.arg = xlab, las = 3)
par(mfrow=c(1,1))

####4. Visualization of the result####
head(dat)

##(a) Linear Regression
plot(x = res$fitted.values, y = dat[,"CRED"], 
     pch = 19, col = "#0000ff10", main = "Linear Regression",
     xlab = "Prediction", ylab = "Actual value")
abline(0,1)

##(b) Poisson Regression
#Linear predictors and fitted values
x <- res.pois$linear.predictors
y <- function(x) {exp(x)}

xt <- c(min(res.pois$linear.predictors),max(res.pois$linear.predictors))
yt <- c(min(dat[,"CRED"]),max(dat[,"CRED"]))

#Comparion of actual values with fitted values
plot(x = x, y = dat[,"CRED"],
     pch = 19, col = "#0000ff10",
     xlim = xt, ylim = yt, main = "Poisson Regression",
     xlab = "Linear Predictors", ylab = "Actual value")
par(new=TRUE)
plot(y, type = "l", xlim = xt, ylim = yt,
     xlab = "", ylab = "")

##(c) Negative binomial regression
#Linear predictors and fitted values
x <- res.nb$linear.predictors
y <- res.nb$fitted.values

xt <- c(min(res.nb$linear.predictors),max(res.nb$linear.predictors))
yt <- c(min(dat[,"CRED"]),max(dat[,"CRED"]))

#Comparion of actual values with fitted values
plot(x = x, y = dat[,"CRED"],
     pch = 19, col = "#0000ff10",
     xlim = xt, ylim = yt, main = "Negative Binominal Regression",
     xlab = "Linear Predictors", ylab = "Actual value")
par(new=TRUE)
plot(x, y, type = "l", xlim = xt, ylim = yt,
     xlab = "", ylab = "")

##(d) Gamma regression
#Linear predictors and fitted values
x <- res.gamma$linear.predictors
y <- res.gamma$fitted.values

xt <- c(min(res.gamma$linear.predictors),max(res.gamma$linear.predictors))
yt <- c(min(dat[,"CRED"]),max(dat[,"CRED"]))

#Comparion of actual values with fitted values
plot(x = x, y = dat[,"CRED"],
     pch = 19, col = "#0000ff10",
     xlim = xt, ylim = yt, main = "Gamma Regression",
     xlab = "Linear Predictors", ylab = "Actual value")
par(new=TRUE)
plot(x, y, type = "l", xlim = xt, ylim = yt,
     xlab = "", ylab = "")