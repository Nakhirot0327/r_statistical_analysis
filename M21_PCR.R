####DSL medium class 2 - 1. Principal Component Regression####
#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160807_M_HighQualityModel/Data")

#Data Input
dat <- read.csv("M2_MIC_Population_breakdown_en.csv")
str(dat)

#####1. Package Insall#####
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")}
library(corrplot)
if("discretization" %in% rownames(installed.packages()) == FALSE) {install.packages("discretization")}
library(discretization)
if("DAAG" %in% rownames(installed.packages()) == FALSE) {install.packages("DAAG")}
library(DAAG)

#####2. Data Confirmation#####
####Confirm data class and correct data type
sapply(dat, class)
name <- dat[,"Name"]
dat <- dat[,-(1:2)]
str(dat)

####Confirm Correlation
par(ps = 8, mar = c(12, 4, 4, 2))
corrplot(cor(dat))
par(ps = 10, mar = c(5, 4, 4, 2))

####Separate objective variable and explanatory variables
colnames(dat)
y.loc <- which(colnames(dat) == "Population_increase_pct")

y <- dat[,y.loc] #Objective variable
x <- dat[,-y.loc] #explanatory variables

####Standardizing the data
dat_s <- scale(x)

#####3. Conduct PCA analysis#####
#Principal component analysis(PCA)
PC <- princomp(dat_s)
#Confrim cumulative proportion of PCA
summary(PC) #top 10 PC accounts for 80% of total variance

#Confirm structure of the first PC
par(ps = 8, mar = c(12, 4, 4, 2))
for(i in 1:10) {
  barplot(PC$loadings[,i], #PC loadings
          las = 3, #Direction of labels in x-axis
          main = print(paste("PC loadings of",colnames(PC$loadings)[i])))
}
par(ps = 10, mar = c(5, 4, 4, 2))
str(PC$loadings)

#Calculate first PC values
dat_new <- data.matrix(dat_s) %*% unclass(loadings(PC))
dat_new <- dat_new[,1:10]

#Combine objective variable and explainatory variables
dat_new <- cbind(y,dat_new)
head(dat_new)

#####3. Develop Principal Component Regression (PCR) model#####
###Develop PCR model
dat_new <- as.data.frame(dat_new)
lm.PCR <- lm(y~., dat=dat_new)
summary(lm.PCR) #summary of PCR result

##Apply step function and select optimized features by using AIC
lm.PCR.FS <- step(lm.PCR)
res.PCR <- summary(lm.PCR.FS)
R.sq.PCR <- res.PCR$r.squared #R-squared = 0.72

#####4. Compare results of PCR and normal liner regression#####
###Develop Linear model
lm.nor <- lm(Population_increase_pct~.,dat=dat)
summary(lm.nor)

##Reject multicollinear variables except for "Over65_pct_2010"
reject <- names(which(vif(lm.nor)>10))
reject <- reject[-1]
reject.loc <- which(colnames(dat) %in% reject)
dat_rev <- dat[,-reject.loc]

##Re-develop liner model
lm.nor <- lm(Population_increase_pct~.,dat=dat_rev)
res.nor <- summary(lm.nor)
which(vif(lm.nor)>10) #OK
R.sq.nor <- res.nor$r.squared #R-squared = 0.66

###R-squared comparison
print(paste("R-squared - PCR:",round(R.sq.PCR,2),"Normal:",round(R.sq.nor,2)))